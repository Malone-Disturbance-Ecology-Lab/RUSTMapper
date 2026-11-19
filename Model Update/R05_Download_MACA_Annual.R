# =============================================================================
# RUSTMapper RE-PROJECTION :: R05_Download_MACA_Annual.R   (future climate)
# -----------------------------------------------------------------------------
# Same five GCMs as NewData/FLOW.ClimateChangeDownload.R (MACAv2-METDATA via
# climateR::getMACA), but each GCM is kept separate and every layer is built
# with the TRAINING definitions, exactly as R01 does for gridMET:
#
#   PRCP = seasonal total | Tmin = seasonal MIN of daily tasmin | Tmax = seasonal MAX
#   VP / VPmax = mean / max of daily actual vapour pressure (Pa) | RH = mean ea/es(Tmean)*100
#   Tmin_Winter = Jan-Feb minimum (hardiness zone)
#
# What the old future layers did instead (FLOW.ClimateChangeDownload.R):
#   Tmin/Tmax = seasonal MEAN (training uses min/max); spring/summer Tmax from
#   BNU-ESM only; PRCP summed over GCMs (then /5); VPD = deficit, summer from
#   Apr-May; RH = mean(rhsmin, rhsmax) without CCSM4; H_Zone from summer Tmin;
#   scenario left at climateR's default (rcp45) except spring PRCP ("rcp8").
#
# Humidity: ea = [es(Tmin) RHmax + es(Tmax) RHmin] / 200 (FAO-56 eq. 17), as for
# gridMET. If a GCM has no rhsmin/rhsmax in MACA (the old script dropped CCSM4 for RH), ea
# comes from daily specific humidity: ea = q P / (0.622 + 0.378 q), with surface
# pressure P from DEM_2021 (standard atmosphere). The method used per GCM is
# logged to QA/R05_humidity_method.csv.
#
# Years: 2011-2099 so every 2030+ year has a complete 20-year window.
# Resumable; OVERWRITE <- TRUE forces a redo.
# =============================================================================

rm(list = ls())
# Works from the repo (RUSTMapper.Rproj open -> "Model Update/") or from the data drive copy
source(if (file.exists("Model Update/R00_Config.R")) "Model Update/R00_Config.R" else
       "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET/R00_Config.R")
library(climateR)

OVERWRITE <- FALSE

get.maca <- function(varname, gcm, scenario, start, end, tries = 4) {
  for (k in seq_len(tries)) {
    out <- tryCatch(climateR::getMACA(AOI, varname = varname, model = gcm, scenario = scenario,
                                      timeRes = "day", startDate = start, endDate = end),
                    error = function(e) e)
    if (!inherits(out, "error")) return(if (inherits(out, "SpatRaster")) out else out[[1]])
    message("  getMACA(", varname, " ", gcm, " ", start, ") failed [try ", k, "]: ", conditionMessage(out))
    Sys.sleep(15 * k)
  }
  stop("getMACA failed: ", varname, " ", gcm, " ", start)
}

# Surface pressure (Pa) on the MACA grid, from the DEM; cached
pressure.pa <- function(like) {
  f <- file.path(maca.dir, "surface_pressure_Pa.tif")
  if (file.exists(f)) { p <- rast(f); if (compareGeom(p, like[[1]], stopOnError = FALSE)) return(p) }
  z <- rast(dem.file) %>% terra::project(like[[1]], method = "average")
  p <- 101325 * (1 - 2.25577e-5 * z)^5.25588
  writeRaster(p, f, overwrite = TRUE); p
}

# Which humidity variables does this GCM have? Checked once per GCM with a
# dry run (catalog lookup only, no download).
has.var <- function(varname, gcm, scenario) {
  r <- tryCatch(climateR::getMACA(AOI, varname = varname, model = gcm, scenario = scenario,
                                  timeRes = "day", startDate = "2050-06-01", endDate = "2050-06-02",
                                  dryrun = TRUE), error = function(e) NULL)
  !is.null(r) && (!is.data.frame(r) || nrow(r) > 0)
}
humidity.method <- list()

# Daily actual vapour pressure (Pa)
daily.ea <- function(method, gcm, scenario, s, tmin, tmax) {
  if (method == "rh") {
    rmin <- get.maca("rhsmin", gcm, scenario, s[1], s[2])
    rmax <- get.maca("rhsmax", gcm, scenario, s[1], s[2])
    return((es.pa(tmin) * rmax / 100 + es.pa(tmax) * rmin / 100) / 2)
  }
  q <- get.maca("huss", gcm, scenario, s[1], s[2])
  q * pressure.pa(q) / (0.622 + 0.378 * q)
}

save.layer <- function(r, var, season, year, src) {
  r <- r[[1]]; names(r) <- paste(var, season, sep = "_"); time(r) <- year
  writeRaster(r, annual.file(var, season, year, src), overwrite = TRUE)
}

for (scenario in SCENARIOS) for (gcm in GCMS) {
  src <- maca.src(scenario, gcm)
  dir.create(file.path(maca.dir, src), showWarnings = FALSE)
  method <- if (has.var("rhsmin", gcm, scenario) && has.var("rhsmax", gcm, scenario)) "rh" else
            if (has.var("huss", gcm, scenario)) "huss" else
            stop(src, ": no rhsmin/rhsmax or huss in MACA - cannot build vapour pressure/RH")
  humidity.method[[src]] <- method
  message("######## ", src, " (humidity from ", method, ") ########")

  for (year in MACA_DOWNLOAD_YEARS) {
    d <- season.dates(year)
    for (season in SEASONS) {
      if (!OVERWRITE && all(file.exists(annual.file(ANNUAL.VARS$var, season, year, src)))) next
      s <- d[[season]]
      message(src, " ", year, " ", season)
      pr   <- get.maca("pr",     gcm, scenario, s[1], s[2])
      tmin <- get.maca("tasmin", gcm, scenario, s[1], s[2]) - 273.15
      tmax <- get.maca("tasmax", gcm, scenario, s[1], s[2]) - 273.15
      ea   <- daily.ea(method, gcm, scenario, s, tmin, tmax)
      if (length(unique(c(nlyr(pr), nlyr(tmin), nlyr(tmax), nlyr(ea)))) != 1)
        stop("Daily layer counts differ: ", src, " ", season, " ", year)
      rh <- ea / es.pa(0.606 * tmax + 0.394 * tmin) * 100

      save.layer(sum(pr, na.rm = TRUE),   "PRCP",  season, year, src)
      save.layer(min(tmin, na.rm = TRUE), "Tmin",  season, year, src)
      save.layer(max(tmax, na.rm = TRUE), "Tmax",  season, year, src)
      save.layer(mean(ea, na.rm = TRUE),  "VP",    season, year, src)
      save.layer(max(ea, na.rm = TRUE),   "VPmax", season, year, src)
      save.layer(mean(rh, na.rm = TRUE),  "RH",    season, year, src)
      rm(pr, tmin, tmax, ea, rh); gc()
    }
    if (OVERWRITE || !file.exists(annual.file("Tmin", "Winter", year, src))) {
      tw <- min(get.maca("tasmin", gcm, scenario, d$Winter[1], d$Winter[2]), na.rm = TRUE) - 273.15
      save.layer(tw, "Tmin", "Winter", year, src)
    }
  }
  file.remove(list.files(file.path(maca.dir, src), pattern = "json$", full.names = TRUE))
}

if (length(humidity.method)) {
  hm <- data.frame(source = names(humidity.method), method = unlist(humidity.method))
  f <- file.path(qa.dir, "R05_humidity_method.csv")
  if (file.exists(f)) hm <- rbind(read.csv(f) %>% filter(!source %in% hm$source), hm)
  write.csv(hm, f, row.names = FALSE); print(hm)
}
message("Next: R06_Project_RF_Future.R")
