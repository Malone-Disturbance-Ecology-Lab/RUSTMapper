# =============================================================================
# RUSTMapper RE-PROJECTION :: R01_Download_GRIDMET_Annual.R
# -----------------------------------------------------------------------------
# Builds ANNUAL seasonal layers (native gridMET grid) for 1979 - END with the
# training definitions (01_STEP1_BuildingDataFiles_DAYMET.R):
#
#   PRCP_<s>  = seasonal total of daily pr (mm)
#   Tmin_<s>  = seasonal minimum of daily tmmn (C)
#   Tmax_<s>  = seasonal maximum of daily tmmx (C)
#   VP_<s>    = seasonal mean of daily actual vapour pressure ea (Pa)   -> model "VPD_<s>"
#   VPmax_<s> = seasonal max  of daily ea (Pa)                          -> model "VPDmax_<s>"
#   RH_<s>    = seasonal mean of daily ea / es(Tmean) * 100             -> model "RH_<s>"
#   Tmin_Winter = Jan-Feb minimum of daily tmmn (C)                     -> H_Zone
#
#   ea    = [ es(Tmin) * RHmax/100 + es(Tmax) * RHmin/100 ] / 2   (FAO-56 eq. 17)
#   Tmean = 0.606 Tmax + 0.394 Tmin ;  es(T) = 610.8 exp(17.27 T / (T + 237.3))
#
# Seasons: Spring Apr-May, Summer Jun-Aug, Fall Sep-Oct (summer VPD now Jun-Aug).
# Resumable: a season-year whose files exist is skipped (OVERWRITE <- TRUE to redo).
# =============================================================================

rm(list = ls())
# Works from the repo (RUSTMapper.Rproj open -> "Model Update/") or from the data drive copy
source(if (file.exists("Model Update/R00_Config.R")) "Model Update/R00_Config.R" else
       "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET/R00_Config.R")
library(climateR)

OVERWRITE <- FALSE

save.layer <- function(r, var, season, year) {
  r <- r[[1]]; names(r) <- paste(var, season, sep = "_"); time(r) <- year
  writeRaster(r, annual.file(var, season, year), overwrite = TRUE)
}

for (year in DOWNLOAD_YEARS) {
  d <- season.dates(year)
  message("==== ", year, " ====")

  for (season in SEASONS) {
    if (!OVERWRITE && all(file.exists(annual.file(ANNUAL.VARS$var, season, year)))) {
      message(season, ": exists, skipping"); next
    }
    s <- d[[season]]
    message(season, " ", s[1], " to ", s[2])

    pr   <- get.gridmet("pr",   s[1], s[2])
    tmin <- get.gridmet("tmmn", s[1], s[2]) - 273.15
    tmax <- get.gridmet("tmmx", s[1], s[2]) - 273.15
    rmin <- get.gridmet("rmin", s[1], s[2])
    rmax <- get.gridmet("rmax", s[1], s[2])
    n <- c(nlyr(pr), nlyr(tmin), nlyr(tmax), nlyr(rmin), nlyr(rmax))
    if (length(unique(n)) != 1) stop("Daily layer counts differ for ", season, " ", year, ": ", paste(n, collapse = "/"))

    ea    <- (es.pa(tmin) * rmax / 100 + es.pa(tmax) * rmin / 100) / 2
    tmean <- 0.606 * tmax + 0.394 * tmin
    rh    <- ea / es.pa(tmean) * 100

    save.layer(sum(pr, na.rm = TRUE),   "PRCP",  season, year)
    save.layer(min(tmin, na.rm = TRUE), "Tmin",  season, year)
    save.layer(max(tmax, na.rm = TRUE), "Tmax",  season, year)
    save.layer(mean(ea, na.rm = TRUE),  "VP",    season, year)
    save.layer(max(ea, na.rm = TRUE),   "VPmax", season, year)
    save.layer(mean(rh, na.rm = TRUE),  "RH",    season, year)
    rm(pr, tmin, tmax, rmin, rmax, ea, tmean, rh); gc()
  }

  if (OVERWRITE || !file.exists(annual.file("Tmin", "Winter", year))) {
    message("Winter ", d$Winter[1], " to ", d$Winter[2])
    tw <- min(get.gridmet("tmmn", d$Winter[1], d$Winter[2]), na.rm = TRUE) - 273.15
    save.layer(tw, "Tmin", "Winter", year)
  }
}

file.remove(list.files(annual.dir, pattern = "json$", full.names = TRUE))

expected <- c(unlist(lapply(DOWNLOAD_YEARS, function(y)
                 as.vector(outer(ANNUAL.VARS$var, SEASONS, function(v, s) annual.file(v, s, y))))),
              annual.file("Tmin", "Winter", DOWNLOAD_YEARS))
missing <- expected[!file.exists(expected)]
if (length(missing)) warning(length(missing), " layers missing:\n", paste(basename(missing), collapse = "\n")) else
  message("All ", length(expected), " annual layers present, ", min(DOWNLOAD_YEARS), "-", max(DOWNLOAD_YEARS))
message("Next: R02_QA_AnnualLayers.R")
