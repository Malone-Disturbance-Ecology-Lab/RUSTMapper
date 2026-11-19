# =============================================================================
# RUSTMapper RE-PROJECTION :: R00_Config.R
# -----------------------------------------------------------------------------
# Re-projects WPBR risk with the SAME fitted models
# (RF_MODELFIT_Results_DAYMET.RDATA, no refit) for every year with gridMET data
# (1980 - latest complete year), building each projection predictor the way
# that model's training predictor was built in 01_STEP1_BuildingDataFiles_DAYMET.R.
#
# What changes relative to 00_GridMetDownload.R / 05_Spatial RFMODELS_GRIDMET.R:
#  1. VPD_* / VPDmax_*: the models were trained on DAYMET `vp` (actual vapor
#     pressure, Pa), not vapor pressure deficit. Projection layers are now daily
#     actual vapor pressure from gridMET (FAO-56 eq. 17: Tmin/Tmax + RHmax/RHmin),
#     seasonal mean and max, in Pa.
#  2. Summer VPD layers use Jun-Aug (the old download used Apr-May).
#  3. RH_*: training RH = daily vp / es(Tmean) * 100, Tmean = 0.606 Tmax + 0.394 Tmin,
#     seasonal mean. Same here. The 20-year training block used es constant 620.8
#     and *200, so 20-year models receive RH * (2 * 610.8 / 620.8), i.e. the
#     scale they were trained on.
#  4. H_Zone: training classified the Jan-Feb minimum Tmin (rounded to 0.1 C);
#     same here (old layers used Jan-Mar plus a focal gap-fill).
#  5. Training windows: a 5/10/20-year model was trained on climate summarised
#     over the 5/10/20 years ending at the survey year (PRCP = mean seasonal
#     total; Tmin = min; Tmax = max; vp, RH = mean; VPmax = max). Projection for
#     year Y with that model uses the same window ending in Y.
#  6. StreamDen uses the `Density` layer by name (as in training), not [[5]].
#  7. Layers are tracked by year (time()), never by position.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(AOI)
  library(terra)
})

# ---- PATHS ------------------------------------------------------------------
data.dir   <- "/Volumes/MaloneLab/Research/RUSTMAPPER"
script.dir <- if (dir.exists("Model Update")) normalizePath("Model Update") else
              "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET"

archive.gridmet.dir <- file.path(data.dir, "GRIDMET_CURRENT")   # old layers (template + QA)
model.file          <- file.path(data.dir, "RF_MODELFIT_Results_DAYMET.RDATA")
hzone.table.file    <- file.path(data.dir, "HardinessZones.csv")
stream.file         <- file.path(data.dir, "StreamDensity_WestUSA_1km.tif")
tpi.file            <- file.path(data.dir, "tpi_2021.tif")
tri.file            <- file.path(data.dir, "tri_2021.tif")
shapes.file         <- file.path(data.dir, "Final_ShapeFiles.RDATA")
ens.archive <- list(est = file.path(data.dir, "Ensemble_1980-2099_EST.tif"),
                    inv = file.path(data.dir, "Ensemble_1980-2099_INV.tif"))

out.dir      <- file.path(data.dir, "REPROJECT_GRIDMET")
annual.dir   <- file.path(out.dir, "GRIDMET_ANNUAL")   # annual seasonal layers (native grid)
proj.dir     <- file.path(out.dir, "RF_Projections")   # per-year ensemble outputs
qa.dir       <- file.path(out.dir, "QA")
maca.dir     <- file.path(out.dir, "MACA_ANNUAL")      # future annual seasonal layers, one folder per GCM
future.dir   <- file.path(out.dir, "RF_Projections_Future")
dem.file     <- file.path(data.dir, "DEM_2021.tif")     # only for the specific-humidity fallback
for (d in c(out.dir, annual.dir, proj.dir, qa.dir, maca.dir, future.dir)) dir.create(d, showWarnings = FALSE, recursive = TRUE)

# ---- YEARS ------------------------------------------------------------------
GRIDMET_FIRST_YEAR <- 1979            # first year gridMET provides
PROJ_FIRST_YEAR    <- 1980            # first projected year (as in the original maps)
PROJ_END_YEAR <- {                    # last year with complete Sep-Oct data
  today <- Sys.Date(); yr <- as.numeric(format(today, "%Y"))
  if (today >= as.Date(paste0(yr, "-11-10"))) yr else yr - 1
}
DOWNLOAD_YEARS <- GRIDMET_FIRST_YEAR:PROJ_END_YEAR
PROJ_YEARS     <- PROJ_FIRST_YEAR:PROJ_END_YEAR

# ---- FUTURE (MACAv2-METDATA, same GCMs as FLOW.ClimateChangeDownload.R) -------
GCMS <- c("BNU-ESM", "CanESM2", "CCSM4", "CNRM-CM5", "IPSL-CM5B-LR")
# FLOW.ClimateChangeDownload.R left most calls at climateR's default scenario ("rcp45");
# spring PRCP asked for "rcp8". Both scenarios are run; R05/R06 loop over them and
# R04 writes one 1980-2099 stack per scenario.
SCENARIOS <- c("rcp45", "rcp85")
FUTURE_YEARS        <- 2030:2099
MACA_FIRST_YEAR     <- 2011                       # 20-year window for 2030 starts in 2011
MACA_DOWNLOAD_YEARS <- MACA_FIRST_YEAR:max(FUTURE_YEARS)
maca.src <- function(scenario, gcm) paste("maca", scenario, gcm, sep = "_")

# ---- MATCHING THE TRAINING DATA ---------------------------------------------
MATCH_TRAINING_WINDOWS <- TRUE   # FALSE = every model uses single-year climate (old behaviour)
MODEL_WINDOWS <- c("5" = 5, "10" = 10, "20" = 20)

# A window that would start before 1979 cannot be built from gridMET.
#  "available": ensemble = mean of the models whose window is complete
#               (1980-82: none -> skipped unless partial; 1983-87: 5-yr only;
#                1988-97: 5 + 10-yr; 1998+: all three)
#  "partial":   use whatever years exist from 1979 on (shorter window)
INCOMPLETE_WINDOW_RULE <- "available"

RH20_FACTOR <- 2 * 610.8 / 620.8   # 20-year training RH scale (see header, item 3)

# ---- STUDY AREA / GRID ------------------------------------------------------
STATES  <- c("CO", "WA", "OR", "CA", "MT", "ID", "UT", "AZ", "NV", "WY", "NM")
LCC.CRS <- "+proj=lcc +lat_0=42.5 +lon_0=-100 +lat_1=25 +lat_2=60 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
AOI <- AOI::aoi_get(state = STATES) %>% st_transform(4326)

# Output grid = the grid of the published maps (archived gridMET layer, cropped/
# masked to the AOI and projected to LCC, as in climate.import.gridmet()).
make.template <- function() {
  f <- list.files(archive.gridmet.dir, pattern = "^PRCP_Fall_1980", full.names = TRUE)[1]
  if (is.na(f)) stop("Template not found: PRCP_Fall_1980 in ", archive.gridmet.dir)
  r <- rast(f); a <- st_transform(AOI, crs(r))
  r %>% crop(a) %>% mask(a) %>% terra::project(LCC.CRS)
}

# ---- DEFINITIONS --------------------------------------------------------------
# Seasons exactly as in 01_STEP1_BuildingDataFiles_DAYMET.R
season.dates <- function(year) list(
  Winter = c(sprintf("%d-01-01", year), format(as.Date(sprintf("%d-03-01", year)) - 1)),  # Jan-Feb
  Spring = c(sprintf("%d-04-01", year), sprintf("%d-05-31", year)),
  Summer = c(sprintf("%d-06-01", year), sprintf("%d-08-31", year)),
  Fall   = c(sprintf("%d-09-01", year), sprintf("%d-10-31", year)))
SEASONS <- c("Spring", "Summer", "Fall")

# Saturation vapour pressure (Pa), the constant used in training (610.8)
es.pa <- function(t.c) 610.8 * exp((17.27 * t.c) / (237.3 + t.c))

# Annual layer variables, how each is aggregated across a training window,
# and the model predictor name each one feeds.
ANNUAL.VARS <- data.frame(
  var      = c("PRCP", "Tmin", "Tmax", "VP",   "VPmax", "RH"),
  window   = c("mean", "min",  "max",  "mean", "max",   "mean"),
  model.pfx= c("PRCP", "Tmin", "Tmax", "VPD",  "VPDmax","RH"),
  stringsAsFactors = FALSE)

# src = "gridmet" (observed) or a MACA source id such as "maca_rcp45_CanESM2" (future)
annual.file <- function(var, season, year, src = "gridmet") {
  d <- if (src == "gridmet") annual.dir else file.path(maca.dir, src)
  file.path(d, sprintf("%s_%s_%d.tif", var, season, year))
}

# ---- HELPERS --------------------------------------------------------------------
get.gridmet <- function(varname, start, end, aoi = AOI, tries = 4) {
  for (k in seq_len(tries)) {
    out <- tryCatch(climateR::getGridMET(aoi, varname = varname, startDate = start, endDate = end),
                    error = function(e) e)
    if (!inherits(out, "error")) return(if (inherits(out, "SpatRaster")) out else out[[1]])
    message("  getGridMET(", varname, " ", start, ") failed [try ", k, "]: ", conditionMessage(out))
    Sys.sleep(15 * k)
  }
  stop("getGridMET failed: ", varname, " ", start, "-", end)
}

# Training classifier (01_STEP1_BuildingDataFiles_DAYMET.R::CALC_H_ZONE), on rasters
calc.hzone <- function(tmin, hz = read.csv(hzone.table.file)) {
  tmin <- round(tmin, 1)
  z <- tmin; z[] <- NA
  z[tmin <= hz$Temp_upper[hz$H_Zone == 1]] <- 1
  for (k in 2:12) z[tmin <= hz$Temp_upper[hz$H_Zone == k] & tmin >= hz$Temp_Lower[hz$H_Zone == k]] <- k
  z[tmin >= hz$Temp_Lower[hz$H_Zone == 13]] <- 13
  z
}

# Years in the window ending at `year` for a w-year model; NULL if not allowed
window.years <- function(year, w, first.year = GRIDMET_FIRST_YEAR) {
  if (!MATCH_TRAINING_WINDOWS) return(year)
  yrs <- (year - w + 1):year
  if (min(yrs) >= first.year) return(yrs)
  if (INCOMPLETE_WINDOW_RULE == "partial") return(yrs[yrs >= first.year])
  NULL
}

p.rust <- function(pred) if ("X1" %in% names(pred)) pred[["X1"]] else pred[[2]]

message("R00_Config: projection years ", PROJ_FIRST_YEAR, "-", PROJ_END_YEAR,
        " | windows matched: ", MATCH_TRAINING_WINDOWS, " (", INCOMPLETE_WINDOW_RULE, ")")
