# =============================================================================
# RUSTMapper RE-PROJECTION :: R06_Project_RF_Future.R   (2030-2099, MACA)
# -----------------------------------------------------------------------------
# Same fitted models and the same code path as R03 (R00b_ProjectionFunctions.R),
# applied to each GCM separately:
#   1. per GCM and year: training-window predictors (5/10/20 years ending in Y)
#      -> P(RUST) from all six models -> RF ensemble mean (as for observed years)
#   2. across GCMs: mean of the per-GCM ensembles = projection for the scenario;
#      SD across GCMs = climate-model spread.
# (The old workflow averaged the climate inputs across GCMs before predicting.
#  Predicting per GCM keeps each GCM's climate internally consistent.)
#
# Outputs (REPROJECT_GRIDMET/RF_Projections_Future/):
#   <scen>/{inv,est}_mean_<Y>.tif     mean over GCMs of the RF ensemble mean
#   <scen>/{inv,est}_gcmsd_<Y>.tif    SD across GCMs
#   <scen>/{inv,est}_rfsd_<Y>.tif     mean over GCMs of the SD across RF models
#   <scen>/gcm/{inv,est}_<gcm>_<Y>.tif per-GCM RF ensemble mean
#   ../QA/R06_outside_training_range.csv
# =============================================================================

rm(list = ls())
# Works from the repo (RUSTMapper.Rproj open -> "Model Update/") or from the data drive copy
source(if (file.exists("Model Update/R00_Config.R")) "Model Update/R00_Config.R" else
       "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET/R00_Config.R")
source(file.path(script.dir, "R00b_ProjectionFunctions.R"))

OVERWRITE <- FALSE
N_CORES   <- 1

for (scenario in SCENARIOS) {
  sdir <- file.path(future.dir, scenario); gdir <- file.path(sdir, "gcm")
  dir.create(gdir, showWarnings = FALSE, recursive = TRUE)
  out.file <- function(type, what, year) file.path(sdir, sprintf("%s_%s_%d.tif", type, what, year))

  for (year in FUTURE_YEARS) {
    if (!OVERWRITE && all(file.exists(out.file(c("inv", "est"), "mean", year)))) {
      message(scenario, " ", year, ": exists, skipping"); next
    }
    message("Projecting ", scenario, " ", year)
    gcm.mean <- list(inv = list(), est = list()); gcm.sd <- list(inv = list(), est = list())

    for (gcm in GCMS) {
      src <- maca.src(scenario, gcm)
      if (!file.exists(annual.file("PRCP", "Fall", year, src))) { message("  missing ", src, " ", year); next }
      pr <- predict.year(year, src, MACA_FIRST_YEAR, N_CORES)
      for (type in c("inv", "est")) {
        if (!length(pr[[type]])) next
        res <- summarise.preds(pr[[type]], paste(type, gcm, sep = "_"), year)
        writeRaster(res$mean, file.path(gdir, sprintf("%s_%s_%d.tif", type, gcm, year)), overwrite = TRUE)
        gcm.mean[[type]][[gcm]] <- res$mean
        gcm.sd[[type]][[gcm]]   <- res$sd
      }
      append.csv(pr$range, file.path(qa.dir, "R06_outside_training_range.csv"))
    }

    for (type in c("inv", "est")) {
      if (length(gcm.mean[[type]]) < length(GCMS))
        warning(scenario, " ", year, " ", type, ": only ", length(gcm.mean[[type]]), " of ", length(GCMS), " GCMs")
      if (!length(gcm.mean[[type]])) next
      g <- summarise.preds(gcm.mean[[type]], type, year)
      rfsd <- mean(rast(unname(gcm.sd[[type]]))); time(rfsd) <- year
      names(rfsd) <- paste(type, "rfsd", year, sep = "-")
      writeRaster(g$mean, out.file(type, "mean",  year), overwrite = TRUE)
      writeRaster(g$sd,   out.file(type, "gcmsd", year), overwrite = TRUE)
      writeRaster(g$n,    out.file(type, "ngcm",  year), overwrite = TRUE)
      writeRaster(rfsd,   out.file(type, "rfsd",  year), overwrite = TRUE)
    }
  }
}
message("Next: R04_Assemble_Ensemble.R")
