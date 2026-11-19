# =============================================================================
# RUSTMapper RE-PROJECTION :: R03_Project_RF.R   (observed climate, gridMET)
# -----------------------------------------------------------------------------
# For each year Y and model window w (5, 10, 20): aggregate the annual gridMET
# layers over the w years ending in Y as in training, then predict P(RUST)
# with the unchanged rf.invading.<w> / rf.established.<w> (see R00b).
# Ensemble per year = mean and SD across the available models + their count.
#
# Outputs (REPROJECT_GRIDMET/RF_Projections/):
#   {inv,est}_{mean,sd,n}_<Y>.tif ; models/{inv,est}_<w>yr_<Y>.tif
#   ../QA/R03_outside_training_range.csv
# =============================================================================

rm(list = ls())
# Works from the repo (RUSTMapper.Rproj open -> "Model Update/") or from the data drive copy
source(if (file.exists("Model Update/R00_Config.R")) "Model Update/R00_Config.R" else
       "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET/R00_Config.R")
source(file.path(script.dir, "R00b_ProjectionFunctions.R"))

OVERWRITE <- FALSE
N_CORES   <- 1

model.out <- file.path(proj.dir, "models"); dir.create(model.out, showWarnings = FALSE)
out.file  <- function(type, what, year) file.path(proj.dir, sprintf("%s_%s_%d.tif", type, what, year))

for (year in PROJ_YEARS) {
  if (!OVERWRITE && all(file.exists(out.file(c("inv", "est"), "mean", year)))) {
    message(year, ": exists, skipping"); next
  }
  message("Projecting ", year)
  pr <- predict.year(year, "gridmet", GRIDMET_FIRST_YEAR, N_CORES)

  for (type in c("inv", "est")) {
    if (!length(pr[[type]])) { message("  no complete model window for ", year); next }
    for (w in names(pr[[type]]))
      writeRaster(pr[[type]][[w]], file.path(model.out, sprintf("%s_%syr_%d.tif", type, w, year)), overwrite = TRUE)
    res <- summarise.preds(pr[[type]], type, year)
    for (k in names(res)) writeRaster(res[[k]], out.file(type, k, year), overwrite = TRUE)
  }
  append.csv(pr$range, file.path(qa.dir, "R03_outside_training_range.csv"))
}
message("Next: R04_Assemble_Ensemble.R (or R05/R06 for the future period)")
