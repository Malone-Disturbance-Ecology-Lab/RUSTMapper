# RUSTMapper RE-PROJECTION :: R99_Run_All.R — observed (R01-R03), future (R05-R06), then R04.
# Edit paths/years/switches in R00_Config.R only. R01 and R03 are resumable.

script.dir <- if (dir.exists("Model Update")) normalizePath("Model Update") else
              "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET"
for (s in c("R01_Download_GRIDMET_Annual.R", "R02_QA_AnnualLayers.R", "R03_Project_RF.R",
            "R05_Download_MACA_Annual.R", "R06_Project_RF_Future.R",
            "R04_Assemble_Ensemble.R")) {
  message("\n########## ", s, " ##########"); t0 <- Sys.time()
  source(file.path(script.dir, s))
  message(s, " finished in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
}
