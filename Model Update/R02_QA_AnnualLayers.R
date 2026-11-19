# =============================================================================
# RUSTMapper RE-PROJECTION :: R02_QA_AnnualLayers.R
# -----------------------------------------------------------------------------
# PRCP, Tmin and Tmax keep their original definitions, so the new annual
# layers should reproduce the archived GRIDMET_CURRENT layers (1980-2023).
# This confirms the download/aggregation code before anything is projected.
# For the redefined variables (VP, VPmax, RH, Tmin_Winter, summer VPD) it also
# reports how far the new layers are from the old ones, for the record.
# -> QA/R02_annual_vs_archive.csv
# =============================================================================

rm(list = ls())
# Works from the repo (RUSTMapper.Rproj open -> "Model Update/") or from the data drive copy
source(if (file.exists("Model Update/R00_Config.R")) "Model Update/R00_Config.R" else
       "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET/R00_Config.R")

# new var/season -> archived file stem (00_GridMetDownload.R names)
pairs <- rbind(
  data.frame(var = "PRCP",  season = SEASONS, old = paste0("PRCP_", SEASONS),   expect = "same"),
  data.frame(var = "Tmin",  season = SEASONS, old = paste0("Tmin_", SEASONS),   expect = "same"),
  data.frame(var = "Tmax",  season = SEASONS, old = paste0("Tmax_", SEASONS),   expect = "same"),
  data.frame(var = "VP",    season = SEASONS, old = c("VPD_Spring", "VPD_Summer", "VPD_fall"),         expect = "redefined: VPD -> vapour pressure"),
  data.frame(var = "VPmax", season = SEASONS, old = c("VPDMAX_Spring", "VPDMAX_Summer", "VPDMAX_fall"), expect = "redefined: VPD -> vapour pressure"),
  data.frame(var = "RH",    season = SEASONS, old = c("RH.Spring", "RH.Summer", "RH.Fall"),             expect = "redefined: training RH"),
  data.frame(var = "Tmin",  season = "Winter", old = "Tmin_Winter", expect = "redefined: Jan-Mar -> Jan-Feb"))

QA_YEARS <- intersect(1980:2023, DOWNLOAD_YEARS)

cmp <- function(i, year) {
  p <- pairs[i, ]
  f.new <- annual.file(p$var, p$season, year)
  f.old <- file.path(archive.gridmet.dir, paste(p$old, year, ".tiff", sep = "_"))
  if (!file.exists(f.new) || !file.exists(f.old)) return(NULL)
  old <- rast(f.old); a <- st_transform(AOI, crs(old))
  old <- old %>% crop(a) %>% mask(a)
  new <- rast(f.new) %>% terra::project(old) %>% mask(old)
  v <- na.omit(data.frame(new = values(new)[, 1], old = values(old)[, 1]))
  data.frame(var = p$var, season = p$season, year = year, expect = p$expect, n = nrow(v),
             mean.old = mean(v$old), mean.new = mean(v$new),
             mean.diff = mean(v$new - v$old), r = suppressWarnings(cor(v$new, v$old)))
}

res <- do.call(rbind, lapply(QA_YEARS, function(y) do.call(rbind, lapply(seq_len(nrow(pairs)), cmp, year = y))))
write.csv(res, file.path(qa.dir, "R02_annual_vs_archive.csv"), row.names = FALSE)

summ <- res %>% group_by(expect, var, season) %>%
  summarise(min.r = round(min(r), 3), mean.diff = signif(mean(mean.diff), 3), .groups = "drop")
print(as.data.frame(summ))

bad <- summ %>% filter(expect == "same", min.r < 0.98)
if (nrow(bad)) {
  warning("PRCP/Tmin/Tmax do not reproduce the archive - check before projecting:\n",
          paste(capture.output(print(as.data.frame(bad))), collapse = "\n"))
} else message("PRCP/Tmin/Tmax reproduce the archived layers (r >= 0.98).")

message("Next: R03_Project_RF.R")
