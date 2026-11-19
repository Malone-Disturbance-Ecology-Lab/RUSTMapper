# =============================================================================
# RUSTMapper RE-PROJECTION :: R04_Assemble_Ensemble.R
# -----------------------------------------------------------------------------
# Stacks the per-year outputs of R03 into the format the downstream scripts
# (06, 07, 08) read, and compares them with the archived ensemble.
# Layers cover the FULL study area (11 western states), NOT masked to the High-5
# range (05_Spatial RFMODELS_GRIDMET.R masked established -> wp, invading -> wp.s).
# Those masks are used only for the QA summaries, to compare like with like
# against the archived (masked) maps. Mask later with Final_ShapeFiles.RDATA if needed.
#
# Outputs (REPROJECT_GRIDMET/):
#   Ensemble_<first>-<END>_EST.tif / _INV.tif          ensemble mean, one layer per year
#   Ensemble_SD_<first>-<END>_EST.tif / _INV.tif       SD across models
#   Ensemble_N_<first>-<END>_EST.tif / _INV.tif        number of models per cell-year
#   QA/R04_mean_risk_old_vs_new.csv / .png
#
# FUTURE (2030-2099): FUTURE_SOURCE = "reprojected" appends the R06 outputs (same
# models, corrected definitions, mean over GCMs) for EACH scenario in SCENARIOS,
# giving the 1980-2099 stacks scripts 06-08 expect. "archived" appends the old
# future layers (old definitions; comparison only). "none" = observed years only.
#   Ensemble_<first>-<END>+2030-2099_<scen>_EST.tif / _INV.tif   (one per scenario)
#   Ensemble_GCMSD_2030-2099_<scen>_*.tif  spread across GCMs
#   Ensemble_RFSD_2030-2099_<scen>_*.tif   spread across the 5/10/20-yr models
# =============================================================================

rm(list = ls())
# Works from the repo (RUSTMapper.Rproj open -> "Model Update/") or from the data drive copy
source(if (file.exists("Model Update/R00_Config.R")) "Model Update/R00_Config.R" else
       "/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET/R00_Config.R")

FUTURE_SOURCE <- "reprojected"   # "reprojected" (one stack per scenario in SCENARIOS), "archived" or "none"

load(shapes.file)
template <- make.template()
aoi.lcc  <- st_transform(AOI, crs(template))
masks <- list(est = vect(st_transform(st_as_sf(wp),   crs(template))),
              inv = vect(st_transform(st_as_sf(wp.s), crs(template))))

stack.years <- function(type, what) {
  f <- file.path(proj.dir, sprintf("%s_%s_%d.tif", type, what, PROJ_YEARS))
  keep <- file.exists(f)
  if (!any(keep)) stop("No R03 outputs for ", type, " ", what)
  r <- rast(f[keep]) %>% crop(aoi.lcc)                     # full range: no High-5 mask
  yrs <- PROJ_YEARS[keep]
  time(r) <- yrs
  names(r) <- paste(switch(what, mean = type, sd = paste0(type, ".sd"), n = paste0(type, ".n")), yrs, sep = "-")
  r
}
future.stack <- function(type, scen, what = "mean", like) {
  ff <- file.path(future.dir, scen, sprintf("%s_%s_%d.tif", type, what, FUTURE_YEARS))
  if (!all(file.exists(ff))) stop(sum(!file.exists(ff)), " R06 '", what, "' layers missing for ", scen, " ", type)
  r <- rast(ff) %>% crop(aoi.lcc)                          # full range: no High-5 mask
  if (!compareGeom(r, like, stopOnError = FALSE)) r <- resample(r, like)
  time(r) <- FUTURE_YEARS
  names(r) <- paste(if (what == "mean") type else paste0(type, ".", what), FUTURE_YEARS, sep = "-")
  r
}
archived.future <- function(type, like) {
  old <- rast(ens.archive[[type]])
  t.old <- as.numeric(time(old)); if (anyNA(t.old)) t.old <- c(1980:2023, 2030:2099)
  fut <- old[[which(t.old >= 2030)]]; time(fut) <- t.old[t.old >= 2030]
  if (!compareGeom(fut, like, stopOnError = FALSE)) fut <- resample(fut, like)
  names(fut) <- paste(type, time(fut), sep = "-")
  warning("Archived future layers use the OLD predictor definitions - comparison only.")
  fut
}

span <- function(r) paste(range(time(r)), collapse = "-")
current <- list(); combined <- list()
for (type in c("est", "inv")) {
  lab <- toupper(type)
  for (what in c("mean", "sd", "n")) {               # observed period
    r <- stack.years(type, what)
    f <- switch(what, mean = sprintf("Ensemble_%s_%s.tif", span(r), lab),
                      sd   = sprintf("Ensemble_SD_%s_%s.tif", span(r), lab),
                      n    = sprintf("Ensemble_N_%s_%s.tif",  span(r), lab))
    writeRaster(r, file.path(out.dir, f), overwrite = TRUE); message("Wrote ", f)
    if (what == "mean") current[[type]] <- r
  }
  cur <- current[[type]]
  if (FUTURE_SOURCE == "reprojected") {              # one combined stack per scenario
    for (scen in SCENARIOS) {
      fut <- future.stack(type, scen, "mean", cur)
      f <- sprintf("Ensemble_%s+2030-2099_%s_%s.tif", span(cur), scen, lab)
      writeRaster(c(cur, fut), file.path(out.dir, f), overwrite = TRUE); message("Wrote ", f)
      for (what in c("gcmsd", "rfsd")) {
        g <- future.stack(type, scen, what, cur)
        writeRaster(g, file.path(out.dir, sprintf("Ensemble_%s_2030-2099_%s_%s.tif", toupper(what), scen, lab)), overwrite = TRUE)
      }
      combined[[paste(type, scen)]] <- c(cur, fut)
    }
  } else if (FUTURE_SOURCE == "archived") {
    fut <- archived.future(type, cur)                        # archived layers are already masked
    f <- sprintf("Ensemble_%s+2030-2099_archivedFUTURE_%s.tif", span(cur), lab)
    writeRaster(c(cur, fut), file.path(out.dir, f), overwrite = TRUE)
    combined[[paste(type, "archived")]] <- c(cur, fut)
  }
}

# ---- QA: archived vs re-projected mean risk over the High-5 range ------------
# Summaries use the archived maps' masks (est -> wp, inv -> wp.s) so old and new
# are compared over the same cells; "full" rows summarise the whole study area.
ts <- function(r, label, type, area = c("High-5", "full")) {
  area <- match.arg(area)
  if (area == "High-5") r <- mask(r, masks[[type]])
  data.frame(year = as.numeric(time(r)), mean = global(r, "mean", na.rm = TRUE)[, 1],
             pct.high = global(r > 0.5, "mean", na.rm = TRUE)[, 1] * 100,
             series = label, area = area)
}
qa <- do.call(rbind, lapply(c("est", "inv"), function(type) {
  old <- rast(ens.archive[[type]])
  t.old <- as.numeric(time(old)); if (anyNA(t.old)) t.old <- c(1980:2023, 2030:2099)
  time(old) <- t.old
  scens <- if (FUTURE_SOURCE == "reprojected") SCENARIOS else character(0)
  new <- c(list(observed = current[[type]]),
           setNames(lapply(scens, function(scen) future.stack(type, scen, "mean", current[[type]])), scens))
  rbind(ts(old, paste(toupper(type), "archived"), type),
        do.call(rbind, lapply(names(new), function(k) rbind(
          ts(new[[k]], paste(toupper(type), "re-projected", k), type, "High-5"),
          ts(new[[k]], paste(toupper(type), "re-projected", k), type, "full")))))
}))
write.csv(qa, file.path(qa.dir, "R04_mean_risk_old_vs_new.csv"), row.names = FALSE)

png(file.path(qa.dir, "R04_mean_risk_old_vs_new.png"), width = 1200, height = 500)
par(mfrow = c(1, 2))
qa.plot <- qa[qa$area == "High-5", ]   # like-for-like with the archived maps
series <- unique(qa.plot$series)
cols <- setNames(rep(c("grey60", "black", "steelblue", "darkorange", "pink3", "firebrick", "purple", "goldenrod"), length.out = length(series)), series)
ltys <- setNames(ifelse(grepl("archived", series), 2, 1), series)
for (k in c("mean", "pct.high")) {
  plot(NA, xlim = range(qa.plot$year), ylim = range(qa.plot[[k]], na.rm = TRUE), xlab = "Year",
       ylab = if (k == "mean") "Mean P(WPBR), High-5 range" else "% of High-5 range with P(WPBR) > 0.5",
       main = "High-5 range (same cells as the archived maps)")
  if (MATCH_TRAINING_WINDOWS && INCOMPLETE_WINDOW_RULE == "available") abline(v = c(1987.5, 1997.5), lty = 3, col = "grey70")
  for (s in names(cols)) {
    d <- qa.plot[qa.plot$series == s, ]
    for (seg in list(d$year < 2030, d$year >= 2030)) if (any(seg))   # no line across the 2027-2029 gap
      lines(d$year[seg], d[[k]][seg], col = cols[s], lty = ltys[s], lwd = 2)
  }
  legend("topleft", names(cols), col = cols, lty = ltys, lwd = 2, bty = "n", cex = 0.8)
}
dev.off()
message("Done. Dotted lines mark where the 10- and 20-year models join the ensemble.")
