# =============================================================================
# RUSTMapper RE-PROJECTION :: R00b_ProjectionFunctions.R
# -----------------------------------------------------------------------------
# Shared by R03 (gridMET, 1980-present) and R06 (MACA, 2030-2099) so current
# and future projections are built by identical code. Source after R00_Config.R.
# Creates: template, models, train.data, static, h5, hz.table + functions.
# =============================================================================

suppressPackageStartupMessages(library(randomForest))

template <- make.template()

# ---- Models (unchanged) ---------------------------------------------------------
load(model.file)
models <- list(
  inv = list("5" = rf.invading.5,    "10" = rf.invading.10,    "20" = rf.invading.20),
  est = list("5" = rf.established.5, "10" = rf.established.10, "20" = rf.established.20))
train.data <- list(
  inv = list("5"  = rbind(train.5.invading,  test.5.invading[,  names(train.5.invading)]),
             "10" = rbind(train.10.invading, test.10.invading[, names(train.10.invading)]),
             "20" = rbind(train.20.invading, test.20.invading[, names(train.20.invading)])),
  est = list("5"  = rbind(train.5.established,  test.5.established[,  names(train.5.established)]),
             "10" = rbind(train.10.established, test.10.established[, names(train.10.established)]),
             "20" = rbind(train.20.established, test.20.established[, names(train.20.established)])))
model.vars <- function(m) rownames(importance(m))

# ---- Static layers, prepared as in training -------------------------------------
stream <- rast(stream.file)
streamDen <- (if ("Density" %in% names(stream)) stream[["Density"]] else stream[[5]]) %>%
  terra::project(template, method = "bilinear")
streamDen[is.na(streamDen)] <- 0
streamDen <- mask(streamDen, template)
tpi <- rast(tpi.file) %>% terra::project(template, method = "bilinear")
tri <- rast(tri.file) %>% terra::project(template, method = "bilinear")
static <- c(streamDen, tpi, tri); names(static) <- c("StreamDen", "TPI", "TRI")

load(shapes.file)
h5 <- vect(st_transform(st_as_sf(wp), crs(template)))
hz.table <- read.csv(hzone.table.file)

# ---- Window predictor stack ----------------------------------------------------
agg <- function(var, season, yrs, how, src) {
  r <- rast(annual.file(var, season, yrs, src))
  if (nlyr(r) == 1) r else app(r, fun = how, na.rm = TRUE)
}

# All predictors for the window `yrs`, for a model trained on a w-year window
window.stack <- function(yrs, w, src = "gridmet") {
  clim <- list()
  for (i in seq_len(nrow(ANNUAL.VARS))) for (s in SEASONS) {
    v <- ANNUAL.VARS[i, ]
    clim[[paste(v$model.pfx, s, sep = "_")]] <- agg(v$var, s, yrs, v$window, src)
  }
  clim.names <- names(clim)
  clim <- rast(unname(clim)); names(clim) <- clim.names
  if (w == 20) {                                   # 20-year training RH scale
    rh <- grep("^RH_", names(clim))
    clim[[rh]] <- clim[[rh]] * RH20_FACTOR
  }
  clim <- terra::project(clim, template, method = "bilinear") %>% mask(template)
  hz <- calc.hzone(agg("Tmin", "Winter", yrs, "min", src), hz.table) %>%
    terra::project(template, method = "near") %>% mask(template)
  names(hz) <- "H_Zone"
  c(clim, hz, static)
}

range.check <- function(stack, type, w, year, src) {
  m <- models[[type]][[w]]; td <- as.data.frame(train.data[[type]][[w]])
  vars <- intersect(model.vars(m), names(td))
  areas <- list("full" = stack[[vars]], "High-5" = mask(stack[[vars]], h5))
  do.call(rbind, lapply(names(areas), function(a) {
    vals <- as.data.frame(areas[[a]], na.rm = TRUE)
    do.call(rbind, lapply(vars, function(v) data.frame(
      source = src, year = year, area = a, model = paste0(type, ".", w), variable = v,
      pct.below = 100 * mean(vals[[v]] < min(td[[v]], na.rm = TRUE)),
      pct.above = 100 * mean(vals[[v]] > max(td[[v]], na.rm = TRUE)))))
  }))
}

# P(RUST) from every model whose window is available for `year`.
# Returns list(inv = list("5"=SpatRaster, ...), est = ..., range = data.frame)
predict.year <- function(year, src = "gridmet", first.year = GRIDMET_FIRST_YEAR, n.cores = 1) {
  preds <- list(inv = list(), est = list()); rng <- list()
  for (w in names(MODEL_WINDOWS)) {
    yrs <- window.years(year, MODEL_WINDOWS[[w]], first.year)
    if (is.null(yrs)) next
    stack <- window.stack(yrs, MODEL_WINDOWS[[w]], src)
    for (type in c("inv", "est")) {
      m <- models[[type]][[w]]
      miss <- setdiff(model.vars(m), names(stack))
      if (length(miss)) stop("Missing predictors for ", type, ".", w, ": ", paste(miss, collapse = ", "))
      p <- p.rust(terra::predict(stack[[model.vars(m)]], m, na.rm = TRUE, cpkgs = "randomForest",
                                 type = "prob", cores = n.cores))
      names(p) <- sprintf("%s_%syr_%d", type, w, year); time(p) <- year
      preds[[type]][[w]] <- p
      rng[[length(rng) + 1]] <- range.check(stack, type, w, year, src)
    }
    rm(stack); gc()
  }
  preds$range <- do.call(rbind, rng)
  preds
}

# mean / sd / n across a list of single-layer probability rasters
summarise.preds <- function(plist, label, year) {
  p <- rast(unname(plist))
  res <- list(mean = mean(p), sd = if (nlyr(p) > 1) stdev(p) else p * 0, n = sum(!is.na(p)))
  for (k in names(res)) { names(res[[k]]) <- paste(label, k, year, sep = "-"); time(res[[k]]) <- year }
  res
}

append.csv <- function(df, f, key = c("source", "year")) {
  if (is.null(df) || !nrow(df)) return(invisible())
  if (file.exists(f)) {
    old <- read.csv(f)
    drop <- do.call(paste, old[key]) %in% unique(do.call(paste, df[key]))
    df <- rbind(old[!drop, names(df)], df)
  }
  write.csv(df, f, row.names = FALSE)
}
