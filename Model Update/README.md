# RUSTMapper: corrected re-projection, 1980–present and 2030–2099

These scripts re-project WPBR risk with **the same fitted models** (`RF_MODELFIT_Results_DAYMET.RDATA`, no refit). Observed climate comes from gridMET (1980 to the latest complete year) and future climate from MACAv2-METDATA (2030–2099). Each predictor is built the way the model it feeds was trained (`01_STEP1_BuildingDataFiles_DAYMET.R`). Current and future runs share the same code (`R00b_ProjectionFunctions.R`).

## What was wrong and how it's fixed

| # | Problem | Where | Fix |
|---|---|---|---|
| 1 | `VPD_*`/`VPDmax_*` were vapour pressure **deficit**. The models were trained on DAYMET `vp`, which is **actual vapour pressure** (Pa). The training `VPD_Fall` range of 342–910 Pa confirms this. | current + future | Daily actual vapour pressure: FAO-56 eq. 17 from Tmin/Tmax and RHmax/RHmin. The layers are the seasonal mean and max. |
| 2 | Summer VPD was built from the Apr–May dates. | current + future | Summer layers now use Jun–Aug. |
| 3 | `RH_*` was the mean of RHmin and RHmax. Training used `vp / es(Tmean) × 100`, and the 20-year block used `620.8` and `×200`, so its training RH runs from 52 to 149. | current + future | RH is now calculated the training way. The 20-year models get RH × 1.968. |
| 4 | **18 of the 44 archived summer RH layers are corrupt** (e.g. 2000, 2023: mostly >100%, range −490 to 1,617). They're the oversized files in `GRIDMET_CURRENT`. | current | RH is rebuilt from scratch. |
| 5 | `H_Zone` came from the Jan–Mar minimum, a focal gap-fill and bilinear resampling. **Future zones came from summer Tmin, giving zones 10–13. Training only has zones 2–8.** | current + future | `H_Zone` now comes from the Jan–Feb minimum rounded to 0.1 °C, classified as in training, with nearest-neighbour resampling. |
| 6 | **Future Tmin/Tmax were seasonal means.** Training uses the seasonal min/max. In 2030, spring Tmin came out about 14 °C too warm and Tmax 7–10 °C too cool. Spring/summer Tmax came from BNU-ESM only. | future | Tmin/Tmax now use the seasonal min/max, from every GCM. |
| 7 | Every model was given single-year climate. Each model was trained on a summary of the **5/10/20 years ending at the survey year**. | current + future | A model's projection for year Y uses the same window ending in Y. Switch: `MATCH_TRAINING_WINDOWS`. |
| 8 | GCMs were averaged at the climate-input stage, with precipitation summed and then divided by 5. | future | Each GCM is projected separately, then the risk is averaged across GCMs. The spread across GCMs is saved. |
| 9 | Years were hard-coded by position (`[[1:44]]`, `[[45:114]]`, `seq(1980,2023)`). | all | Years are tracked with `time()`. `StreamDen` is read by name; `[[5]]` was the same layer, so that one wasn't a bug. |

## Running

In the repo, open `RUSTMapper.Rproj` (so the working directory is the repo root) and run `source("Model Update/R99_Run_All.R")`. The scripts find each other in `Model Update/`. If that folder isn't in the working directory, they fall back to the copy at `/Volumes/MaloneLab/Research/RUSTMAPPER/09_REPROJECT_GRIDMET`. Data paths are set in `R00_Config.R` (`data.dir`), and outputs go to `<data.dir>/REPROJECT_GRIDMET/`.

## Run order (`R99_Run_All.R` runs everything)

| Script | Does | Output (`<data.dir>/REPROJECT_GRIDMET/`) |
|---|---|---|
| `R00_Config.R` | Paths, years, GCMs, scenario, switches. **Edit here only.** | – |
| `R00b_ProjectionFunctions.R` | Window predictors and predictions, shared by R03 and R06. | – |
| `R01_Download_GRIDMET_Annual.R` | Builds annual gridMET seasonal layers for 1979 to END. Resumable. | `GRIDMET_ANNUAL/` |
| `R02_QA_AnnualLayers.R` | Checks that PRCP, Tmin and Tmax (definitions unchanged) reproduce the archive. | `QA/R02_*.csv` |
| `R03_Project_RF.R` | Projects the observed years. | `RF_Projections/` |
| `R05_Download_MACA_Annual.R` | Builds annual MACA layers for each GCM, 2011–2099. Resumable. Humidity comes from RHmin/RHmax where available, otherwise from specific humidity plus DEM pressure (logged). | `MACA_ANNUAL/<scen>_<gcm>/`, `QA/R05_humidity_method.csv` |
| `R06_Project_RF_Future.R` | Projects 2030–2099 for each GCM, then averages across GCMs. | `RF_Projections_Future/<scen>/` |
| `R04_Assemble_Ensemble.R` | Assembles a 1980–END + 2030–2099 stack for each scenario in the format scripts 06–08 expect, and plots old vs new. | `Ensemble_*.tif`, `QA/R04_*` |

## Points to know

- **Scenarios.** Both RCP4.5 and RCP8.5 are run (`SCENARIOS <- c("rcp45", "rcp85")`). R04 writes one 1980–2099 stack per scenario. The old script left most `getMACA` calls at climateR's default (`rcp45`); only spring precipitation asked for `"rcp8"`, and the paper doesn't name a scenario.
- **Early observed years have fewer models.** gridMET starts in 1979. With `INCOMPLETE_WINDOW_RULE = "available"`, the ensemble is the 5-year model only for 1983–87 and the 5- and 10-year models for 1988–97. It uses all three models from 1998. `Ensemble_N_*` records the count. Future years always have complete windows, because MACA is downloaded from 2011.
- **Training was on DAYMET, projections use gridMET/MACA.** MACAv2-METDATA is bias-corrected to gridMET, so current and future layers are on the same footing. `04_DAYMETvsGRIDMET.R` covers the DAYMET–gridMET difference.
- **Full range, no High-5 mask.** All model layers cover the whole 11-state study area. The old workflow masked established risk to `wp` and invading risk to `wp.s`. Those masks are now used only in the QA summaries, so old and new maps are compared over the same cells. Range checks are reported for both the full area and the High-5 range. To mask later, use the shapes in `Final_ShapeFiles.RDATA`.
- **Scripts 06–08.** Point them at `REPROJECT_GRIDMET/Ensemble_<first>-<END>+2030-2099_rcp45_EST.tif` or `..._rcp85_EST.tif` (and `_INV`), and run once per scenario. They now select layers by year.
- **Range checks.** `QA/R03_*` and `QA/R06_outside_training_range.csv` give the % of High-5 cells outside each model's training range, by year. Random forests flat-line outside that range, so read these before interpreting late-century risk.
