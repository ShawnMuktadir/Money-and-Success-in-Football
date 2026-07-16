# Money and Success in Football
### Does Financial Strength Predict Points Per Game?

Seminar project, Department of Statistics, TU Dortmund University, Summer Semester 2026.
Data science component by Md. Muktadir; journalistic article by Linus Mainka.

## What this project does

This project tests whether a club's financial strength predicts its sporting success across
Europe's five major football leagues (Bundesliga, La Liga, Ligue 1, Premier League, Serie A)
over ten seasons (2015-16 through 2024-25), covering 976 team-seasons.

Financial strength is measured as **Financial Rank (FR)**: each squad's market value rank
within its own league and season (FR 1 = wealthiest squad that season). Ranking within
league-seasons makes the five leagues directly comparable and makes the measure robust to
transfer market inflation over the decade.

Performance is measured as **points per game (PPG)**: total league points divided by matches
played (34 for the Bundesliga, 38 for the other four leagues), so that the two league formats
are on the same scale.

The primary model is a **generalized additive model (GAM)**: PPG explained by a smooth
function of Financial Rank, fitted with [mgcv](https://cran.r-project.org/package=mgcv)
using REML smoothing parameter selection. Headline result: deviance explained = 0.707,
AIC = 75.32, edf = 6.30. Leicester City 2015-16 and Bayer Leverkusen 2023-24 are the two
standout overperformers under every specification tested.

The full written report is `09_final_report_v2.Rmd` (rendered PDF included in the submission).

## Data

- **Standings:** ESPN FC final league tables (points, position), 2015-16 to 2024-25.
- **Market values:** Transfermarkt squad values via a pre-compiled Kaggle dataset,
  spot-checked against the live Transfermarkt site.
- **Working dataset:** `data/merged/master_dataset_corrected.csv` (CSV, self-explanatory
  column names). This corrected version fixes one missing value: SPAL 2017-18 (Serie A)
  had no market value in the Kaggle source due to a merge failure; the correct value
  (EUR 63.55M, verified on Transfermarkt) was inserted by
  `extensions/scripts/15_spal_correction.R`, raising N from 975 to 976. The original
  `data/merged/master_dataset.csv` is preserved untouched.
- **NBA comparison data:** payroll ranks and final standings for the cross-sport
  robustness check (see report Section 7).

## Repository structure

```
RScripts/            Main pipeline, scripts 01-09 (run in numeric order)
extensions/scripts/  Post-presentation extensions, scripts 13-16
extensions/outputs/  Extension outputs (E1-E6, gam_revision/)
data/merged/         Merged datasets (original and corrected)
outputs/             Main pipeline outputs (figures, tables)
utils/               Shared helpers: packages.R, constants.R, helpers.R
```

### Main pipeline (`RScripts/`)

| Script | Purpose |
|---|---|
| `01_scrape_ESPN_standings.R` | Scrapes final league standings and points from the ESPN standings API, all five leagues, 2015-16 to 2024-25 |
| `02_scrape_transfermarkt_market_values.R` | Scrapes Transfermarkt squad market values for the same leagues and seasons |
| `03_compute_financial_rank.R` | Recomputes Financial Rank from the raw market value data, with proper tie handling |
| `04_merge_standings_and_market_values.R` | Merges ESPN standings and Transfermarkt market values into the master dataset |
| `05_analysis_and_regression.R` | Exploratory analysis and the baseline regression models (linear and early quadratic specifications) |
| `05b_extended_regression.R` | Extended regression diagnostics: heteroskedasticity checks, interaction and quadratic (Model H) specifications |
| `05c_model_h4_extended.R` | Extends Model H with league fixed effects and a season index |
| `06_residuals_and_outliers.R` | Residual analysis and outlier visualisations for Model H |
| `07_beautiful_charts.R` | Presentation-ready versions of the residual and outlier charts |
| `08_time_split_model.R` | Splits the decade into Early/Late halves and tests whether the Financial Rank slope is stable over time |
| `09_final_report_v2.Rmd` | Final report: PPG + GAM as primary specification |

### Extensions (`extensions/scripts/`)

| Script | Purpose |
|---|---|
| `13_model_evolution_comparison.R` | Traces the full model evolution from the linear baseline (Model A) to the PPG quadratic Model H, and validates that Financial Rank is robust to the transfer-market inflation adjustment |
| `14_additional_visuals.R` | Three follow-up visuals: a faceted inflation rank-stability check, a standardised effect-size comparison across models, and PPG residuals-vs-fitted plot |
| `15_spal_correction.R` | SPAL 2017-18 market value fix; writes `master_dataset_corrected.csv` |
| `16_linear_gam_revision.R` | Linear and GAM PPG models, model comparison table, fitted-smooth figure, residual diagnostics |

## How to reproduce

1. Install R (>= 4.5). Required packages are installed automatically by
   `utils/packages.R` and the `ensure_installed()` helper inside the scripts.
2. Run the main pipeline scripts in `RScripts/` in numeric order.
3. Run `extensions/scripts/15_spal_correction.R` to produce the corrected dataset,
   then `16_linear_gam_revision.R` for the GAM revision outputs.
4. Knit `09_final_report_v2.Rmd` to PDF. All inline statistics are computed at knit
   time from `master_dataset_corrected.csv`, so the report always reflects the data.

All scripts are commented to the standard set by the course instructions: enough that
the analysis is understandable twelve months from now without external context.

## Key packages

- [mgcv](https://cran.r-project.org/package=mgcv) - generalized additive models (Wood, 2017)
- [ggplot2](https://ggplot2.tidyverse.org/) - all visualisations
- [dplyr](https://dplyr.tidyverse.org/) / [tidyr](https://tidyr.tidyverse.org/) - data wrangling
- [broom](https://broom.tidymodels.org/) - tidy model output
- [rvest](https://rvest.tidyverse.org/) / [httr](https://httr.r-lib.org/) - data collection
- [kableExtra](https://cran.r-project.org/package=kableExtra) - report tables
- [sandwich](https://cran.r-project.org/package=sandwich) / [lmtest](https://cran.r-project.org/package=lmtest) - robust standard errors (position-based robustness checks)

## Contact

Md. Muktadir - mdmuktadir0611@gmail.com
