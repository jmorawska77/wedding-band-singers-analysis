# Wedding-band singers study: analysis code

This repository contains the final R workflow for the study of voice outcomes
among non-wedding contemporary commercial music (CCM) singers, current
wedding-band singers, and former wedding-band singers.

The scripts replace the sequential working notebooks used during analysis. They
use relative project paths, recreate every statistical model from a clean R
session, and generate the final aggregate tables, figures, and captions.

## Privacy and data access

The participant-level workbook and derived participant-level RDS file are not
included. Put the private workbook at:

`data/private/wedding_band_singers_data.xlsx`

Both `data/private/` and `data/derived/` are excluded by `.gitignore`. Before
publishing the repository, verify with `git status` that no participant-level
file is staged.

## Software

The final working session used R 4.6.0. Required packages are:

- `readxl`
- `sandwich`
- `emmeans`
- `car`
- `WeightIt`
- `cobalt`
- `dplyr`
- `tidyr`
- `ggplot2`
- `officer`
- `flextable`

Install missing packages manually before running the workflow:

```r
install.packages(c(
  "readxl", "sandwich", "emmeans", "car", "WeightIt", "cobalt",
  "dplyr", "tidyr", "ggplot2", "officer", "flextable"
))
```

## Reproduce the analyses

1. Open `WeddingBandSingers.Rproj` in RStudio.
2. Add the private workbook at the path shown above.
3. Restart R to obtain a clean session.
4. Run:

```r
source("run_all.R")
```

The workflow deliberately stops when data dimensions, variable ranges, group
counts, multiplicity-family sizes, or key rounded results differ from the final
analysis.

## Scripts

- `R/00_prepare_data.R`: imports and validates the private workbook, applies
  source-verified corrections, reconstructs composite scores, and creates the
  labelled analytic variables.
- `R/01_primary_analyses.R`: fits the primary and secondary HC3 models, applies
  the secondary-family BH correction, and runs vocal-load, overlap-weighting,
  influence, leave-one-out, bootstrap, and fixed-sample sensitivity analyses.
- `R/02_item_level_analyses.R`: fits the nine vocal-symptom and 36 SVHI item
  models and applies BH correction within the separate 27- and 108-test
  exploratory families.
- `R/03_voice_care_analyses.R`: presents all nine voice-care/training variables
  in the omnibus family and fits adjusted logistic models for five unambiguous
  binary indicators (15 pairwise contrasts).
- `R/04_validate_reference_results.R`: checks the newly generated voice-care
  results against the archived corrected aggregate results in
  `reference-results/`.
- `R/05_publication_outputs.R`: generates Tables 1-2, Supplementary Tables
  S1-S4, Figures 1-2, Supplementary Figures S1-S2, and their captions with the
  final manuscript numbering.

## Analysis hierarchy and multiplicity

The prespecified primary comparison is the adjusted difference in total Singing
Voice Handicap Index (SVHI) score between current wedding-band and non-wedding
CCM singers. It is not multiplicity-adjusted.

The secondary outcome family contains the remaining two SVHI contrasts plus all
three contrasts for vocal-symptom burden, spoken-voice VAS, and singing-voice
VAS (11 tests total). Benjamini-Hochberg false-discovery-rate correction is
applied across those 11 tests.

The exploratory item-level analyses use separate BH families:

- 9 vocal-symptom items x 3 contrasts = 27 tests;
- 36 SVHI items x 3 contrasts = 108 tests.

For voice care and singing training, nine original variables enter the omnibus
family. Adjusted logistic analyses cover five binary indicators x three group
contrasts = 15 tests. The three-category voice-examination variable is not
collapsed: regular examinations and examinations prompted by voice problems
are treated as distinct care patterns.

## Outputs

Aggregate, non-identifiable numerical results are written to
`outputs/analysis/`. Publication files are written to
`outputs/publication/`, including:

- Table 1 and Table 2;
- Supplementary Table S1 (sensitivity analyses);
- Supplementary Table S2 (vocal-symptom item results);
- Supplementary Table S3 (SVHI item results);
- Supplementary Table S4 (voice care and singing training);
- Figures 1-2 and Supplementary Figures S1-S2 in JPG and TIFF formats;
- figure captions and R session information.

The Word and image files are ignored by Git because they can be regenerated.
The aggregate CSV files may be retained in the public repository if permitted
by the study's data-sharing and journal policies.

## Important implementation notes

- Categorical career-duration and musical-education covariates are treated as
  nominal factors in the models.
- All principal and item-level linear models use HC3 robust covariance
  estimation.
- Fisher exact tests for sparse voice-care tables use Monte Carlo simulation
  with 20,000 replicates and a fixed seed.
- No pandemic-period, mediation, or post hoc dose-response analysis is included.
