# Run the complete analysis and publication-output workflow.
# Open WeddingBandSingers.Rproj before sourcing this file.

scripts <- c(
  "R/00_prepare_data.R",
  "R/01_primary_analyses.R",
  "R/02_item_level_analyses.R",
  "R/03_voice_care_analyses.R",
  "R/04_validate_reference_results.R",
  "R/05_publication_outputs.R"
)

for (script in scripts) {
  message("Running ", script)
  source(script, echo = FALSE, chdir = FALSE)
}

message("All analyses and publication outputs completed successfully.")
