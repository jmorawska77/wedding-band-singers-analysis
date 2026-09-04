# Validate the corrected voice-care outputs against archived aggregate results

source(file.path("R", "00_utils.R"))

analysis_dir <- project_path("outputs", "analysis")
reference_dir <- project_path("reference-results")

compare_character_columns <- function(observed, expected, columns, label) {
  for (column in columns) {
    if (!identical(as.character(observed[[column]]), as.character(expected[[column]]))) {
      stop(label, ": character column differs: ", column)
    }
  }
}

compare_numeric_columns <- function(
    observed,
    expected,
    columns,
    tolerance,
    label) {
  for (column in columns) {
    difference <- abs(as.numeric(observed[[column]]) - as.numeric(expected[[column]]))
    if (any(difference > tolerance, na.rm = TRUE)) {
      stop(label, ": numeric column differs: ", column)
    }
  }
}

# Supplementary Figure S2 data ----------------------------------------------

observed_figure <- utils::read.csv(
  file.path(analysis_dir, "Supplementary_Figure_S2_data.csv"),
  check.names = FALSE
)
expected_figure <- utils::read.csv(
  file.path(reference_dir, "Supplementary_Figure_S2_final_data.csv"),
  check.names = FALSE
)
compare_character_columns(
  observed_figure,
  expected_figure,
  c("Characteristic", "Group"),
  "Supplementary Figure S2 data"
)
compare_numeric_columns(
  observed_figure,
  expected_figure,
  "Percentage",
  0.051,
  "Supplementary Figure S2 data"
)

# Fifteen adjusted pairwise logistic contrasts ------------------------------

observed_pairwise <- utils::read.csv(
  file.path(analysis_dir, "voice_care_pairwise_15_tests.csv"),
  check.names = FALSE
)
expected_pairwise <- utils::read.csv(
  file.path(reference_dir, "voice_care_pairwise_15_tests.csv"),
  check.names = FALSE
)
names(observed_pairwise)[names(observed_pairwise) == "q_value_BH"] <- "q_value"
pairwise_key <- function(data) paste(data$outcome, data$contrast, sep = "|")
observed_pairwise <- observed_pairwise[order(pairwise_key(observed_pairwise)), ]
expected_pairwise <- expected_pairwise[order(pairwise_key(expected_pairwise)), ]
row.names(observed_pairwise) <- NULL
row.names(expected_pairwise) <- NULL
compare_character_columns(
  observed_pairwise,
  expected_pairwise,
  c("outcome", "contrast"),
  "Voice-care pairwise results"
)
compare_numeric_columns(
  observed_pairwise,
  expected_pairwise,
  c("OR", "lower_CI", "upper_CI", "p_value", "q_value"),
  1e-5,
  "Voice-care pairwise results"
)

# Complete Supplementary Table S4 data --------------------------------------

observed_table <- utils::read.csv(
  file.path(analysis_dir, "Supplementary_Table_S4_data.csv"),
  check.names = FALSE
)
expected_table <- utils::read.csv(
  file.path(reference_dir, "Supplementary_Table_S4_final_data.csv"),
  check.names = FALSE
)
character_columns <- c(
  "Characteristic",
  "Category",
  "Non-wedding CCM singers",
  "Current wedding-band singers",
  "Former wedding-band singers",
  "VariableID",
  "Adjusted pairwise contrasts"
)
compare_character_columns(
  observed_table,
  expected_table,
  character_columns,
  "Supplementary Table S4 data"
)
compare_numeric_columns(
  observed_table,
  expected_table,
  c("Omnibus P value", "BH q value"),
  5.1e-5,
  "Supplementary Table S4 data"
)

cat("Archived aggregate voice-care results reproduced successfully.\n")
