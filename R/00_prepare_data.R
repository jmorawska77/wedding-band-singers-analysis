# Prepare the final analytic dataset -----------------------------------------
#
# The source workbook is private and is not distributed with this repository.
# This script assigns analytic variable names, applies three corrections that
# were verified against the source records, reconstructs composite scores, and
# creates labelled factors used by all subsequent analyses.

source(file.path("R", "00_utils.R"))
require_packages("readxl")

input_file <- project_path("data", "private", "wedding_band_singers_data.xlsx")
output_file <- project_path("data", "derived", "data_analysis_final.rds")

if (!file.exists(input_file)) {
  stop(
    "Private source workbook not found. See data/private/README.md. Expected: ",
    input_file
  )
}

data_raw <- as.data.frame(readxl::read_excel(input_file))

if (nrow(data_raw) != 412L || ncol(data_raw) != 78L) {
  stop(
    "Unexpected source-data dimensions. Expected 412 rows and 78 columns; found ",
    nrow(data_raw), " rows and ", ncol(data_raw), " columns."
  )
}

analytic_names <- c(
  "subject_code",
  "gender",
  "vocalist_type",
  "date_of_birth",
  "date_of_examination",
  "age",
  "residence",
  "general_education",
  "occupational_vocal_load",
  "vocal_career_duration",
  "formal_musical_education",
  "singing_income_source",
  "daily_singing_time",
  "wedding_singing_duration",
  "typical_event_duration",
  "singing_classes",
  "individual_singing_lessons",
  "individual_lessons_regularity",
  "technique_knowledge",
  "regular_voice_examination",
  "voice_examiner_type",
  "last_ent_visit",
  "last_phoniatrician_visit",
  "otc_voice_medication",
  "smoking",
  "smoking_years",
  "cigarettes_per_day",
  "alcohol",
  "symptom_hoarseness",
  "symptom_dry_cough",
  "symptom_wet_cough",
  "symptom_voice_loss",
  "symptom_lump_throat",
  "symptom_throat_clearing",
  "symptom_dryness",
  "symptom_vocal_fatigue",
  "symptom_laryngeal_pain",
  "vocal_symptom_burden_excel",
  paste0("svhi_item_", 1:36),
  "svhi_total_excel",
  "spoken_voice_vas",
  "singing_voice_vas",
  "months_since_last_wedding"
)

stopifnot(length(analytic_names) == 78L)
names(data_raw) <- analytic_names
data_analysis <- data_raw

if (anyDuplicated(data_analysis$subject_code)) {
  stop("Duplicate subject codes detected.")
}

# Corrections verified against the source records ---------------------------

set_verified_value <- function(data, subject, variable, accepted_before, value) {
  row_index <- which(data$subject_code == subject)
  if (length(row_index) != 1L) {
    stop("Could not identify exactly one record for a verified correction.")
  }

  observed <- data[[variable]][row_index]
  accepted <- any(
    (is.na(observed) & is.na(accepted_before)) |
      (!is.na(observed) & !is.na(accepted_before) & observed == accepted_before)
  ) || identical(as.numeric(observed), as.numeric(value))

  if (!accepted) {
    stop("Unexpected source value encountered while applying a verified correction.")
  }

  data[[variable]][row_index] <- value
  data
}

data_analysis <- set_verified_value(
  data_analysis, "S279", "months_since_last_wedding", NA_real_, NA_real_
)
data_analysis <- set_verified_value(
  data_analysis, "S379", "months_since_last_wedding", NA_real_, 25
)
data_analysis <- set_verified_value(
  data_analysis, "S233", "svhi_item_17", 13, 2
)

# Composite scores and range checks -----------------------------------------

symptom_variables <- c(
  "symptom_hoarseness",
  "symptom_dry_cough",
  "symptom_wet_cough",
  "symptom_voice_loss",
  "symptom_lump_throat",
  "symptom_throat_clearing",
  "symptom_dryness",
  "symptom_vocal_fatigue",
  "symptom_laryngeal_pain"
)
svhi_variables <- paste0("svhi_item_", 1:36)

if (any(vapply(data_analysis[symptom_variables], function(x) {
  any(is.na(x) | x < 0 | x > 4)
}, logical(1)))) {
  stop("A vocal-symptom item is missing or outside the expected 0-4 range.")
}

if (any(vapply(data_analysis[svhi_variables], function(x) {
  any(is.na(x) | x < 0 | x > 4)
}, logical(1)))) {
  stop("An SVHI item is missing or outside the expected 0-4 range.")
}

data_analysis$Vocal_Symptoms <- rowSums(data_analysis[symptom_variables])
data_analysis$SVHI_total <- rowSums(data_analysis[svhi_variables])

# Labelled variables ---------------------------------------------------------

data_analysis$vocalist_group <- factor(
  data_analysis$vocalist_type,
  levels = c(0, 1, 2),
  labels = c(
    "Non-wedding CCM singers",
    "Current wedding-band singers",
    "Former wedding-band singers"
  )
)
data_analysis$gender_f <- factor(
  data_analysis$gender,
  levels = c(0, 1),
  labels = c("Female", "Male")
)
data_analysis$residence_f <- factor(
  data_analysis$residence,
  levels = 0:4,
  labels = c(
    "Village", "Up to 40,000", "50,000-150,000",
    "150,000-500,000", "Over 500,000"
  )
)
data_analysis$general_education_f <- factor(
  data_analysis$general_education,
  levels = 0:4,
  labels = c("Primary", "Lower secondary", "Vocational", "Secondary", "Higher")
)
data_analysis$occupational_vocal_load_f <- factor(
  data_analysis$occupational_vocal_load,
  levels = 0:2,
  labels = c(
    "No occupational vocal load",
    "Professional spoken voice use",
    "Professional singing"
  )
)
data_analysis$vocal_career_duration_f <- factor(
  data_analysis$vocal_career_duration,
  levels = 0:4,
  labels = c("<1 year", "1-2 years", "2-5 years", "5-10 years", ">10 years")
)
data_analysis$formal_musical_education_f <- factor(
  data_analysis$formal_musical_education,
  levels = 0:3,
  labels = c("None", "First degree", "Second degree", "Higher")
)
data_analysis$singing_income_source_f <- factor(
  data_analysis$singing_income_source,
  levels = 0:3,
  labels = c("Basic income source", "Additional income source", "Hobby", "Aspiring")
)
data_analysis$wedding_singing_duration_f <- factor(
  data_analysis$wedding_singing_duration,
  levels = 0:3,
  labels = c("1-2 years", "2-5 years", "5-10 years", ">10 years")
)
data_analysis$typical_event_duration_f <- factor(
  data_analysis$typical_event_duration,
  levels = 0:5,
  labels = c("1-2 h", "2-4 h", "4-6 h", "6-8 h", "8-10 h", ">10 h")
)
data_analysis$singing_classes_f <- factor(
  data_analysis$singing_classes,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
data_analysis$individual_singing_lessons_f <- factor(
  data_analysis$individual_singing_lessons,
  levels = c(0, 1),
  labels = c("No", "Yes")
)
data_analysis$individual_lessons_regularity_f <- factor(
  data_analysis$individual_lessons_regularity,
  levels = c(0, 1),
  labels = c("Single meetings", "Regular")
)
data_analysis$technique_knowledge_f <- factor(
  data_analysis$technique_knowledge,
  levels = 0:3,
  labels = c(
    "Enough knowledge",
    "Enough, but eager to learn more",
    "No, singing intuitively",
    "No, but eager to learn more"
  )
)
data_analysis$regular_voice_examination_f <- factor(
  data_analysis$regular_voice_examination,
  levels = 0:2,
  labels = c("Yes", "No", "Only when problems")
)
data_analysis$voice_examiner_type_f <- factor(
  data_analysis$voice_examiner_type,
  levels = 0:2,
  labels = c("GP", "ENT", "Phoniatrician")
)
visit_labels <- c(
  "In the last year", "2 years ago", "3 years ago",
  "4 years ago", ">5 years ago", "Never/cannot remember"
)
data_analysis$last_ent_visit_f <- factor(
  data_analysis$last_ent_visit,
  levels = 0:5,
  labels = visit_labels
)
data_analysis$last_phoniatrician_visit_f <- factor(
  data_analysis$last_phoniatrician_visit,
  levels = 0:5,
  labels = visit_labels
)
data_analysis$otc_voice_medication_f <- factor(
  data_analysis$otc_voice_medication,
  levels = c(0, 1),
  labels = c("Yes", "No")
)
data_analysis$smoking_f <- factor(
  data_analysis$smoking,
  levels = 0:2,
  labels = c("No", "Current", "Former")
)
data_analysis$alcohol_f <- factor(
  data_analysis$alcohol,
  levels = 0:2,
  labels = c("No", "Occasionally", "Often")
)

data_analysis <- prepare_model_factors(data_analysis)

expected_group_counts <- c(328L, 44L, 40L)
observed_group_counts <- as.integer(table(data_analysis$vocalist_group))
if (!identical(observed_group_counts, expected_group_counts)) {
  stop(
    "Unexpected group counts. Expected 328/44/40; found ",
    paste(observed_group_counts, collapse = "/"), "."
  )
}

model_variables <- c(
  "SVHI_total", "Vocal_Symptoms", "spoken_voice_vas", "singing_voice_vas",
  "vocalist_group", "age", "gender_f", "vocal_career_duration_model",
  "formal_musical_education_model", "smoking_f", "daily_singing_time"
)
if (anyNA(data_analysis[model_variables])) {
  stop("Unexpected missing values in a principal-model variable.")
}

ensure_directory(dirname(output_file))
saveRDS(data_analysis, output_file)

integrity_summary <- data.frame(
  check = c(
    "Participants",
    "Non-wedding CCM singers",
    "Current wedding-band singers",
    "Former wedding-band singers",
    "Missing principal-model values",
    "SVHI item values outside 0-4",
    "Symptom item values outside 0-4"
  ),
  value = c(
    nrow(data_analysis),
    observed_group_counts,
    sum(is.na(data_analysis[model_variables])),
    0,
    0
  )
)

write_csv(
  integrity_summary,
  project_path("outputs", "analysis", "data_integrity_summary.csv")
)

cat("Prepared analytic data:", output_file, "\n")

