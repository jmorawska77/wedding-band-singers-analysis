# Exploratory voice-care and singing-training analyses ----------------------
#
# Nine variables enter the omnibus family. Adjusted logistic contrasts are
# limited to five unambiguous binary indicators (5 x 3 = 15 tests). The full
# three-category voice-examination variable is not collapsed because routine
# examinations and examinations prompted by a problem are distinct care paths.

source(file.path("R", "00_utils.R"))
require_packages(c("sandwich", "emmeans"))

input_file <- project_path("data", "derived", "data_analysis_final.rds")
output_dir <- project_path("outputs", "analysis")
ensure_directory(output_dir)

if (!file.exists(input_file)) {
  stop("Prepared analytic dataset not found. Run R/00_prepare_data.R first.")
}

data_analysis <- prepare_model_factors(readRDS(input_file))

voice_variables <- c(
  "singing_classes_f",
  "individual_singing_lessons_f",
  "individual_lessons_regularity_f",
  "technique_knowledge_f",
  "regular_voice_examination_f",
  "voice_examiner_type_f",
  "last_ent_visit_f",
  "last_phoniatrician_visit_f",
  "otc_voice_medication_f"
)
voice_labels <- c(
  singing_classes_f = "Participation in singing classes",
  individual_singing_lessons_f = "Individual singing lessons",
  individual_lessons_regularity_f = "Regularity of individual singing lessons",
  technique_knowledge_f = "Self-reported knowledge of vocal technique",
  regular_voice_examination_f = "Regularity of voice examinations",
  voice_examiner_type_f = "Professional performing the voice examination",
  last_ent_visit_f = "Time since last ENT visit",
  last_phoniatrician_visit_f = "Time since last phoniatrician visit",
  otc_voice_medication_f = "Use of over-the-counter voice products or medications"
)
assert_columns(
  data_analysis,
  c(
    "vocalist_group", voice_variables, "age", "gender_f",
    "vocal_career_duration_model", "formal_musical_education_model", "smoking_f"
  ),
  "analytic dataset"
)

omnibus_test <- function(variable) {
  complete <- stats::complete.cases(data_analysis[c("vocalist_group", variable)])
  table_data <- table(
    data_analysis$vocalist_group[complete],
    data_analysis[[variable]][complete]
  )
  chi_square <- suppressWarnings(stats::chisq.test(table_data, correct = FALSE))

  if (min(chi_square$expected) < 5) {
    result <- stats::fisher.test(
      table_data,
      simulate.p.value = TRUE,
      B = 20000
    )
    data.frame(
      variable = variable,
      N = sum(table_data),
      test = "Fisher exact test (Monte Carlo)",
      statistic = NA_real_,
      df = NA_real_,
      p_value = result$p.value
    )
  } else {
    data.frame(
      variable = variable,
      N = sum(table_data),
      test = "Pearson chi-square",
      statistic = unname(chi_square$statistic),
      df = unname(chi_square$parameter),
      p_value = chi_square$p.value
    )
  }
}

set.seed(20260903)
omnibus_results <- do.call(rbind, lapply(voice_variables, omnibus_test))
omnibus_results$q_value_BH <- stats::p.adjust(
  omnibus_results$p_value,
  method = "BH"
)
omnibus_results$characteristic <- unname(voice_labels[omnibus_results$variable])
omnibus_results <- omnibus_results[, c(
  "variable", "characteristic", "N", "test", "statistic", "df",
  "p_value", "q_value_BH"
)]
stopifnot(nrow(omnibus_results) == 9L)

describe_categorical <- function(variable) {
  complete <- stats::complete.cases(data_analysis[c("vocalist_group", variable)])
  table_data <- table(
    data_analysis[[variable]][complete],
    data_analysis$vocalist_group[complete]
  )
  denominators <- colSums(table_data)
  result <- data.frame(
    variable = variable,
    category = row.names(table_data),
    check.names = FALSE
  )

  for (group in colnames(table_data)) {
    result[[group]] <- paste0(
      table_data[, group], "/", denominators[group], " (",
      sprintf("%.1f", 100 * table_data[, group] / denominators[group]), "%)"
    )
  }
  result
}

voice_descriptives <- do.call(
  rbind,
  lapply(voice_variables, describe_categorical)
)
row.names(voice_descriptives) <- NULL

# Five binary indicators for adjusted logistic contrasts --------------------

make_binary <- function(x, positive) {
  ifelse(is.na(x), NA_integer_, as.integer(x %in% positive))
}

binary_definitions <- list(
  singing_classes_binary = list(
    source = "singing_classes_f",
    positive = "Yes",
    label = "Participation in singing classes"
  ),
  individual_lessons_binary = list(
    source = "individual_singing_lessons_f",
    positive = "Yes",
    label = "Individual singing lessons"
  ),
  ent_last_year_binary = list(
    source = "last_ent_visit_f",
    positive = "In the last year",
    label = "ENT visit in the last year"
  ),
  phoniatrician_last_year_binary = list(
    source = "last_phoniatrician_visit_f",
    positive = "In the last year",
    label = "Phoniatrician visit in the last year"
  ),
  otc_voice_medication_binary = list(
    source = "otc_voice_medication_f",
    positive = "Yes",
    label = "Use of OTC voice products or medications"
  )
)

pairwise_data <- data_analysis
for (binary_name in names(binary_definitions)) {
  definition <- binary_definitions[[binary_name]]
  pairwise_data[[binary_name]] <- make_binary(
    pairwise_data[[definition$source]],
    definition$positive
  )
}

voice_pairwise_contrasts <- list(
  "Current vs non-wedding" = c(-1, 1, 0),
  "Current vs former" = c(0, 1, -1),
  "Former vs non-wedding" = c(-1, 0, 1)
)

fit_binary_indicator <- function(outcome, label) {
  model <- stats::glm(
    analysis_formula(outcome),
    data = pairwise_data,
    family = stats::binomial()
  )
  means <- emmeans::emmeans(
    model,
    ~ vocalist_group,
    vcov. = sandwich::vcovHC(model, type = "HC3")
  )
  contrasts <- emmeans::contrast(
    means,
    method = voice_pairwise_contrasts,
    adjust = "none"
  )
  result <- as.data.frame(
    summary(contrasts, infer = c(TRUE, TRUE), type = "response")
  )

  data.frame(
    outcome = label,
    contrast = result$contrast,
    OR = result$odds.ratio,
    lower_CI = result$asymp.LCL,
    upper_CI = result$asymp.UCL,
    p_value = result$p.value
  )
}

pairwise_results <- do.call(
  rbind,
  lapply(names(binary_definitions), function(binary_name) {
    fit_binary_indicator(
      binary_name,
      binary_definitions[[binary_name]]$label
    )
  })
)
pairwise_results$q_value_BH <- stats::p.adjust(
  pairwise_results$p_value,
  method = "BH"
)
row.names(pairwise_results) <- NULL
stopifnot(nrow(pairwise_results) == 15L)

individual_primary <- pairwise_results[
  pairwise_results$outcome == "Individual singing lessons" &
    pairwise_results$contrast == "Current vs non-wedding",
]
stopifnot(
  nrow(individual_primary) == 1L,
  identical(round(individual_primary$OR, 2), 0.23),
  identical(round(individual_primary$q_value_BH, 3), 0.008)
)

format_contrast_line <- function(row) {
  p_text <- if (row$p_value < 0.001) {
    "P < .001"
  } else {
    paste0("P = ", sub("^0", "", sprintf("%.3f", row$p_value)))
  }
  q_text <- sub("^0", "", sprintf("%.3f", row$q_value_BH))
  sprintf(
    "%s: OR %.2f (95%% CI %.2f to %.2f), %s, BH q = %s",
    row$contrast,
    row$OR,
    row$lower_CI,
    row$upper_CI,
    p_text,
    q_text
  )
}

pairwise_text <- vapply(
  unique(pairwise_results$outcome),
  function(outcome) {
    rows <- pairwise_results[pairwise_results$outcome == outcome, ]
    paste(vapply(seq_len(nrow(rows)), function(i) {
      format_contrast_line(rows[i, ])
    }, character(1)), collapse = "\n")
  },
  character(1)
)

table_data <- voice_descriptives
table_data$Characteristic <- unname(voice_labels[table_data$variable])
omnibus_position <- match(table_data$variable, omnibus_results$variable)
table_data$`Omnibus P value` <- omnibus_results$p_value[omnibus_position]
table_data$`BH q value` <- omnibus_results$q_value_BH[omnibus_position]
table_data$VariableID <- table_data$Characteristic

table_data$`Adjusted pairwise contrasts` <- ""
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "singing_classes_f"
] <- pairwise_text[["Participation in singing classes"]]
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "individual_singing_lessons_f"
] <- pairwise_text[["Individual singing lessons"]]
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "last_ent_visit_f"
] <- pairwise_text[["ENT visit in the last year"]]
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "last_phoniatrician_visit_f"
] <- pairwise_text[["Phoniatrician visit in the last year"]]
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "otc_voice_medication_f"
] <- pairwise_text[["Use of OTC voice products or medications"]]
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "individual_lessons_regularity_f"
] <- "Not modeled: conditional on reporting individual singing lessons."
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "technique_knowledge_f"
] <- paste(
  "Not modeled: retained as a multicategorical variable with no unambiguous",
  "binary coding."
)
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "regular_voice_examination_f"
] <- paste(
  "Not modeled: retained as a three-category variable because regular",
  "examinations and examinations prompted by voice problems represent",
  "distinct care patterns."
)
table_data$`Adjusted pairwise contrasts`[
  table_data$variable == "voice_examiner_type_f"
] <- paste(
  "Not modeled: conditional on undergoing a voice examination and",
  "multicategorical."
)

table_data <- table_data[, c(
  "Characteristic",
  "category",
  "Non-wedding CCM singers",
  "Current wedding-band singers",
  "Former wedding-band singers",
  "Omnibus P value",
  "BH q value",
  "VariableID",
  "Adjusted pairwise contrasts"
)]
names(table_data)[2] <- "Category"
stopifnot(nrow(table_data) == 30L)

# Data for Supplementary Figure S2: five selected binary indicators ----------

figure_order <- c(
  "ENT visit in the last year",
  "Phoniatrician visit in the last year",
  "OTC voice products or medications",
  "Individual singing lessons",
  "Participation in singing classes"
)
figure_groups <- c(
  "Non-wedding CCM singers" = "Non-wedding",
  "Current wedding-band singers" = "Current wedding-band",
  "Former wedding-band singers" = "Former wedding-band"
)
figure_definition_names <- c(
  "ent_last_year_binary",
  "phoniatrician_last_year_binary",
  "otc_voice_medication_binary",
  "individual_lessons_binary",
  "singing_classes_binary"
)
figure_labels <- c(
  ent_last_year_binary = "ENT visit in the last year",
  phoniatrician_last_year_binary = "Phoniatrician visit in the last year",
  otc_voice_medication_binary = "OTC voice products or medications",
  individual_lessons_binary = "Individual singing lessons",
  singing_classes_binary = "Participation in singing classes"
)

figure_rows <- lapply(figure_definition_names, function(binary_name) {
  definition <- binary_definitions[[binary_name]]
  percentages <- tapply(
    pairwise_data[[binary_name]],
    pairwise_data$vocalist_group,
    mean,
    na.rm = TRUE
  ) * 100
  data.frame(
    Characteristic = unname(figure_labels[binary_name]),
    Group = unname(figure_groups[names(percentages)]),
    Percentage = as.numeric(percentages)
  )
})
figure_data <- do.call(rbind, figure_rows)
row.names(figure_data) <- NULL
stopifnot(nrow(figure_data) == 15L)

# Aggregate, non-identifiable exports ----------------------------------------

write_csv(
  omnibus_results,
  file.path(output_dir, "voice_care_omnibus_9_tests.csv")
)
write_csv(
  pairwise_results,
  file.path(output_dir, "voice_care_pairwise_15_tests.csv")
)
write_csv(
  table_data,
  file.path(output_dir, "Supplementary_Table_S4_data.csv")
)
write_csv(
  figure_data,
  file.path(output_dir, "Supplementary_Figure_S2_data.csv")
)

cat("Voice-care and singing-training analyses completed successfully.\n")
