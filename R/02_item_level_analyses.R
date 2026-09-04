# Exploratory item-level analyses -------------------------------------------
#
# Separate BH families are used for the 27 vocal-symptom contrasts and the
# 108 SVHI item contrasts. The SVHI questionnaire wording is not reproduced in
# this public script; items are identified by their scale numbers.

source(file.path("R", "00_utils.R"))
require_packages(c("sandwich", "emmeans", "car"))

input_file <- project_path("data", "derived", "data_analysis_final.rds")
output_dir <- project_path("outputs", "analysis")
ensure_directory(output_dir)

if (!file.exists(input_file)) {
  stop("Prepared analytic dataset not found. Run R/00_prepare_data.R first.")
}

data_analysis <- prepare_model_factors(readRDS(input_file))

fit_item_model <- function(variable, label) {
  model <- stats::lm(analysis_formula(variable), data = data_analysis)
  robust_vcov <- sandwich::vcovHC(model, type = "HC3")
  means <- emmeans::emmeans(model, ~ vocalist_group, vcov. = robust_vcov)
  contrasts <- emmeans::contrast(
    means,
    method = planned_contrasts,
    adjust = "none"
  )
  group_coefficients <- grep(
    "^vocalist_group",
    names(stats::coef(model)),
    value = TRUE
  )
  global_test <- car::linearHypothesis(
    model,
    group_coefficients,
    vcov. = robust_vcov,
    test = "F"
  )

  contrast_table <- as.data.frame(
    summary(contrasts, infer = c(TRUE, TRUE), level = 0.95)
  )
  contrast_table$variable <- variable
  contrast_table$label <- label
  contrast_table$n <- stats::nobs(model)

  means_table <- as.data.frame(summary(means, infer = c(TRUE, TRUE)))
  means_table$variable <- variable
  means_table$label <- label
  means_table$n <- stats::nobs(model)

  list(
    contrasts = contrast_table,
    means = means_table,
    global = data.frame(
      variable = variable,
      label = label,
      numerator_df = unname(global_test[2, "Df"]),
      denominator_df = unname(global_test[2, "Res.Df"]),
      F = unname(global_test[2, "F"]),
      p_value = unname(global_test[2, "Pr(>F)"])
    )
  )
}

combine_item_results <- function(variables, labels) {
  fitted <- Map(fit_item_model, variables, unname(labels[variables]))

  list(
    contrasts = do.call(rbind, lapply(fitted, `[[`, "contrasts")),
    means = do.call(rbind, lapply(fitted, `[[`, "means")),
    global = do.call(rbind, lapply(fitted, `[[`, "global"))
  )
}

# Vocal-symptom items: 9 items x 3 contrasts = 27 tests ---------------------

symptom_variables <- c(
  "symptom_vocal_fatigue",
  "symptom_throat_clearing",
  "symptom_dryness",
  "symptom_hoarseness",
  "symptom_dry_cough",
  "symptom_wet_cough",
  "symptom_lump_throat",
  "symptom_voice_loss",
  "symptom_laryngeal_pain"
)
symptom_labels <- c(
  symptom_vocal_fatigue = "Vocal fatigue after voice use",
  symptom_throat_clearing = "Throat clearing",
  symptom_dryness = "Voice breaks",
  symptom_hoarseness = "Hoarseness",
  symptom_dry_cough = "Dry cough",
  symptom_wet_cough = "Wet cough",
  symptom_lump_throat = "Shortness of breath",
  symptom_voice_loss = "Episodes of voice loss/aphonia",
  symptom_laryngeal_pain = "Effortful speaking"
)
assert_columns(data_analysis, symptom_variables, "analytic dataset")

symptom_results <- combine_item_results(symptom_variables, symptom_labels)
symptom_contrasts <- symptom_results$contrasts
symptom_contrasts$q_value_BH <- stats::p.adjust(
  symptom_contrasts$p.value,
  method = "BH"
)
symptom_contrasts$fdr_significant <- symptom_contrasts$q_value_BH < 0.05
symptom_contrasts$item_order <- match(symptom_contrasts$variable, symptom_variables)
symptom_contrasts$contrast_order <- match(
  symptom_contrasts$contrast,
  names(planned_contrasts)
)
symptom_contrasts <- symptom_contrasts[
  order(symptom_contrasts$item_order, symptom_contrasts$contrast_order),
]
row.names(symptom_contrasts) <- NULL
stopifnot(nrow(symptom_contrasts) == 27L)

symptom_means <- symptom_results$means
symptom_means$item_order <- match(symptom_means$variable, symptom_variables)
symptom_means <- symptom_means[order(symptom_means$item_order), ]
row.names(symptom_means) <- NULL

symptom_global <- symptom_results$global
symptom_global$q_value_BH <- stats::p.adjust(symptom_global$p_value, method = "BH")

significant_symptoms <- symptom_contrasts[
  symptom_contrasts$fdr_significant,
  c("label", "contrast")
]
expected_significant_symptoms <- data.frame(
  label = rep("Vocal fatigue after voice use", 2),
  contrast = c("Current vs Non-wedding", "Current vs Former")
)
if (!setequal(
  paste(significant_symptoms$label, significant_symptoms$contrast),
  paste(expected_significant_symptoms$label, expected_significant_symptoms$contrast)
)) {
  stop("Unexpected FDR-significant vocal-symptom item results.")
}

# SVHI items: 36 items x 3 contrasts = 108 tests ----------------------------

svhi_variables <- paste0("svhi_item_", 1:36)
svhi_labels <- setNames(paste("SVHI item", 1:36), svhi_variables)
assert_columns(data_analysis, svhi_variables, "analytic dataset")

svhi_results <- combine_item_results(svhi_variables, svhi_labels)
svhi_contrasts <- svhi_results$contrasts
svhi_contrasts$item_number <- as.integer(sub("svhi_item_", "", svhi_contrasts$variable))
svhi_contrasts$q_value_BH <- stats::p.adjust(
  svhi_contrasts$p.value,
  method = "BH"
)
svhi_contrasts$fdr_significant <- svhi_contrasts$q_value_BH < 0.05
svhi_contrasts$contrast_order <- match(
  svhi_contrasts$contrast,
  names(planned_contrasts)
)
svhi_contrasts <- svhi_contrasts[
  order(svhi_contrasts$item_number, svhi_contrasts$contrast_order),
]
row.names(svhi_contrasts) <- NULL
stopifnot(nrow(svhi_contrasts) == 108L)

svhi_means <- svhi_results$means
svhi_means$item_number <- as.integer(sub("svhi_item_", "", svhi_means$variable))
svhi_means <- svhi_means[order(svhi_means$item_number), ]
row.names(svhi_means) <- NULL

svhi_global <- svhi_results$global
svhi_global$item_number <- as.integer(sub("svhi_item_", "", svhi_global$variable))
svhi_global$q_value_BH <- stats::p.adjust(svhi_global$p_value, method = "BH")
svhi_global <- svhi_global[order(svhi_global$item_number), ]

# Aggregate, non-identifiable exports ----------------------------------------

contrast_columns <- c(
  "variable", "label", "contrast", "estimate", "SE", "df",
  "lower.CL", "upper.CL", "t.ratio", "p.value", "q_value_BH",
  "fdr_significant", "n"
)
write_csv(
  symptom_contrasts[contrast_columns],
  file.path(output_dir, "vocal_symptom_item_contrasts.csv")
)
write_csv(
  symptom_means[, c(
    "variable", "label", "vocalist_group", "emmean", "SE", "df",
    "lower.CL", "upper.CL", "n"
  )],
  file.path(output_dir, "vocal_symptom_adjusted_means.csv")
)
write_csv(
  symptom_global,
  file.path(output_dir, "vocal_symptom_item_omnibus.csv")
)
write_csv(
  svhi_contrasts[, c("item_number", contrast_columns)],
  file.path(output_dir, "svhi_item_contrasts.csv")
)
write_csv(
  svhi_means[, c(
    "item_number", "variable", "label", "vocalist_group", "emmean", "SE",
    "df", "lower.CL", "upper.CL", "n"
  )],
  file.path(output_dir, "svhi_item_adjusted_means.csv")
)
write_csv(
  svhi_global,
  file.path(output_dir, "svhi_item_omnibus.csv")
)

cat("Item-level analyses completed successfully.\n")

