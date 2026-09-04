# Primary, secondary, and sensitivity analyses ------------------------------

source(file.path("R", "00_utils.R"))
require_packages(c("sandwich", "emmeans", "car", "WeightIt", "cobalt"))

input_file <- project_path("data", "derived", "data_analysis_final.rds")
output_dir <- project_path("outputs", "analysis")
ensure_directory(output_dir)

if (!file.exists(input_file)) {
  stop("Prepared analytic dataset not found. Run R/00_prepare_data.R first.")
}

data_analysis <- prepare_model_factors(readRDS(input_file))

required_variables <- c(
  "subject_code", "vocalist_group", "SVHI_total", "Vocal_Symptoms",
  "spoken_voice_vas", "singing_voice_vas", "age", "gender_f",
  "vocal_career_duration_model", "formal_musical_education_model",
  "smoking_f", "daily_singing_time"
)
assert_columns(data_analysis, required_variables, "analytic dataset")

expected_groups <- c(
  "Non-wedding CCM singers",
  "Current wedding-band singers",
  "Former wedding-band singers"
)
if (!identical(levels(data_analysis$vocalist_group), expected_groups)) {
  stop("Unexpected vocalist-group order.")
}
if (!identical(as.integer(table(data_analysis$vocalist_group)), c(328L, 44L, 40L))) {
  stop("Unexpected vocalist-group counts.")
}
if (anyNA(data_analysis[setdiff(required_variables, "subject_code")])) {
  stop("Unexpected missing values in principal-model variables.")
}

fit_hc3_outcome <- function(outcome, include_daily_singing_time = FALSE) {
  model <- stats::lm(
    analysis_formula(outcome, include_daily_singing_time),
    data = data_analysis
  )
  robust_vcov <- sandwich::vcovHC(model, type = "HC3")
  marginal_means <- emmeans::emmeans(
    model,
    ~ vocalist_group,
    vcov. = robust_vcov
  )
  contrasts <- emmeans::contrast(
    marginal_means,
    method = planned_contrasts,
    adjust = "none"
  )

  list(
    model = model,
    vcov = robust_vcov,
    means = as.data.frame(summary(marginal_means, infer = c(TRUE, TRUE))),
    contrasts = as.data.frame(
      summary(contrasts, infer = c(TRUE, TRUE), level = 0.95)
    )
  )
}

select_contrast <- function(table, label) {
  result <- table[table$contrast == label, , drop = FALSE]
  if (nrow(result) != 1L) {
    stop("Could not uniquely select contrast: ", label)
  }
  result
}

robust_group_test <- function(model) {
  coefficients <- grep("^vocalist_group", names(stats::coef(model)), value = TRUE)
  test <- car::linearHypothesis(
    model,
    coefficients,
    vcov. = sandwich::vcovHC(model, type = "HC3"),
    test = "F"
  )

  data.frame(
    numerator_df = unname(test[2, "Df"]),
    denominator_df = unname(test[2, "Res.Df"]),
    F = unname(test[2, "F"]),
    p_value = unname(test[2, "Pr(>F)"])
  )
}

outcomes <- c(
  "SVHI total" = "SVHI_total",
  "Vocal Symptoms" = "Vocal_Symptoms",
  "Spoken Voice VAS" = "spoken_voice_vas",
  "Singing Voice VAS" = "singing_voice_vas"
)

analyses <- lapply(unname(outcomes), fit_hc3_outcome)
names(analyses) <- names(outcomes)

all_contrasts <- do.call(
  rbind,
  lapply(names(analyses), function(outcome) {
    result <- analyses[[outcome]]$contrasts
    result$outcome <- outcome
    result[, c(
      "outcome", "contrast", "estimate", "SE", "df",
      "lower.CL", "upper.CL", "t.ratio", "p.value"
    )]
  })
)
row.names(all_contrasts) <- NULL

all_adjusted_means <- do.call(
  rbind,
  lapply(names(analyses), function(outcome) {
    result <- analyses[[outcome]]$means
    result$outcome <- outcome
    result[, c(
      "outcome", "vocalist_group", "emmean", "SE", "df", "lower.CL", "upper.CL"
    )]
  })
)
row.names(all_adjusted_means) <- NULL

primary_index <-
  all_contrasts$outcome == "SVHI total" &
  all_contrasts$contrast == "Current vs Non-wedding"
if (sum(primary_index) != 1L) {
  stop("The prespecified primary contrast could not be identified.")
}

all_contrasts$hierarchy <- "Secondary"
all_contrasts$hierarchy[primary_index] <- "Primary"
all_contrasts$q_value_BH <- NA_real_
all_contrasts$q_value_BH[!primary_index] <- stats::p.adjust(
  all_contrasts$p.value[!primary_index],
  method = "BH"
)
stopifnot(sum(!primary_index) == 11L)

all_contrasts$interpretation <- ifelse(
  primary_index,
  "Primary hypothesis; not multiplicity-adjusted",
  ifelse(
    all_contrasts$q_value_BH < 0.05,
    "FDR-significant",
    ifelse(
      all_contrasts$p.value < 0.05,
      "Nominal only; not FDR-significant",
      "Not statistically significant"
    )
  )
)

final_order <- c(
  "SVHI total|Current vs Non-wedding",
  "SVHI total|Current vs Former",
  "Vocal Symptoms|Current vs Non-wedding",
  "Vocal Symptoms|Current vs Former",
  "Singing Voice VAS|Current vs Former",
  "Singing Voice VAS|Current vs Non-wedding",
  "SVHI total|Former vs Non-wedding",
  "Spoken Voice VAS|Current vs Non-wedding",
  "Singing Voice VAS|Former vs Non-wedding",
  "Spoken Voice VAS|Former vs Non-wedding",
  "Vocal Symptoms|Former vs Non-wedding",
  "Spoken Voice VAS|Current vs Former"
)
result_keys <- paste(all_contrasts$outcome, all_contrasts$contrast, sep = "|")
if (!setequal(result_keys, final_order)) {
  stop("Unexpected set of primary/secondary contrasts.")
}
all_contrasts <- all_contrasts[match(final_order, result_keys), ]
row.names(all_contrasts) <- NULL

final_controlled_results <- data.frame(
  hierarchy = all_contrasts$hierarchy,
  outcome = all_contrasts$outcome,
  contrast = all_contrasts$contrast,
  estimate = all_contrasts$estimate,
  SE = all_contrasts$SE,
  df = all_contrasts$df,
  lower_95_CI = all_contrasts$lower.CL,
  upper_95_CI = all_contrasts$upper.CL,
  p_value = all_contrasts$p.value,
  q_value_BH = all_contrasts$q_value_BH,
  interpretation = all_contrasts$interpretation
)

omnibus_results <- do.call(
  rbind,
  lapply(names(analyses), function(outcome) {
    cbind(outcome = outcome, robust_group_test(analyses[[outcome]]$model))
  })
)
row.names(omnibus_results) <- NULL

# Vocal-load sensitivity -----------------------------------------------------

svhi_vocal_load <- fit_hc3_outcome(
  "SVHI_total",
  include_daily_singing_time = TRUE
)
svhi_vocal_load_primary <- select_contrast(
  svhi_vocal_load$contrasts,
  "Current vs Non-wedding"
)

# Overlap-weighted sensitivity: current versus non-wedding ------------------

overlap_data <- droplevels(
  data_analysis[data_analysis$vocalist_group %in% expected_groups[1:2], ]
)
overlap_data$current_wedding <- as.integer(
  overlap_data$vocalist_group == "Current wedding-band singers"
)

set.seed(20260827)
overlap_weights <- WeightIt::weightit(
  current_wedding ~ age + gender_f + vocal_career_duration_model +
    formal_musical_education_model + smoking_f,
  data = overlap_data,
  method = "glm",
  link = "logit",
  estimand = "ATO"
)
overlap_balance_object <- cobalt::bal.tab(
  overlap_weights,
  un = TRUE,
  thresholds = c(m = 0.10),
  disp = c("means", "sds")
)
overlap_balance <- data.frame(
  covariate = row.names(overlap_balance_object$Balance),
  overlap_balance_object$Balance,
  row.names = NULL,
  check.names = FALSE
)
if (any(abs(overlap_balance_object$Balance$Diff.Adj) >= 0.10, na.rm = TRUE)) {
  warning("At least one overlap-weighted absolute SMD is >= 0.10.")
}

fit_overlap_outcome <- function(outcome) {
  model <- WeightIt::lm_weightit(
    stats::reformulate("current_wedding", response = outcome),
    data = overlap_data,
    weightit = overlap_weights,
    vcov = "asympt"
  )
  coefficient_table <- summary(model)$coefficients
  confidence_interval <- stats::confint(model, level = 0.95)

  data.frame(
    estimate = unname(stats::coef(model)["current_wedding"]),
    SE = unname(coefficient_table["current_wedding", "Std. Error"]),
    lower_95_CI = unname(confidence_interval["current_wedding", 1]),
    upper_95_CI = unname(confidence_interval["current_wedding", 2]),
    p_value = unname(coefficient_table["current_wedding", "Pr(>|z|)"])
  )
}

svhi_overlap <- fit_overlap_outcome("SVHI_total")
symptoms_overlap <- fit_overlap_outcome("Vocal_Symptoms")
svhi_primary <- select_contrast(analyses[["SVHI total"]]$contrasts, "Current vs Non-wedding")
symptoms_primary <- select_contrast(
  analyses[["Vocal Symptoms"]]$contrasts,
  "Current vs Non-wedding"
)

sensitivity_summary <- rbind(
  data.frame(
    outcome = "SVHI total",
    analysis = "Primary adjusted HC3",
    contrast = "Current vs Non-wedding",
    estimate = svhi_primary$estimate,
    SE = svhi_primary$SE,
    lower_95_CI = svhi_primary$lower.CL,
    upper_95_CI = svhi_primary$upper.CL,
    p_value = svhi_primary$p.value
  ),
  data.frame(
    outcome = "SVHI total",
    analysis = "Vocal-load sensitivity HC3",
    contrast = "Current vs Non-wedding",
    estimate = svhi_vocal_load_primary$estimate,
    SE = svhi_vocal_load_primary$SE,
    lower_95_CI = svhi_vocal_load_primary$lower.CL,
    upper_95_CI = svhi_vocal_load_primary$upper.CL,
    p_value = svhi_vocal_load_primary$p.value
  ),
  data.frame(
    outcome = "SVHI total",
    analysis = "Overlap-weighted sensitivity",
    contrast = "Current vs Non-wedding",
    svhi_overlap
  ),
  data.frame(
    outcome = "Vocal Symptoms",
    analysis = "Primary adjusted HC3",
    contrast = "Current vs Non-wedding",
    estimate = symptoms_primary$estimate,
    SE = symptoms_primary$SE,
    lower_95_CI = symptoms_primary$lower.CL,
    upper_95_CI = symptoms_primary$upper.CL,
    p_value = symptoms_primary$p.value
  ),
  data.frame(
    outcome = "Vocal Symptoms",
    analysis = "Overlap-weighted sensitivity",
    contrast = "Current vs Non-wedding",
    symptoms_overlap
  )
)

# Influence and leave-one-out checks ----------------------------------------

svhi_model <- analyses[["SVHI total"]]$model
model_n <- stats::nobs(svhi_model)
model_p <- length(stats::coef(svhi_model))
current_coefficient <- grep(
  "^vocalist_groupCurrent",
  names(stats::coef(svhi_model)),
  value = TRUE
)

influence_diagnostics <- data.frame(
  studentized_residual = stats::rstudent(svhi_model),
  cooks_distance = stats::cooks.distance(svhi_model),
  leverage = stats::hatvalues(svhi_model),
  dfbeta_current = stats::dfbetas(svhi_model)[, current_coefficient]
)
influence_summary <- data.frame(
  diagnostic = c(
    "abs_studentized_residual_gt_3",
    "cooks_distance_gt_4_over_n",
    "leverage_gt_2p_over_n",
    "abs_dfbeta_current_gt_2_over_sqrt_n"
  ),
  threshold = c(3, 4 / model_n, 2 * model_p / model_n, 2 / sqrt(model_n)),
  count = c(
    sum(abs(influence_diagnostics$studentized_residual) > 3),
    sum(influence_diagnostics$cooks_distance > 4 / model_n),
    sum(influence_diagnostics$leverage > 2 * model_p / model_n),
    sum(abs(influence_diagnostics$dfbeta_current) > 2 / sqrt(model_n))
  )
)

fit_leave_one_out <- function(index_to_remove) {
  subset_data <- data_analysis[-index_to_remove, ]
  model <- stats::lm(analysis_formula("SVHI_total"), data = subset_data)
  means <- emmeans::emmeans(
    model,
    ~ vocalist_group,
    vcov. = sandwich::vcovHC(model, type = "HC3")
  )
  contrast <- emmeans::contrast(
    means,
    method = list("Current vs Non-wedding" = c(-1, 1, 0)),
    adjust = "none"
  )
  result <- as.data.frame(summary(contrast, infer = c(TRUE, TRUE)))

  data.frame(
    estimate = result$estimate,
    lower_95_CI = result$lower.CL,
    upper_95_CI = result$upper.CL,
    p_value = result$p.value
  )
}

leave_one_out <- do.call(
  rbind,
  lapply(seq_len(nrow(data_analysis)), fit_leave_one_out)
)
leave_one_out_summary <- data.frame(
  statistic = c(
    "Minimum estimate", "Maximum estimate", "Minimum p-value",
    "Maximum p-value", "Number of estimates below zero",
    "Number of 95% CIs excluding zero"
  ),
  value = c(
    min(leave_one_out$estimate),
    max(leave_one_out$estimate),
    min(leave_one_out$p_value),
    max(leave_one_out$p_value),
    sum(leave_one_out$estimate < 0),
    sum(leave_one_out$lower_95_CI > 0)
  )
)

# Stratified nonparametric bootstrap ----------------------------------------

set.seed(20260827)
bootstrap_iterations <- 2000L
group_indices <- split(seq_len(nrow(data_analysis)), data_analysis$vocalist_group)
bootstrap_estimates <- replicate(
  bootstrap_iterations,
  {
    indices <- unlist(
      lapply(group_indices, function(x) sample(x, length(x), replace = TRUE)),
      use.names = FALSE
    )
    model <- stats::lm(
      analysis_formula("SVHI_total"),
      data = data_analysis[indices, ]
    )
    coefficient <- grep(
      "^vocalist_groupCurrent",
      names(stats::coef(model)),
      value = TRUE
    )
    unname(stats::coef(model)[coefficient])
  }
)
bootstrap_ci <- stats::quantile(
  bootstrap_estimates,
  c(0.025, 0.50, 0.975),
  na.rm = TRUE
)
bootstrap_summary <- data.frame(
  iterations = bootstrap_iterations,
  successful_iterations = sum(!is.na(bootstrap_estimates)),
  lower_2.5_percent = unname(bootstrap_ci[1]),
  median = unname(bootstrap_ci[2]),
  upper_97.5_percent = unname(bootstrap_ci[3])
)

# Fixed-sample sensitivity / minimum detectable differences -----------------

power_two_sided <- function(delta, standard_error, df, alpha = 0.05) {
  critical <- stats::qt(1 - alpha / 2, df = df)
  noncentrality <- delta / standard_error
  stats::pt(-critical, df = df, ncp = noncentrality) +
    1 - stats::pt(critical, df = df, ncp = noncentrality)
}

minimum_detectable_difference <- function(
    standard_error,
    df,
    target_power,
    alpha = 0.05) {
  objective <- function(delta) {
    power_two_sided(delta, standard_error, df, alpha) - target_power
  }
  stats::uniroot(objective, c(0, 1000))$root
}

fixed_sample_sensitivity <- final_controlled_results
fixed_sample_sensitivity$mdd_80 <- mapply(
  minimum_detectable_difference,
  standard_error = fixed_sample_sensitivity$SE,
  df = fixed_sample_sensitivity$df,
  MoreArgs = list(target_power = 0.80)
)
fixed_sample_sensitivity$mdd_90 <- mapply(
  minimum_detectable_difference,
  standard_error = fixed_sample_sensitivity$SE,
  df = fixed_sample_sensitivity$df,
  MoreArgs = list(target_power = 0.90)
)
fixed_sample_sensitivity$power_for_5_point_difference <- mapply(
  power_two_sided,
  standard_error = fixed_sample_sensitivity$SE,
  df = fixed_sample_sensitivity$df,
  MoreArgs = list(delta = 5)
)
fixed_sample_sensitivity$power_for_10_point_difference <- mapply(
  power_two_sided,
  standard_error = fixed_sample_sensitivity$SE,
  df = fixed_sample_sensitivity$df,
  MoreArgs = list(delta = 10)
)
fixed_sample_sensitivity <- fixed_sample_sensitivity[, c(
  "outcome", "contrast", "estimate", "SE", "df",
  "mdd_80", "mdd_90", "power_for_5_point_difference",
  "power_for_10_point_difference"
)]

# Checks against the rounded manuscript values ------------------------------

stopifnot(
  identical(round(svhi_primary$estimate, 2), 9.01),
  identical(round(svhi_primary$lower.CL, 2), -0.30),
  identical(round(svhi_primary$upper.CL, 2), 18.31),
  identical(round(symptoms_primary$estimate, 2), 3.60)
)

# Aggregate, non-identifiable exports ----------------------------------------

write_csv(final_controlled_results, file.path(output_dir, "main_contrasts.csv"))
write_csv(all_adjusted_means, file.path(output_dir, "adjusted_marginal_means.csv"))
write_csv(omnibus_results, file.path(output_dir, "robust_omnibus_tests.csv"))
write_csv(sensitivity_summary, file.path(output_dir, "sensitivity_summary.csv"))
write_csv(overlap_balance, file.path(output_dir, "overlap_balance.csv"))
write_csv(influence_summary, file.path(output_dir, "influence_summary.csv"))
write_csv(leave_one_out_summary, file.path(output_dir, "leave_one_out_summary.csv"))
write_csv(bootstrap_summary, file.path(output_dir, "bootstrap_summary.csv"))
write_csv(
  fixed_sample_sensitivity,
  file.path(output_dir, "fixed_sample_sensitivity.csv")
)

cat("Primary and sensitivity analyses completed successfully.\n")

