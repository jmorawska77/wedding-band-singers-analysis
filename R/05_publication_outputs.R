# Publication tables, figures, and captions ---------------------------------
#
# Final supplement order:
#   S1 sensitivity analyses
#   S2 vocal-symptom item results
#   S3 SVHI item results
#   S4 voice-care and singing-training characteristics

source(file.path("R", "00_utils.R"))
require_packages(c("dplyr", "tidyr", "ggplot2", "officer", "flextable"))

analysis_dir <- project_path("outputs", "analysis")
publication_dir <- project_path("outputs", "publication")
subdirectories <- c(
  "figures_jpg", "figures_tiff", "tables_csv", "tables_word",
  "captions", "source_tables"
)
invisible(lapply(file.path(publication_dir, subdirectories), ensure_directory))

required_files <- c(
  "main_contrasts.csv",
  "adjusted_marginal_means.csv",
  "sensitivity_summary.csv",
  "vocal_symptom_item_contrasts.csv",
  "vocal_symptom_adjusted_means.csv",
  "svhi_item_contrasts.csv",
  "Supplementary_Table_S4_data.csv",
  "Supplementary_Figure_S2_data.csv"
)
missing_files <- required_files[
  !file.exists(file.path(analysis_dir, required_files))
]
if (length(missing_files) > 0L) {
  stop(
    "Run the three analysis scripts before creating publication outputs. Missing: ",
    paste(missing_files, collapse = ", ")
  )
}

read_analysis_csv <- function(filename) {
  utils::read.csv(
    file.path(analysis_dir, filename),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

format_q <- function(x, digits = 3L) {
  ifelse(is.na(x), "Not applicable", format_p(x, digits))
}

format_fixed_without_zero <- function(x, digits) {
  ifelse(is.na(x), "", sub("^0", "", sprintf(paste0("%.", digits, "f"), x)))
}

format_estimate_ci <- function(estimate, lower, upper, digits = 2L) {
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f to %.", digits, "f)"),
    estimate,
    lower,
    upper
  )
}

make_flextable <- function(
    data,
    font_size = 8,
    widths = NULL,
    left_columns = 1L,
    padding = 3) {
  result <- flextable::flextable(data)
  result <- flextable::theme_booktabs(result)
  result <- flextable::font(result, fontname = "Arial", part = "all")
  result <- flextable::fontsize(result, size = font_size, part = "all")
  result <- flextable::fontsize(result, size = font_size, part = "header")
  result <- flextable::bold(result, bold = TRUE, part = "header")
  result <- flextable::bg(result, bg = "#E6E6E6", part = "header")
  result <- flextable::align(result, align = "center", part = "all")
  if (length(left_columns) > 0L) {
    result <- flextable::align(
      result,
      j = left_columns,
      align = "left",
      part = "body"
    )
  }
  result <- flextable::valign(result, valign = "center", part = "all")
  result <- flextable::padding(result, padding = padding, part = "all")
  result <- flextable::set_table_properties(
    result,
    layout = "fixed",
    opts_word = list(split = FALSE, repeat_headers = TRUE)
  )
  if (!is.null(widths)) {
    if (length(widths) != ncol(data)) {
      stop("The number of widths must equal the number of table columns.")
    }
    result <- flextable::width(
      result,
      j = seq_len(ncol(data)),
      width = widths,
      unit = "in"
    )
  }
  result
}

save_table_docx <- function(
    table,
    title,
    filename,
    note,
    landscape = FALSE) {
  document <- officer::read_docx()
  table <- flextable::set_caption(
    table,
    caption = title,
    word_stylename = "Normal",
    fp_p = officer::fp_par(keep_with_next = TRUE, padding = 3),
    align_with_table = FALSE
  )
  document <- flextable::body_add_flextable(document, table)
  document <- officer::body_add_par(document, note, style = "Normal")
  section <- officer::prop_section(
    page_size = officer::page_size(
      orient = if (isTRUE(landscape)) "landscape" else "portrait"
    ),
    page_margins = officer::page_mar(
      top = 0.50,
      bottom = 0.50,
      left = 0.45,
      right = 0.45,
      header = 0.25,
      footer = 0.25
    ),
    type = "continuous"
  )
  document <- officer::body_set_default_section(document, section)
  print(
    document,
    target = file.path(publication_dir, "tables_word", filename)
  )
  invisible(filename)
}

write_publication_csv <- function(data, filename) {
  write_csv(data, file.path(publication_dir, "tables_csv", filename))
}

save_figure <- function(plot, stem, width, height, tiff_dpi = 600) {
  ggplot2::ggsave(
    file.path(publication_dir, "figures_jpg", paste0(stem, ".jpg")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  ggplot2::ggsave(
    file.path(publication_dir, "figures_tiff", paste0(stem, ".tiff")),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = tiff_dpi,
    compression = "lzw",
    bg = "white"
  )
  invisible(stem)
}

charcoal <- "#222222"
dark_grey <- "#3F3F3F"
mid_grey <- "#6F6F6F"
light_grey <- "#9B9B9B"
very_light_grey <- "#F3F3F3"
group_palette <- c(
  "Non-wedding" = light_grey,
  "Current wedding-band" = dark_grey,
  "Former wedding-band" = mid_grey
)
bar_palette <- c(
  "Non-wedding" = "#BDBDBD",
  "Current wedding-band" = "#595959",
  "Former wedding-band" = "#969696"
)

theme_publication <- function(base_size = 11) {
  ggplot2::theme_classic(base_family = "Arial", base_size = base_size) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = charcoal),
      strip.background = ggplot2::element_rect(
        fill = very_light_grey,
        colour = "#B8B8B8"
      ),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        colour = "#E5E5E5",
        linewidth = 0.35
      ),
      panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(12, 18, 12, 12)
    )
}

# Table 1: characteristics of the study groups ------------------------------

data_file <- project_path("data", "derived", "data_analysis_final.rds")
if (!file.exists(data_file)) {
  stop("Prepared analytic dataset not found. Run R/00_prepare_data.R first.")
}
data_analysis <- readRDS(data_file)
groups <- levels(data_analysis$vocalist_group)

empty_table_row <- function(characteristic, category) {
  result <- data.frame(
    Characteristic = characteristic,
    Category = category,
    check.names = FALSE
  )
  for (group in groups) {
    result[[group]] <- ""
  }
  result
}

categorical_rows <- function(
    variable,
    characteristic,
    categories,
    display = categories,
    applicable_groups = groups) {
  output <- lapply(seq_along(categories), function(i) {
    result <- empty_table_row(
      if (i == 1L) characteristic else "",
      display[i]
    )
    for (group in groups) {
      if (!group %in% applicable_groups) {
        result[[group]] <- "Not applicable"
      } else {
        values <- as.character(
          data_analysis[[variable]][data_analysis$vocalist_group == group]
        )
        denominator <- sum(!is.na(values))
        numerator <- sum(values == categories[i], na.rm = TRUE)
        result[[group]] <- sprintf(
          "%d (%.1f%%)",
          numerator,
          100 * numerator / denominator
        )
      }
    }
    result
  })
  do.call(rbind, output)
}

group_size <- empty_table_row("Group size", "n")
for (group in groups) {
  group_size[[group]] <- as.character(sum(data_analysis$vocalist_group == group))
}

age_row <- empty_table_row("Age, years", "Mean (SD)")
daily_row <- empty_table_row("Daily singing-time score, 1-6", "Mean (SD)")
for (group in groups) {
  age_values <- data_analysis$age[data_analysis$vocalist_group == group]
  daily_values <- data_analysis$daily_singing_time[
    data_analysis$vocalist_group == group
  ]
  age_row[[group]] <- sprintf("%.2f (%.2f)", mean(age_values), stats::sd(age_values))
  daily_row[[group]] <- sprintf(
    "%.2f (%.2f)",
    mean(daily_values),
    stats::sd(daily_values)
  )
}

months_row <- empty_table_row(
  "Months since last wedding/event",
  "Median [IQR]"
)
months_row[[groups[1]]] <- "Not applicable"
months_row[[groups[2]]] <- "Not applicable"
months_values <- data_analysis$months_since_last_wedding[
  data_analysis$vocalist_group == groups[3]
]
months_quartiles <- stats::quantile(months_values, c(0.25, 0.50, 0.75), na.rm = TRUE)
months_row[[groups[3]]] <- sprintf(
  "%.1f [%.2f, %.1f]",
  months_quartiles[2],
  months_quartiles[1],
  months_quartiles[3]
)

table_1 <- rbind(
  group_size,
  age_row,
  categorical_rows("gender_f", "Gender", c("Male", "Female")),
  categorical_rows(
    "vocal_career_duration_f",
    "Vocal career duration",
    c(">10 years", "5-10 years", "2-5 years", "1-2 years", "<1 year")
  ),
  categorical_rows(
    "formal_musical_education_f",
    "Formal musical education",
    c("None", "First degree", "Second degree", "Higher")
  ),
  daily_row,
  categorical_rows(
    "occupational_vocal_load_f",
    "Occupational vocal load",
    c(
      "No occupational vocal load",
      "Professional spoken voice use",
      "Professional singing"
    ),
    c(
      "No professional voice use",
      "Professional speaking-voice use",
      "Professional singing"
    )
  ),
  categorical_rows(
    "singing_income_source_f",
    "Singing as income source",
    c("Additional income source", "Aspiring", "Hobby", "Basic income source"),
    c(
      "Additional source of income",
      "Future career goal",
      "Unpaid or hobby activity",
      "Main source of income"
    )
  ),
  categorical_rows(
    "smoking_f",
    "Smoking status",
    c("No", "Current", "Former"),
    c("Never", "Current", "Former")
  ),
  categorical_rows(
    "wedding_singing_duration_f",
    "Wedding-band singing duration",
    c(">10 years", "5-10 years", "2-5 years", "1-2 years"),
    applicable_groups = groups[2:3]
  ),
  categorical_rows(
    "typical_event_duration_f",
    "Typical wedding/event duration",
    c("1-2 h", "2-4 h", "4-6 h", "6-8 h", "8-10 h", ">10 h"),
    applicable_groups = groups[2:3]
  ),
  months_row
)
row.names(table_1) <- NULL
write_publication_csv(table_1, "Table_1_group_characteristics.csv")
table_1_ft <- make_flextable(
  table_1,
  font_size = 7.2,
  widths = c(1.35, 0.75, 1.45, 1.45, 1.45),
  left_columns = c(1, 2),
  padding = 2.5
)
save_table_docx(
  table_1_ft,
  "Table 1. Characteristics of the study groups",
  "Table_1_group_characteristics.docx",
  paste0(
    "Note. Values are presented as n (%), mean (SD), or median [IQR], as ",
    "indicated. CCM = contemporary commercial music."
  )
)

# Table 2: adjusted outcome contrasts ---------------------------------------

main_results <- read_analysis_csv("main_contrasts.csv")
table_2 <- main_results |>
  dplyr::transmute(
    Outcome = dplyr::recode(
      outcome,
      "SVHI total" = "Singing Voice Handicap Index total score",
      "Vocal Symptoms" = "Vocal symptom burden",
      "Singing Voice VAS" = "Singing voice self-assessment VAS",
      "Spoken Voice VAS" = "Spoken voice self-assessment VAS"
    ),
    Family = dplyr::recode(
      hierarchy,
      "Primary" = "Primary hypothesis",
      "Secondary" = "Secondary family"
    ),
    Contrast = contrast,
    `Adjusted difference (95% CI)` = format_estimate_ci(
      estimate,
      lower_95_CI,
      upper_95_CI
    ),
    `P value` = format_p(p_value),
    `BH q value` = format_q(q_value_BH)
  )
write_publication_csv(table_2, "Table_2_adjusted_outcome_contrasts.csv")
table_2_ft <- make_flextable(
  table_2,
  font_size = 7.2,
  widths = c(1.35, 0.85, 1.25, 1.45, 0.50, 0.65),
  left_columns = c(1, 2, 3)
)
save_table_docx(
  table_2_ft,
  "Table 2. Adjusted between-group contrasts for voice outcomes",
  "Table_2_adjusted_outcome_contrasts.docx",
  paste0(
    "Note. Estimates are adjusted mean differences from linear regression ",
    "models with HC3 robust standard errors, adjusted for age, gender, vocal ",
    "career duration, formal musical education, and smoking status. The ",
    "primary hypothesis was not included in the secondary BH-FDR family. ",
    "BH = Benjamini-Hochberg; FDR = false discovery rate; VAS = visual ",
    "analogue scale."
  )
)

# Supplementary Table S1: sensitivity analyses ------------------------------

sensitivity <- read_analysis_csv("sensitivity_summary.csv")
table_s1 <- sensitivity |>
  dplyr::transmute(
    Outcome = outcome,
    Analysis = analysis,
    Contrast = contrast,
    Estimate = sprintf("%.2f", estimate),
    `95% CI` = sprintf("%.2f to %.2f", lower_95_CI, upper_95_CI),
    `P value` = format_p(p_value)
  )
write_publication_csv(
  table_s1,
  "Supplementary_Table_S1_sensitivity_analyses.csv"
)
table_s1_ft <- make_flextable(
  table_s1,
  font_size = 7.4,
  widths = c(1.15, 1.70, 1.30, 0.65, 1.15, 0.65),
  left_columns = c(1, 2, 3)
)
save_table_docx(
  table_s1_ft,
  paste0(
    "Supplementary Table S1. Vocal-load and overlap-weighting sensitivity ",
    "analyses for current versus non-wedding comparisons"
  ),
  "Supplementary_Table_S1_sensitivity_analyses.docx",
  paste0(
    "Note. HC3 analyses used heteroskedasticity-consistent standard errors. ",
    "Overlap-weighted analyses balanced the prespecified covariates. These ",
    "sensitivity-analysis P values were not included in the secondary BH-FDR ",
    "family. SVHI = Singing Voice Handicap Index."
  )
)

# Supplementary Table S2: vocal-symptom item results ------------------------

symptom_contrasts <- read_analysis_csv("vocal_symptom_item_contrasts.csv")
symptom_contrasts$result_order <- rank(
  symptom_contrasts$p.value,
  ties.method = "first"
)
symptom_contrasts <- symptom_contrasts[order(symptom_contrasts$result_order), ]

format_item_result <- function(estimate, lower, upper, p, q) {
  paste0(
    sprintf("%.2f (95%% CI %.2f to %.2f), ", estimate, lower, upper),
    "P ", ifelse(p < 0.001, "< .001", paste0("= ", format_p(p))),
    ", q ", ifelse(q < 0.001, "< .001", paste0("= ", format_q(q)))
  )
}

table_s2 <- symptom_contrasts |>
  dplyr::transmute(
    `Symptom item` = label,
    Contrast = dplyr::recode(
      contrast,
      "Current vs Non-wedding" = "Current vs non-wedding",
      "Current vs Former" = "Current vs former",
      "Former vs Non-wedding" = "Former vs non-wedding"
    ),
    `Adjusted difference in severity score` = format_item_result(
      estimate,
      lower.CL,
      upper.CL,
      p.value,
      q_value_BH
    ),
    `FDR result` = ifelse(
      q_value_BH < 0.05,
      "FDR-significant",
      "Not FDR-significant"
    )
  )
write_publication_csv(
  table_s2,
  "Supplementary_Table_S2_vocal_symptom_item_results.csv"
)
table_s2_ft <- make_flextable(
  table_s2,
  font_size = 7.0,
  widths = c(1.30, 1.25, 3.45, 1.20),
  left_columns = c(1, 2, 3)
)
save_table_docx(
  table_s2_ft,
  "Supplementary Table S2. Item-level vocal symptom results",
  "Supplementary_Table_S2_vocal_symptom_item_results.docx",
  paste0(
    "Note. Values are adjusted mean differences with 95% confidence ",
    "intervals from exploratory item-level linear models using HC3 robust ",
    "standard errors. Models were adjusted for age, gender, vocal career ",
    "duration, formal musical education, and smoking status. BH q values ",
    "refer to the separate family of 27 vocal-symptom item contrasts. ",
    "FDR = false discovery rate."
  ),
  landscape = TRUE
)

# Supplementary Table S3: SVHI item results ---------------------------------

svhi_contrasts <- read_analysis_csv("svhi_item_contrasts.csv")
svhi_contrasts$contrast_display <- dplyr::recode(
  svhi_contrasts$contrast,
  "Current vs Non-wedding" = "Current vs non-wedding",
  "Current vs Former" = "Current vs former",
  "Former vs Non-wedding" = "Former vs non-wedding"
)
svhi_contrasts$result_compact <- paste0(
  sprintf(
    "%.2f [%.2f, %.2f]; P ",
    svhi_contrasts$estimate,
    svhi_contrasts$lower.CL,
    svhi_contrasts$upper.CL
  ),
  ifelse(
    svhi_contrasts$p.value < 0.001,
    "< .001",
    paste0("= ", format_p(svhi_contrasts$p.value))
  ),
  "; q ",
  ifelse(
    svhi_contrasts$q_value_BH < 0.001,
    "< .001",
    paste0("= ", format_q(svhi_contrasts$q_value_BH))
  )
)

table_s3 <- svhi_contrasts |>
  dplyr::select(item_number, contrast_display, result_compact) |>
  tidyr::pivot_wider(
    names_from = contrast_display,
    values_from = result_compact
  ) |>
  dplyr::mutate(Item = paste("SVHI", item_number)) |>
  dplyr::select(
    Item,
    `Current vs non-wedding`,
    `Current vs former`,
    `Former vs non-wedding`
  )
write_publication_csv(table_s3, "Supplementary_Table_S3_SVHI_item_results.csv")
table_s3_ft <- make_flextable(
  table_s3,
  font_size = 6.4,
  widths = c(0.65, 2.15, 2.15, 2.15),
  left_columns = 1
)
save_table_docx(
  table_s3_ft,
  "Supplementary Table S3. Item-level SVHI results",
  "Supplementary_Table_S3_SVHI_item_results.docx",
  paste0(
    "Note. Values are adjusted mean differences [95% CI] with nominal P ",
    "values and Benjamini-Hochberg q values. All 36 SVHI items were analysed ",
    "across the three predefined vocalist-group contrasts. Models used HC3 ",
    "robust standard errors and were adjusted for age, gender, vocal career ",
    "duration, formal musical education, and smoking status. BH q values ",
    "refer to the separate family of all 108 SVHI item-level contrasts. ",
    "SVHI = Singing Voice Handicap Index."
  ),
  landscape = TRUE
)

# Supplementary Table S4: voice care and singing training -------------------

table_s4_raw <- read_analysis_csv("Supplementary_Table_S4_data.csv")
write_publication_csv(
  table_s4_raw,
  "Supplementary_Table_S4_voice_care_training.csv"
)
variable_starts <- which(!duplicated(table_s4_raw$VariableID))
table_s4_word <- table_s4_raw
table_s4_word$`Omnibus P value` <- format_fixed_without_zero(
  table_s4_word$`Omnibus P value`,
  5
)
table_s4_word$`BH q value` <- format_fixed_without_zero(
  table_s4_word$`BH q value`,
  4
)
table_s4_word$VariableID <- NULL

table_s4_ft <- make_flextable(
  table_s4_word,
  font_size = 7.2,
  widths = c(1.55, 1.15, 0.95, 0.95, 0.95, 0.60, 0.55, 3.10),
  left_columns = c(1, 2, 8),
  padding = 2
)
table_s4_ft <- flextable::merge_v(
  table_s4_ft,
  j = c(
    "Characteristic",
    "Omnibus P value",
    "BH q value",
    "Adjusted pairwise contrasts"
  ),
  part = "body"
)
table_s4_ft <- flextable::padding(
  table_s4_ft,
  j = "Category",
  padding.left = 7,
  part = "body"
)
table_s4_ft <- flextable::paginate(
  table_s4_ft,
  init = FALSE,
  hdr_ftr = TRUE,
  group = variable_starts,
  group_def = "starts"
)

save_table_docx(
  table_s4_ft,
  paste0(
    "Supplementary Table S4. Voice-care and singing-training ",
    "characteristics by vocalist group"
  ),
  "Supplementary_Table_S4_voice_care_training.docx",
  paste0(
    "Note. Values are n/N (%). Denominators are participants with non-missing, ",
    "applicable data for each variable. For regularity of individual singing ",
    "lessons, group-specific denominators were 236, 22, and 27; for the ",
    "professional performing the voice examination, they were 183, 37, and ",
    "29. Omnibus P values used Pearson chi-square tests or Fisher exact tests ",
    "with Monte Carlo simulation (20,000 replicates), as appropriate; BH q ",
    "values were adjusted across nine omnibus comparisons. Adjusted logistic ",
    "models covered five unambiguous binary indicators and three group ",
    "contrasts per indicator (15 tests), with BH correction across all 15. ",
    "The three-category voice-examination variable was not collapsed because ",
    "routine examinations and examinations prompted by voice problems ",
    "represent distinct care patterns. BH = Benjamini-Hochberg; CI = ",
    "confidence interval; ENT = otorhinolaryngologist; GP = general ",
    "practitioner; OR = odds ratio; OTC = over-the-counter."
  ),
  landscape = TRUE
)

# Figure 1: adjusted outcome contrasts --------------------------------------

figure_1_data <- main_results |>
  dplyr::mutate(
    outcome_display = dplyr::recode(
      outcome,
      "SVHI total" = "SVHI total",
      "Vocal Symptoms" = "Vocal symptoms",
      "Singing Voice VAS" = "Singing voice VAS",
      "Spoken Voice VAS" = "Spoken voice VAS"
    ),
    outcome_display = factor(
      outcome_display,
      levels = c(
        "SVHI total",
        "Vocal symptoms",
        "Singing voice VAS",
        "Spoken voice VAS"
      )
    ),
    contrast_display = factor(
      contrast,
      levels = c(
        "Former vs Non-wedding",
        "Current vs Former",
        "Current vs Non-wedding"
      )
    ),
    result_class = dplyr::case_when(
      hierarchy == "Primary" ~ "Primary hypothesis",
      !is.na(q_value_BH) & q_value_BH < 0.05 ~ "Secondary, FDR q < 0.05",
      TRUE ~ "Secondary, not FDR-significant"
    ),
    result_class = factor(
      result_class,
      levels = c(
        "Primary hypothesis",
        "Secondary, FDR q < 0.05",
        "Secondary, not FDR-significant"
      )
    )
  )

figure_1 <- ggplot2::ggplot(
  figure_1_data,
  ggplot2::aes(
    x = estimate,
    y = contrast_display,
    shape = result_class,
    colour = result_class
  )
) +
  ggplot2::geom_vline(
    xintercept = 0,
    linetype = 2,
    linewidth = 0.45,
    colour = charcoal
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower_95_CI, xmax = upper_95_CI),
    orientation = "y",
    width = 0.12,
    linewidth = 0.70
  ) +
  ggplot2::geom_point(size = 3.1) +
  ggplot2::facet_wrap(~ outcome_display, scales = "free_x", ncol = 2) +
  ggplot2::scale_shape_manual(values = c(16, 17, 15)) +
  ggplot2::scale_colour_manual(values = c(dark_grey, mid_grey, light_grey)) +
  ggplot2::labs(
    x = "Adjusted mean difference with 95% CI",
    y = NULL
  ) +
  theme_publication(11)

save_figure(figure_1, "Figure_1_adjusted_outcome_contrasts", 12, 9)

# Figure 2: adjusted vocal-symptom profile ----------------------------------

symptom_means <- read_analysis_csv("vocal_symptom_adjusted_means.csv")
symptom_order <- c(
  "Vocal fatigue after voice use",
  "Throat clearing",
  "Voice breaks",
  "Hoarseness",
  "Dry cough",
  "Wet cough",
  "Shortness of breath",
  "Episodes of voice loss/aphonia",
  "Effortful speaking"
)
symptom_means <- symptom_means |>
  dplyr::mutate(
    group = dplyr::recode(
      vocalist_group,
      "Non-wedding CCM singers" = "Non-wedding",
      "Current wedding-band singers" = "Current wedding-band",
      "Former wedding-band singers" = "Former wedding-band"
    ),
    group = factor(group, levels = names(group_palette)),
    label = factor(label, levels = rev(symptom_order))
  )

figure_2 <- ggplot2::ggplot(
  symptom_means,
  ggplot2::aes(x = emmean, y = label, colour = group, shape = group)
) +
  ggplot2::geom_rect(
    data = data.frame(ymin = 8.55, ymax = 9.45),
    ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = very_light_grey,
    colour = NA
  ) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower.CL, xmax = upper.CL),
    orientation = "y",
    width = 0.10,
    linewidth = 0.65,
    position = ggplot2::position_dodge(width = 0.32)
  ) +
  ggplot2::geom_point(
    size = 3.0,
    position = ggplot2::position_dodge(width = 0.32)
  ) +
  ggplot2::scale_colour_manual(values = group_palette) +
  ggplot2::scale_shape_manual(values = c(16, 18, 17)) +
  ggplot2::scale_x_continuous(limits = c(0, 4), breaks = 0:4) +
  ggplot2::labs(x = "Adjusted mean symptom severity (0-4)", y = NULL) +
  theme_publication(11)

save_figure(figure_2, "Figure_2_vocal_symptom_profile", 9, 8)

# Supplementary Figure S1: adjusted marginal means --------------------------

adjusted_means <- read_analysis_csv("adjusted_marginal_means.csv")
adjusted_means <- adjusted_means |>
  dplyr::mutate(
    group = dplyr::recode(
      vocalist_group,
      "Non-wedding CCM singers" = "Non-wedding",
      "Current wedding-band singers" = "Current wedding-band",
      "Former wedding-band singers" = "Former wedding-band"
    ),
    group = factor(
      group,
      levels = c("Non-wedding", "Former wedding-band", "Current wedding-band")
    ),
    outcome_display = dplyr::recode(
      outcome,
      "SVHI total" = "SVHI total",
      "Vocal Symptoms" = "Vocal symptoms",
      "Singing Voice VAS" = "Singing voice VAS",
      "Spoken Voice VAS" = "Spoken voice VAS"
    ),
    outcome_display = factor(
      outcome_display,
      levels = c(
        "SVHI total",
        "Vocal symptoms",
        "Singing voice VAS",
        "Spoken voice VAS"
      )
    )
  )

figure_s1 <- ggplot2::ggplot(
  adjusted_means,
  ggplot2::aes(x = emmean, y = group)
) +
  ggplot2::geom_errorbar(
    ggplot2::aes(xmin = lower.CL, xmax = upper.CL),
    orientation = "y",
    width = 0.12,
    colour = charcoal,
    linewidth = 0.55
  ) +
  ggplot2::geom_point(size = 2.8, colour = dark_grey) +
  ggplot2::facet_wrap(~ outcome_display, scales = "free_x", ncol = 2) +
  ggplot2::labs(
    x = "Adjusted estimated marginal mean with 95% CI",
    y = NULL
  ) +
  theme_publication(11) +
  ggplot2::theme(legend.position = "none")

save_figure(
  figure_s1,
  "Supplementary_Figure_S1_adjusted_marginal_means",
  10,
  8
)

# Supplementary Figure S2: five selected voice-care indicators --------------

figure_s2_data <- read_analysis_csv("Supplementary_Figure_S2_data.csv")
figure_s2_order <- c(
  "ENT visit in the last year",
  "Phoniatrician visit in the last year",
  "OTC voice products or medications",
  "Individual singing lessons",
  "Participation in singing classes"
)
figure_s2_data$Characteristic <- factor(
  figure_s2_data$Characteristic,
  levels = rev(figure_s2_order)
)
figure_s2_data$Group <- factor(
  figure_s2_data$Group,
  levels = names(bar_palette)
)

figure_s2 <- ggplot2::ggplot(
  figure_s2_data,
  ggplot2::aes(x = Percentage, y = Characteristic, fill = Group)
) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.78),
    width = 0.23,
    colour = "#3F3F3F",
    linewidth = 0.25
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f%%", Percentage)),
    position = ggplot2::position_dodge(width = 0.78),
    hjust = -0.12,
    size = 3.2
  ) +
  ggplot2::scale_fill_manual(values = bar_palette) +
  ggplot2::scale_x_continuous(
    limits = c(0, 90),
    breaks = c(0, 25, 50, 75),
    labels = c("0%", "25%", "50%", "75%"),
    expand = ggplot2::expansion(mult = c(0, 0))
  ) +
  ggplot2::labs(
    x = "Participants reporting characteristic",
    y = NULL,
    fill = NULL
  ) +
  theme_publication(11) +
  ggplot2::theme(
    panel.grid.major.y = ggplot2::element_blank(),
    plot.margin = ggplot2::margin(10, 35, 10, 10)
  )

save_figure(
  figure_s2,
  "Supplementary_Figure_S2_selected_voice_care_training",
  10,
  8,
  tiff_dpi = 300
)

# Captions -------------------------------------------------------------------

captions <- c(
  Figure_1_adjusted_outcome_contrasts_caption.txt = paste0(
    "Figure 1. Adjusted between-group differences in voice outcomes. Points ",
    "represent adjusted mean differences and horizontal bars represent 95% ",
    "confidence intervals from linear regression models with HC3 robust ",
    "standard errors. Models were adjusted for age, gender, vocal career ",
    "duration, formal musical education, and smoking status. The circle ",
    "denotes the prespecified primary hypothesis; triangles denote secondary ",
    "contrasts significant after Benjamini-Hochberg false-discovery-rate ",
    "correction; squares denote secondary contrasts that were not ",
    "FDR-significant. SVHI = Singing Voice Handicap Index; VAS = visual ",
    "analogue scale. Lower VAS values indicate poorer self-assessed voice."
  ),
  Figure_2_vocal_symptom_profile_caption.txt = paste0(
    "Figure 2. Adjusted vocal symptom profile by vocalist group. Points ",
    "represent estimated marginal means and horizontal bars represent 95% ",
    "confidence intervals from item-level linear regression models with HC3 ",
    "robust standard errors. Models were adjusted for age, gender, vocal ",
    "career duration, formal musical education, and smoking status. The ",
    "shaded row highlights vocal fatigue after voice use, the symptom showing ",
    "the clearest between-group differences in the separate exploratory ",
    "item-level analysis."
  ),
  Supplementary_Figure_S1_adjusted_marginal_means_caption.txt = paste0(
    "Supplementary Figure S1. Adjusted estimated marginal means for the four ",
    "voice outcomes by vocalist group. Points represent estimated marginal ",
    "means and horizontal bars represent 95% confidence intervals from linear ",
    "regression models with HC3 robust standard errors. Models were adjusted ",
    "for age, gender, vocal career duration, formal musical education, and ",
    "smoking status. SVHI = Singing Voice Handicap Index; VAS = visual ",
    "analogue scale."
  ),
  Supplementary_Figure_S2_selected_voice_care_training_caption.txt = paste0(
    "Supplementary Figure S2. Selected voice-care and singing-training ",
    "characteristics by vocalist group. Bars show the percentage of ",
    "participants reporting each of five selected binary characteristics. ",
    "These descriptive and exploratory comparisons were not part of the ",
    "primary outcome hierarchy. OTC = over-the-counter."
  )
)
for (filename in names(captions)) {
  writeLines(
    captions[[filename]],
    file.path(publication_dir, "captions", filename),
    useBytes = TRUE
  )
}

# Copy all aggregate analysis results used by the publication script ---------

analysis_csv_files <- list.files(
  analysis_dir,
  pattern = "[.]csv$",
  full.names = TRUE
)
file.copy(
  analysis_csv_files,
  file.path(publication_dir, "source_tables", basename(analysis_csv_files)),
  overwrite = TRUE
)

capture.output(
  sessionInfo(),
  file = file.path(publication_dir, "R_session_info.txt")
)

expected_word_tables <- c(
  "Table_1_group_characteristics.docx",
  "Table_2_adjusted_outcome_contrasts.docx",
  "Supplementary_Table_S1_sensitivity_analyses.docx",
  "Supplementary_Table_S2_vocal_symptom_item_results.docx",
  "Supplementary_Table_S3_SVHI_item_results.docx",
  "Supplementary_Table_S4_voice_care_training.docx"
)
expected_tiff_figures <- c(
  "Figure_1_adjusted_outcome_contrasts.tiff",
  "Figure_2_vocal_symptom_profile.tiff",
  "Supplementary_Figure_S1_adjusted_marginal_means.tiff",
  "Supplementary_Figure_S2_selected_voice_care_training.tiff"
)
expected_outputs <- c(
  file.path(publication_dir, "tables_word", expected_word_tables),
  file.path(publication_dir, "figures_tiff", expected_tiff_figures)
)
if (any(!file.exists(expected_outputs))) {
  stop("At least one expected publication file was not created.")
}

cat("Publication tables, figures, and captions completed successfully.\n")
