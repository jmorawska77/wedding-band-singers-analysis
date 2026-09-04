# Shared utilities -----------------------------------------------------------

options(stringsAsFactors = FALSE)

project_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

if (!file.exists(file.path(project_root, "WeddingBandSingers.Rproj"))) {
  stop(
    "Open WeddingBandSingers.Rproj or set the working directory to the ",
    "repository root before running a script."
  )
}

project_path <- function(...) {
  file.path(project_root, ...)
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

require_packages <- function(packages) {
  missing <- packages[
    !vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing) > 0L) {
    stop(
      "Install the following R packages before running this script: ",
      paste(missing, collapse = ", ")
    )
  }

  invisible(TRUE)
}

assert_columns <- function(data, columns, object_name = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0L) {
    stop(
      "Missing columns in ", object_name, ": ",
      paste(missing, collapse = ", ")
    )
  }
  invisible(TRUE)
}

planned_contrasts <- list(
  "Current vs Non-wedding" = c(-1, 1, 0),
  "Former vs Non-wedding" = c(-1, 0, 1),
  "Current vs Former" = c(0, 1, -1)
)

prepare_model_factors <- function(data) {
  data$vocalist_group <- factor(
    as.character(data$vocalist_group),
    levels = c(
      "Non-wedding CCM singers",
      "Current wedding-band singers",
      "Former wedding-band singers"
    )
  )

  data$vocal_career_duration_model <- factor(
    as.character(data$vocal_career_duration_f),
    levels = c("<1 year", "1-2 years", "2-5 years", "5-10 years", ">10 years")
  )

  data$formal_musical_education_model <- factor(
    as.character(data$formal_musical_education_f),
    levels = c("None", "First degree", "Second degree", "Higher")
  )

  data
}

analysis_formula <- function(outcome, include_daily_singing_time = FALSE) {
  predictors <- c(
    "vocalist_group",
    "age",
    "gender_f",
    "vocal_career_duration_model",
    "formal_musical_education_model",
    "smoking_f"
  )

  if (isTRUE(include_daily_singing_time)) {
    predictors <- c(predictors, "daily_singing_time")
  }

  reformulate(predictors, response = outcome)
}

format_p <- function(x, digits = 3L) {
  threshold <- 10^(-digits)
  ifelse(
    is.na(x),
    "Not applicable",
    ifelse(
      x < threshold,
      paste0("<", sub("^0", "", formatC(threshold, format = "f", digits = digits))),
      sub("^0", "", formatC(x, format = "f", digits = digits))
    )
  )
}

write_csv <- function(data, path) {
  ensure_directory(dirname(path))
  utils::write.csv(data, path, row.names = FALSE, na = "")
  invisible(path)
}

