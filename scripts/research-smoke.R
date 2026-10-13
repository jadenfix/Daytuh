# Bounded reproduction of the real-data OLS / two-regime core in MSmodel3.15.R.
# This validates computation, not forecasting quality or investment performance.
suppressPackageStartupMessages(library(xts))
suppressPackageStartupMessages(library(MSwM))
args <- commandArgs(trailingOnly = TRUE)
output <- if (length(args)) args[[1]] else "artifacts/research-smoke.csv"
set.seed(7)
lines <- readLines("MSmodel3.15.R", warn = FALSE)
boundary <- grep("library\\(tseries\\)", lines, fixed = FALSE)[[1]]
core <- lines[seq_len(boundary - 1L)]
# The original exports a correlation CSV into the source tree; keep smoke outputs isolated.
core <- core[!grepl("^write.csv\\(", core)]
scope <- new.env(parent = globalenv())
invisible(capture.output(eval(parse(text = core), envir = scope)))
models <- list(DJ = scope$olsDJ, SP = scope$olsSP)
results <- lapply(names(models), function(name) {
  ols <- models[[name]]
  stopifnot(all(is.finite(coef(ols))), all(is.finite(residuals(ols))))
  original_error <- ""
  original <- tryCatch(
    msmFit(ols, k = 2, sw = rep(TRUE, length(coef(ols)) + 1L),
           control = list(parallelization = FALSE)),
    error = function(error) { original_error <<- conditionMessage(error); NULL })
  # A separately labeled residual diagnostic stays identifiable with this finite sample.
  standardized_residual <- residuals(ols) / sd(residuals(ols))
  diagnostic <- lm(standardized_residual ~ 1)
  fit <- msmFit(diagnostic, k = 2, sw = c(TRUE, TRUE),
                control = list(parallelization = FALSE))
  probabilities <- fit@Fit@smoProb
  stopifnot(all(is.finite(probabilities)))
  stopifnot(max(abs(rowSums(probabilities) - 1)) < 1e-6)
  score <- AIC(fit)
  stopifnot(is.finite(score))
  data.frame(series = name, observations = nobs(ols), ols_aic = AIC(ols),
             residual_two_regime_aic = score,
             original_switching_aic = if (is.null(original)) NA_real_ else AIC(original),
             original_switching_error = original_error, smoothed_probability_error = max(abs(rowSums(probabilities) - 1)),
             r_version = as.character(getRversion()), msm_version = as.character(packageVersion("MSwM")),
             scope = "original full OLS + separate residual-regime diagnostic; not forecast validation")
})
report <- do.call(rbind, results)
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
write.csv(report, output, row.names = FALSE)
print(report)
