# Daytuh financial research archive

R scripts and supplied macro/financial datasets for retrospective OLS, Markov-switching and machine-learning experiments.

```sh
# Requires R with xts and MSwM installed.
Rscript scripts/research-smoke.R artifacts/research-smoke.csv
```

The bounded smoke executes the original `MSmodel3.15.R` preparation and full OLS models on `data1451.csv` (468 usable observations per outcome). It attempts the original two-regime model, records numerical failures, and separately fits a clearly labeled two-regime diagnostic on standardized OLS residuals. Smoothed probabilities must be finite and sum to one. Outputs include runtime/package versions and never overwrite the archived correlation CSV.

On R 4.3.3 / MSwM 1.5, both OLS fits and residual diagnostics execute; the original full DJ switching model fails numerically. That failure is retained in the report. The original SP switching fit succeeds for the fixed smoke seed. This is fit-only, retrospective computation; it is not an out-of-sample forecast validation or trading-performance claim. The other archive scripts still require their own optional packages and validation.
