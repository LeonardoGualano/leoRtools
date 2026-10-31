# leoRtools 0.2.0

## Breaking changes

* All documentation, plot labels and messages are now in English.
* `supuestos()` and `supuestos.ml()` were replaced by a single function,
  `assumptions()`, which detects the class of the model (`lm`/`aov` or
  `gls`/`lme`) and performs the corresponding checks. The old names still
  work but are deprecated and issue a warning.
* `shapiro.modif.test()`:
  * The argument `num_simulaciones` was renamed to `nsim`, and its default
    changed from 50000 to 10000.
  * It now returns an `"htest"` object (printed like `shapiro.test()`); use
    `$statistic` and `$p.value` instead of `$estadistico` and `$p.valor`.
  * The Monte Carlo p-value is now computed as `(1 + k) / (nsim + 1)`, so it
    is never exactly 0.
* For `gls` and `lme` models, `assumptions()` returns the Shapiro-Wilk
  p-value instead of the whole test object (as `supuestos.ml()` did).
* Unsupported models (e.g. `glm`) now give an informative error.
* p-values are rounded to 4 significant digits instead of 4 decimal places,
  so very small p-values are no longer shown as 0.

## Bug fixes

* `assumptions()` no longer fails for models with quantitative predictors
  (regression, ANCOVA): Levene's test is skipped with a message and returned
  as `NA`.
* `shapiro.modif.test()` removes missing values instead of silently returning
  a wrong result, and checks its input.
* The y-axis of the residual plot in `assumptions()` is now correctly labelled
  as standardized residuals, and the Q-Q plots have proper axis labels.
* The functions restore the user's graphical parameters, and
  `shapiro.modif.test(seed = ...)` restores the random number generator state.

## Other changes

* Added `datos_trigo`, a simulated wheat dataset for teaching.
* `shapiro.modif.test()` computes the coefficients once instead of in every
  simulation, which makes it considerably faster.
* Added LICENSE file, README and declared `stats` and `graphics` imports.

# leoRtools 0.1.0

* First version.
