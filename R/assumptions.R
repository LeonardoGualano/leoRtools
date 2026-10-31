#' Graphical and statistical check of model assumptions
#'
#' Draws a plot of residuals against fitted values and a normal Q-Q plot of
#' the residuals, and tests the assumptions of the model. The checks
#' performed depend on the class of the model (see Details).
#'
#' @section Linear models (`lm`, `aov`):
#' Uses standardized residuals ([stats::rstandard()]) in the plots, and tests
#' homogeneity of variances (Levene's test) and normality of the raw residuals
#' (Shapiro-Wilk test).
#'
#' Levene's test is computed with [car::leveneTest()], which by default centers
#' on the group medians (i.e. the Brown-Forsythe variant). It is only defined
#' when every explanatory variable is categorical, so if the model includes
#' any quantitative predictor (e.g. a regression or an ANCOVA) the test is
#' skipped with a message and its p-value is returned as `NA`.
#'
#' @section Models fitted with nlme (`gls`, `lme`):
#' Uses Pearson residuals, which are scaled by the modeled standard deviation.
#' These models are fitted by maximum likelihood or REML and may model
#' heterogeneity of variances explicitly (e.g. with [nlme::varIdent()]), so
#' Pearson residuals should show constant spread if the variance structure is
#' adequate. Therefore no Levene's test is performed: homogeneity of variances
#' is assessed graphically and, if needed, by comparing models with and
#' without a variance structure (e.g. by AIC or a likelihood-ratio test).
#' Normality is tested on the Pearson residuals (Shapiro-Wilk test). For
#' `lme` models, residuals and fitted values are at the innermost level of
#' grouping.
#'
#' @section Other models:
#' Generalized linear models (`glm`) and multivariate linear models (`mlm`)
#' are not supported, since normality and homogeneity of variances of the
#' residuals are not assumptions of those models in the same sense. Other
#' classes give an informative error.
#'
#' @param model A fitted model of class `lm`, `aov`, `gls` or `lme`.
#' @param ... Currently not used.
#' @return A named numeric vector with p-values rounded to 4 significant
#'   digits: `Levene` (`NA` if it was skipped) and `Shapiro-Wilk` for linear
#'   models; `Shapiro-Wilk` for `gls` and `lme` models.
#' @details The plots are produced as a side effect. The user's graphical
#'   parameters are restored on exit.
#' @seealso [shapiro.modif.test()] for a more powerful alternative to the
#'   Shapiro-Wilk test.
#' @examples
#' # One-way ANOVA: both tests are reported
#' m1 = lm(weight ~ group, data = PlantGrowth)
#' assumptions(m1)
#'
#' # Regression: Levene's test is skipped
#' m2 = lm(dist ~ speed, data = cars)
#' assumptions(m2)
#'
#' if (requireNamespace("nlme", quietly = TRUE)) {
#'   # Model with a different variance for each group
#'   m3 = nlme::gls(weight ~ group, data = PlantGrowth,
#'                  weights = nlme::varIdent(form = ~ 1 | group))
#'   assumptions(m3)
#' }
#' @export
assumptions = function(model, ...) {
  UseMethod("assumptions")                                         # dispatch on the class of the model
}


#' @rdname assumptions
#' @export
assumptions.lm = function(model, ...) {
  if (inherits(model, c("glm", "mlm")))                            # glm and mlm inherit from lm, but do not fit here
    stop("assumptions() does not support models of class '", class(model)[1], "'.")

  e = stats::resid(model)                                          # raw residuals (Shapiro-Wilk test)
  re = stats::rstandard(model)                                     # standardized residuals (plots)
  diagnostic_plots(stats::fitted(model), re, "Standardized residuals")

  groups = levene_groups(model)                                    # NULL if Levene's test is not applicable
  if (!is.null(groups)) {
    y = stats::model.response(stats::model.frame(model))          # response (already transformed, e.g. log(y))
    p_levene = car::leveneTest(y, groups)[["Pr(>F)"]][1]           # p-value of Levene's test
  } else {
    message("Levene's test skipped: it requires at least one explanatory ",
            "variable and all of them must be categorical (factors).")  # explain why it is missing
    p_levene = NA_real_                                            # keep the output structure constant
  }
  p_sw = stats::shapiro.test(e)$p.value                            # p-value of the Shapiro-Wilk test

  c("Levene" = signif(p_levene, 4),                                # signif() keeps very small p-values (round() turned them into 0)
    "Shapiro-Wilk" = signif(p_sw, 4))
}


#' @rdname assumptions
#' @export
assumptions.gls = function(model, ...) {
  pearson_check(model)                                             # same checks for gls and lme
}


#' @rdname assumptions
#' @export
assumptions.lme = function(model, ...) {
  pearson_check(model)                                             # same checks for gls and lme
}


#' @rdname assumptions
#' @export
assumptions.default = function(model, ...) {
  stop("assumptions() does not support models of class '", class(model)[1],
       "'. Supported classes: lm, aov, gls, lme.")
}


# Internal helper: plots and Shapiro-Wilk test on Pearson residuals (gls, lme)
pearson_check = function(model) {
  re = as.numeric(stats::residuals(model, type = "pearson"))       # residuals scaled by the modeled SD
  diagnostic_plots(as.numeric(stats::fitted(model)), re, "Pearson residuals")
  c("Shapiro-Wilk" = signif(stats::shapiro.test(re)$p.value, 4))   # p-value of the Shapiro-Wilk test
}


# Internal helper: residuals vs fitted and normal Q-Q plot, side by side
diagnostic_plots = function(fit, res, res_label) {
  old_par = graphics::par(mfrow = c(1, 2))                         # two panels side by side; keep previous settings
  on.exit(graphics::par(old_par))                                  # restore the user's settings when the function ends
  plot(fit, res, xlab = "Fitted values", ylab = res_label)         # residuals vs fitted
  graphics::abline(h = 0, lty = 2)                                 # reference line at zero
  car::qqPlot(res, xlab = "Normal quantiles", ylab = res_label)    # Q-Q plot with confidence band
  invisible(NULL)
}


# Internal helper: groups for Levene's test (all combinations of the explanatory
# variables), or NULL if the test is not applicable. Works on the model frame,
# so factors created inside the formula, e.g. factor(dose), are handled.
levene_groups = function(model) {
  mf = stats::model.frame(model)                                   # data actually used by the model
  preds = mf[-1]                                                   # drop the response (always first)
  preds = preds[!grepl("^\\(", names(preds))]                      # drop special columns such as "(weights)"
  is_categ = vapply(preds, function(v)
    is.factor(v) || is.character(v) || is.logical(v), logical(1)) # TRUE for categorical columns
  if (length(preds) == 0 || !all(is_categ)) return(NULL)           # no predictors, or some quantitative one
  interaction(preds, drop = TRUE)                                  # one group per observed combination of levels
}
