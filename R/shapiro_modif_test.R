#' Modified Shapiro-Wilk normality test (Rahman & Govindarajulu, 1997)
#'
#' Performs the modification of the Shapiro-Wilk test proposed by Rahman and
#' Govindarajulu (1997), approximating the p-value by Monte Carlo simulation
#' under the null hypothesis of normality.
#'
#' The statistic is \eqn{W = (\sum a_i x_{(i)})^2 / \sum (x_i - \bar{x})^2},
#' where \eqn{x_{(i)}} are the ordered observations and \eqn{a_i} are the
#' normalized coefficients of Rahman and Govindarajulu, which depend only on
#' the sample size. Small values of \eqn{W} indicate departure from normality,
#' so the p-value is the proportion of simulated statistics less than or equal
#' to the observed one, computed as \eqn{(1 + k) / (n_{sim} + 1)} (Davison &
#' Hinkley, 1997). This estimator is never exactly zero; its smallest possible
#' value is \eqn{1 / (n_{sim} + 1)}.
#'
#' Missing values are removed before computing the test, as in
#' [stats::shapiro.test()].
#'
#' @param x Numeric vector (e.g. the residuals of a linear model).
#' @param nsim Number of Monte Carlo simulations.
#' @param seed Seed for the random number generator, for reproducible
#'   p-values. If `NULL` (default) no seed is set. When a seed is given, the
#'   random number generator state of the session is restored on exit, so
#'   the function does not alter later random draws.
#' @return An object of class `"htest"` (printed like the output of
#'   [stats::shapiro.test()]) with components `statistic` (\eqn{W}),
#'   `p.value`, `method` and `data.name`.
#' @references
#' Rahman, M. M., & Govindarajulu, Z. (1997). A modification of the test of
#' Shapiro and Wilk for normality. *Journal of Applied Statistics*, 24(2),
#' 219-236.
#'
#' Davison, A. C., & Hinkley, D. V. (1997). *Bootstrap Methods and Their
#' Application*. Cambridge University Press.
#' @examples
#' x = rnorm(50)
#' shapiro.modif.test(x, nsim = 1000, seed = 123)
#'
#' # Skewed data
#' y = rexp(50)
#' res = shapiro.modif.test(y, nsim = 1000, seed = 123)
#' res$statistic
#' res$p.value
#' @export
shapiro.modif.test = function(x, nsim = 10000, seed = NULL) {
  dname = paste(deparse(substitute(x)), collapse = " ")           # name of the data, for the printed output

  x = x[!is.na(x)]                                                 # remove missing values (as shapiro.test does)
  if (!is.numeric(x)) stop("'x' must be a numeric vector.")
  if (any(!is.finite(x))) stop("'x' contains infinite values.")
  n = length(x)                                                    # sample size after removing NAs
  if (n < 3) stop("At least 3 non-missing values are needed.")
  if (diff(range(x)) == 0) stop("All 'x' values are identical.")   # W would be 0/0
  if (nsim < 1) stop("'nsim' must be a positive integer.")

  if (!is.null(seed)) {                                            # reproducible run without side effects
    genv = globalenv()
    if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      old_seed = get(".Random.seed", envir = genv, inherits = FALSE)  # save the current RNG state...
      on.exit(assign(".Random.seed", old_seed, envir = genv), add = TRUE)  # ...and restore it on exit
    } else {
      on.exit(rm(".Random.seed", envir = genv), add = TRUE)        # there was no state: leave it that way
    }
    set.seed(seed)
  }

  a = rg_coefficients(n)                                           # coefficients depend only on n: compute once
  w_obs = rg_statistic(x, a)                                       # observed statistic
  w_sim = vapply(seq_len(nsim),
                 function(i) rg_statistic(stats::rnorm(n), a),
                 numeric(1))                                       # null distribution of W under normality
  p_value = (1 + sum(w_sim <= w_obs)) / (nsim + 1)                 # Monte Carlo p-value (lower tail)

  structure(list(
    statistic = c(W = w_obs),
    p.value = p_value,
    method = paste0("Rahman-Govindarajulu modified Shapiro-Wilk normality test ",
                    "(Monte Carlo p-value based on ", nsim, " simulations)"),
    data.name = dname
  ), class = "htest")
}


# Internal helper: normalized Rahman & Govindarajulu (1997) coefficients for sample size n
rg_coefficients = function(n) {
  m = stats::qnorm((1:n) / (n + 1))                                # approximate expected normal order statistics
  f = stats::dnorm(m)                                              # standard normal density at m
  mf = m * f
  mf_next = c(mf[-1], 0)                                           # m[i+1] * f(m[i+1]), zero at the upper boundary
  mf_prev = c(0, mf[-n])                                           # m[i-1] * f(m[i-1]), zero at the lower boundary
  a = f * (2 * mf - mf_next - mf_prev)                             # unnormalized coefficients
  a / sqrt(sum(a^2))                                               # normalize so that sum(a^2) = 1
}


# Internal helper: W statistic for data x and coefficients a
rg_statistic = function(x, a) {
  xs = sort(x)                                                     # order statistics
  sum(a * xs)^2 / sum((xs - mean(xs))^2)
}
