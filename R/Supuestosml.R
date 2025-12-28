
#' Chequeo gráfico y estadístico de supuestos de un modelo lineal
#'
#' Realiza gráficos de residuos vs predichos y Q-Q plot, y evalúa
#' normalidad de los residuos (test de Shapiro-Wilk).
#'
#' @param mod Objeto de modelo lineal estimado por máxima verosimilitud(por ejemplo, de \code{gls}).
#' @return El valor p del tests de Shapiro-Wilk.
#' @details La función produce gráficos como efecto colateral.
#' @export
supuestos.ml = function(mod){
  e = resid(mod)
  re = residuals(mod, type = "p")
  pre = predict(mod)

  par(mfrow = c(1, 2))
  plot(pre, re, xlab = "Predichos", ylab = "Residuos de Pearson")
  abline(0, 0)
  car::qqPlot(re)
  par(mfrow = c(1, 1))

  return(shapiro.test(re))
}

