
#' Chequeo gráfico y estadístico de supuestos de un modelo lineal
#'
#' Realiza gráficos de residuos vs predichos y Q-Q plot, y evalúa
#' homogeneidad de varianzas (test de Levene) y normalidad de los
#' residuos (test de Shapiro-Wilk).
#'
#' @param modelo Objeto de modelo lineal (por ejemplo, de \code{lm}).
#' @return Un vector de texto con los valores p de los tests de Levene
#'   y Shapiro-Wilk.
#' @details La función produce gráficos como efecto colateral.
#' @export
supuestos = function(modelo) {
  e = resid(modelo)
  re = rstandard(modelo)
  pre = predict(modelo)

  par(mfrow = c(1, 2))
  plot(pre, re, xlab = "Predichos", ylab = "Residuos de Pearson")
  abline(0, 0)
  car::qqPlot(re)
  par(mfrow = c(1, 1))

  a = car::leveneTest(modelo)
  b = shapiro.test(e)

  return(c(
    "Levene:" = round(a$`Pr(>F)`[1], 4),
    "Shapiro:" = round(b$p.value, 4)
  ))
}

