
# Función helper interna
calcular_W_RG = function(x) {
  n = length(x)
  p = (1:n) / (n + 1)
  m = qnorm(p)
  f = (1 / sqrt(2 * pi)) * exp(-0.5 * m^2)
  mf = m * f
  mf1 = c(mf[-1], 0)
  mf2 = c(0, mf[-length(mf)])
  mf3 = f * (2 * mf - mf1 - mf2)
  a = mf3 / sqrt(sum(mf3^2))
  x_sorted = sort(x)
  b = sum(a * x_sorted)
  w = b^2 / sum((x_sorted - mean(x_sorted))^2)
  return(w)
}

#' Test de Shapiro modificado (Rahman & Govindarajulu, 1997)
#'
#' Implementa el test propuesto por Rahman y Govindarajulu (1997),
#' utilizando simulaciones Monte Carlo para aproximar el valor p.
#'
#' @param x Vector numérico (por ejemplo, residuos de un modelo lineal).
#' @param num_simulaciones Número de simulaciones Monte Carlo.
#' @param seed Semilla para el generador aleatorio. Si es NULL, no se fija.
#' @return Una lista con el estadístico W y el valor p.
#'
#' @examples
#' x = rnorm(50)
#' shapiro.modif.test(x, num_simulaciones = 1000, seed = 123)
#'
#' @export
shapiro.modif.test = function(x, num_simulaciones = 50000, seed = NULL) {

  if (!is.null(seed)) {
    set.seed(seed)
  }

  n = length(x)

  Ws_simulados = replicate(
    num_simulaciones,
    calcular_W_RG(rnorm(n))
  )

  W_observado = calcular_W_RG(x)
  p_valor = mean(Ws_simulados <= W_observado)

  return(list(
    estadistico = W_observado,
    p.valor = p_valor
  ))
}

