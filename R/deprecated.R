#' Deprecated functions in leoRtools
#'
#' These functions were renamed in version 0.2.0 and are kept only so that
#' older scripts keep working. They issue a deprecation warning and call
#' [assumptions()], which now detects the class of the model.
#'
#' * `supuestos()`: use [assumptions()].
#' * `supuestos.ml()`: use [assumptions()].
#'
#' @param modelo,mod A fitted model.
#' @return See [assumptions()].
#' @name leoRtools-deprecated
#' @keywords internal
NULL

#' @rdname leoRtools-deprecated
#' @export
supuestos = function(modelo) {
  .Deprecated("assumptions", package = "leoRtools")               # warn and point to the new name
  assumptions(modelo)
}

#' @rdname leoRtools-deprecated
#' @export
supuestos.ml = function(mod) {
  .Deprecated("assumptions", package = "leoRtools")               # warn and point to the new name
  assumptions(mod)
}
