#' @title Make parameters for null human demography model
#' @param pars an **`xds`** object
#' @param H size of human population in each strata
#' @return none
#' @export
make_demography_matrix <- function(pars, H) {
  stopifnot(length(H) == pars$nStrata)
  Hpar <- list()
  Hpar$H <- H
  Hpar$nStrata <- length(H)

  Bf <- "zero"
  class(Bf) <- "zero"
  Hpar$Bf <- Bf

  dH <- "zero"
  class(dH) <- "zero"
  Hpar$dH <- dH

  pars$Hpar[[1]] <- Hpar

  return(pars)
}
