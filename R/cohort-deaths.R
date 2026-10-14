
#' @title Set up death rates
#' 
#' @description 
#' Pass pre-configured constant death rates. If it passes 
#' the checks, it replaces the current death rates.
#' 
#' If called with `name = "as_is"`, the deaths
#' must be at `options$death_rates
#' 
#' @inheritParams ramp.xds::setup_deaths
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_deaths.func = function(name, xds_obj, options=list(), i=1){
  stopifnot(class(xds_obj$frame) == "eir")
  check_deaths(options$F_mu(365), xds_obj$nStrata[i]) 
  class(xds_obj$XH_obj[[i]]$death_obj) = "func"
  xds_obj$XH_obj[[i]]$death_obj$F_mu = options$F_mu
  return(xds_obj)
}

#' @title Human/host deaths
#'
#' @inheritParams ramp.xds::Deaths
#' 
#' @note The first argument is called \eqn{t}, for
#' time, but it should only be used by [xds_solve_cohort]
#' which sets up \eqn{t=a}. 
#' 
#'
#' @return mortality rate for \eqn{X}
#' 
#' @keywords internal
#' @export
Deaths.func <- function(t, X, xds_obj, i){
  death_rates = xds_obj$XH_obj[[i]]$death_obj$F_mu(t) 
  return(death_rates*X)
}