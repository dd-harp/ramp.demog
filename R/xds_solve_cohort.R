#' @title Solve a Dynamical System
#'
#' @description
#' Use `xds_setup_eir` to set up a model for cohort dynamics. 
#' This sets the birthday for a cohort \eqn{(d)}, the ages at which output is wanted \eqn{(a)}, 
#' and solves for \eqn{t=a+d}: 
#' \deqn{F_w(a) \times F_S(t) \times F_T(t) \times F_K(t)}
#'
#' @note
#' The variable `xds_obj$EIR_obj$bday` is set up
#' by `xds_setup_eir` but it is set to 0, so age
#' and time are identical. Using the birthday \eqn{(d)} and 
#' ages \eqn{(A)}, the system calls `xds_setup_eir` and 
#' sets \eqn{t = d + A.} 
#'
#' @param xds_obj an **`xds`** model object
#' @param birthday a cohort's birthday
#' @param Amax the oldest year, run from 0...Amax
#' @param da the age interval
#' @param ages a set of ages
#'
#' @export
xds_solve_cohort = function(xds_obj, birthday=0, Amax=365, da=1, ages=NULL){
  stopifnot(class(xds_obj) == "eir")
  xds_obj$EIR_obj$bday = birthday
  xds_obj <- xds_solve(xds_obj, Tmax=birthday+Amax, dt=da, times=ages+birthday)
  return(xds_obj)
}