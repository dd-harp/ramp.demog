#' @title Change relative biting by age
#'
#' @description
#' Change the function for relative biting rates by age
#'
#' @param age_par a function object
#' @param xds_obj an **`xds`** model object
#' @param ix the species index
#'
#' @return an **`xds`** object
#' 
#' @importFrom ramp.func make_function
#' 
#' @keywords internal
#' @export
change_age = function(age_par, xds_obj, ix=1){
  xds_obj$EIR_obj$age_par <- age_par
  xds_obj$EIR_obj$F_age <- make_function(age_par)
  return(xds_obj)
}