#' @title Update the blood search weights

#' @description Update blood search weights dynamically

#' @inheritParams ramp.xds::update_blood_search_weights
#'
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_blood_search_weights.dynamic <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]]$search_obj,{
    xds_obj$MY_obj[[s]]$search_weights <- F_blood_weights(t, y, xds_obj, s)
    return(xds_obj)
  })}

#' @title Compute blood search weights
#'
#' @description The `S3` definition for the
#' function that computes blood search weights
#'
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
F_blood_weights = function(t, y, xds_obj, s){
  UseMethod("F_blood_weights", xds_obj$MY_obj[[s]]$search_obj)
}

#' @title Compute availability of blood search weights
#'
#' @description Static updating for blood search weights
#'
#' @inheritParams F_blood_weights
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_blood_weights.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Set up blood search weights
#'
#' @description Compose a time series function, `F_blood_weights` to compute [setup_blood_search_weights].
#'
#' @inheritParams ramp.xds::setup_blood_search_weights
#' @param season_par seasonality function for
#' @param trend_par trend function parameters
#' @param shock_par trend function parameters
#'
#' @seealso [make_tsy_function]
#'
#' @return an **`xds`** object
#' @keywords internal
#'
#' @export
setup_blood_search_weights.ts_function = function(name, xds_obj, options = list(), s=1,
                                                    season_par=makepar_F_c(1),
                                                    trend_par=makepar_F_c(1),
                                                    shock_par=makepar_F_c(1)){
  class(xds_obj$bloods) <- 'dynamic'
  with(options,{
    search_obj <- list()
    class(search_obj) <- c("ts_function", "dynamic")
    search_obj$season_par = season_par
    search_obj$trend_par = trend_par
    search_obj$shock_par = shock_par
    search_obj$F_blood_weights = make_tsy_function(season_par=season_par, trend_par=trend_par, shock_par=shock_par, N=xds_obj$nPatches)
    xds_obj$MY_obj[[s]]$search_obj = search_obj
    return(xds_obj)
  })}

#' @title Compute blood search weights
#'
#' @description Compute blood search weights
#' using a time series function
#'
#' @inheritParams F_blood_weights
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_blood_weights.ts_function = function(t, y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$search_obj, {
    xds_obj$MY_obj[[s]]$search_weights = F_blood_weights(t, y, xds_obj, s)
    return(xds_obj)
  })}
