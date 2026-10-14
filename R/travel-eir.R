#' @title Update the time away

#' @description Update time away dynamically

#' @inheritParams ramp.xds::update_travel_eir
#'
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_travel_eir.dynamic <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]]$travel_eir_obj,{
    xds_obj$MY_obj[[s]]$travel_eir <- F_travel_eir(t, y, xds_obj, s)
    return(xds_obj)
  })}

#' @title Compute time away
#'
#' @description The `S3` definition for the
#' function that computes time away
#'
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
F_travel_eir = function(t, y, xds_obj, s){
  UseMethod("F_travel_eir", xds_obj$MY_obj[[s]]$travel_eir_obj)
}

#' @title Compute availability of time away
#'
#' @description Static updating for time away
#'
#' @inheritParams F_travel_eir
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_travel_eir.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Set up time away
#'
#' @description Compose a time series function, `F_travel_eir` to compute [setup_travel_eir].
#'
#' @inheritParams ramp.xds::setup_travel_eir
#' @param travel_eir the
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
setup_travel_eir.ts_function = function(name, xds_obj, options = list(), s=1,
                                        travel_eir = 0,
                                        season_par=makepar_F_c(1),
                                        trend_par=makepar_F_c(1),
                                        shock_par=makepar_F_c(1)){
  class(xds_obj$blood_feeding_obj) <- 'dynamic'
  class(xds_obj$transmission_obj) <- 'dynamic'
  with(options,{
    travel_eir_obj <- list()
    class(travel_eir_obj) <- c("ts_function", "dynamic")
    travel_eir_obj$travel_eir = travel_eir
    travel_eir_obj$season_par = season_par
    travel_eir_obj$trend_par = trend_par
    travel_eir_obj$shock_par = shock_par
    travel_eir_obj$F_travel_eir = make_tsy_function(season_par=season_par, trend_par=trend_par, shock_par=shock_par, N=xds_obj$nPatches)
    xds_obj$MY_obj[[s]]$travel_eir_obj = travel_eir_obj
    return(xds_obj)
  })}

#' @title Compute time away
#'
#' @description Compute time away
#' using a time series function
#'
#' @inheritParams F_travel_eir
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_travel_eir.ts_function = function(t, y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$travel_eir_obj, {
    xds_obj$MY_obj[[s]]$travel_eir = travel_eir*F_travel_eir(t, y, xds_obj, s)
    return(xds_obj)
  })}
