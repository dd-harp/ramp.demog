#' @title Update the time away

#' @description Update time away dynamically

#' @inheritParams ramp.xds::update_time_away
#'
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_time_away.dynamic <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]]$time_away_obj,{
    xds_obj$MY_obj[[s]]$time_away <- F_time_away(t, y, xds_obj, s)
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
F_time_away = function(t, y, xds_obj, s){
  UseMethod("F_time_away", xds_obj$MY_obj[[s]]$time_away_obj)
}

#' @title Compute availability of time away
#'
#' @description Static updating for time away
#'
#' @inheritParams F_time_away
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_time_away.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Set up time away
#'
#' @description Compose a time series function, `F_time_away` to compute [setup_time_away].
#'
#' @inheritParams ramp.xds::setup_time_away
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
setup_time_away.ts_function = function(name, xds_obj, options = list(), s=1,
                                       season_par=makepar_F_c(1),
                                       trend_par=makepar_F_c(1),
                                       shock_par=makepar_F_c(1)){
  class(xds_obj$blood_feeding_obj) <- 'dynamic'
  class(xds_obj$transmission_obj) <- 'dynamic'
  with(options,{
    time_away_obj <- list()
    class(time_away_obj) <- c("ts_function", "dynamic")
    time_away_obj$season_par = season_par
    time_away_obj$trend_par = trend_par
    time_away_obj$shock_par = shock_par
    time_away_obj$F_time_away = make_tsy_function(season_par=season_par, trend_par=trend_par, shock_par=shock_par, N=xds_obj$nPatches)
    xds_obj$MY_obj[[s]]$time_away_obj = time_away_obj
    return(xds_obj)
  })}

#' @title Compute time away
#'
#' @description Compute time away
#' using a time series function
#'
#' @inheritParams F_time_away
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_time_away.ts_function = function(t, y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$time_away_obj, {
    xds_obj$MY_obj[[s]]$time_away = F_time_away(t, y, xds_obj, s)
    return(xds_obj)
  })}
