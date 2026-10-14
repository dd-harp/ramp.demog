#' @title Update the time spent matrix

#' @description Update time spent matrix dynamically

#' @inheritParams ramp.xds::update_timespent
#'
#' @return an **`xds`** object
#' @export
#' @keywords internal
update_timespent.dynamic <- function(t, y, xds_obj, s) {
  with(xds_obj$MY_obj[[s]]$timespent_obj,{
    xds_obj$MY_obj[[s]]$timespent <- F_timespent(t, y, xds_obj, s)
    return(xds_obj)
  })}

#' @title Compute time spent matrix
#'
#' @description The `S3` definition for the
#' function that computes time spent matrix
#'
#' @param t the time
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
  F_timespent = function(t, y, xds_obj, s){
  UseMethod("F_timespent", xds_obj$MY_obj[[s]]$timespent_obj)
}

#' @title Compute availability of time spent matrix
#'
#' @description Static updating for time spent matrix
#'
#' @inheritParams F_timespent
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_timespent.static = function(t, y, xds_obj, s){
  return(return(xds_obj))
}

#' @title Set up time spent matrix
#'
#' @description Compose a time series function, `F_timespent` to compute [setup_timespent].
#'
#' @inheritParams ramp.xds::setup_timespent
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
setup_timespent.ts_function = function(name, xds_obj, options = list(), s=1,
                                                  season_par=makepar_F_c(1),
                                                  trend_par=makepar_F_c(1),
                                                  shock_par=makepar_F_c(1)){
  class(xds_obj$bloods) <- 'dynamic'
  with(options,{
    timespent_obj <- list()
    class(timespent_obj) <- c("ts_function", "dynamic")
    timespent_obj$season_par = season_par
    timespent_obj$trend_par = trend_par
    timespent_obj$shock_par = shock_par
    timespent_obj$F_timespent = make_tsy_function(season_par=season_par, trend_par=trend_par, shock_par=shock_par, N=xds_obj$nPatches)
    xds_obj$MY_obj[[s]]$timespent_obj = timespent_obj
    return(xds_obj)
  })}

#' @title Compute time spent matrix
#'
#' @description Compute time spent matrix
#' using a time series function
#'
#' @inheritParams F_timespent
#'
#' @return an **`xds`** object
#'
#' @keywords internal
#' @export
F_timespent.ts_function = function(t, y, xds_obj, s){
  with(xds_obj$MY_obj[[s]]$timespent_obj, {
    xds_obj$MY_obj[[s]]$timespent = F_timespent(t, y, xds_obj, s)
    return(xds_obj)
  })}
