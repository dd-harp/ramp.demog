
#' @title Set up no births  
#' @description Don't change anything 
#' @inheritParams ramp.xds::setup_births
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_births.ts_func = function(name, xds_obj, options = list(), i=1){
  
  with(xds_obj$XH_obj[[i]]$birth_obj, with(options,stopifnot(exists("birth_rates"))))
  birth_rates = with(xds_obj$XH_obj[[i]]$birth_obj, with(options, birth_rates))
  check_births(birth_rates, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$birth_obj$birth_rates = birth_rates
  
  if(with(xds_obj$XH_obj[[i]]$birth_obj, !exists("base_birth_rates")))
    xds_obj$XH_obj[[i]]$birth_obj$base_birth_rates = birth_rates
  
  trend_par = options$trend_par
  if(is.null(trend_par)) trend_par = makepar_F_c(1)
  xds_obj$XH_obj[[i]]$birth_obj$trend_par = trend_par
  xds_obj$XH_obj[[i]]$birth_obj$F_trend = make_F_t(trend_par)
  
  season_par = options$season_par
  if(is.null(season_par)) season_par = makepar_F_c(1)
  xds_obj$XH_obj[[i]]$birth_obj$season_par = season_par
  xds_obj$XH_obj[[i]]$birth_obj$F_season = make_F_t(season_par)
  
  shock_par = options$shock_par
  if(is.null(shock_par)) shock_par = makepar_F_c(1)
  xds_obj$XH_obj[[i]]$birth_obj$shock_par = shock_par
  xds_obj$XH_obj[[i]]$birth_obj$F_shock = make_F_t(shock_par)
  
  class(xds_obj$XH_obj[[i]]$birth_obj) = "ts_func"
  
  xds_obj <- update_XY_interface(xds_obj)
  
  class(xds_obj$blood_feeding_obj) = "dynamic"
  class(xds_obj$transmission_obj) = "dynamic"

  return(xds_obj)
}

#' @title Update the base birth rates
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_base_birth_rates = function(xds_obj, i=1){
  births <- xds_obj$XH_obj[[i]]$birth_obj$birth_rates
  xds_obj$XH_obj[[i]]$birth_obj$base_birth_rates <- births
  return(xds_obj)
}

#' @title Human/host births
#'
#' @description This is a 
#'
#' @inheritParams ramp.xds::Births
#'
#' @return see help pages for specific methods
#' @keywords internal
#' @export
Births.ts_func <- function(t, xds_obj, i){
  with(xds_obj$XH_obj[[i]]$birth_obj,{
    return(birth_rates*F_trend(t)*F_season(t)*F_shock(t))
})}
