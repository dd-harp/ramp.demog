#' @title Split by care seeking 
#' 
#' @description
#' Using a base model, split each population stratum 
#' into new strata with modified strata for treatment.
#' 
#' Since each module handles treatment in a slightly 
#' different way, the 
#'
#' @param splits the fraction in each new level
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @export
split_base_by_care_seeking = function(splits, xds_obj, i=1){
  
  Nw = length(splits)
  stopifnot(sum(splits)==1)
  
  Ns <- xds_obj$nStrata[i]
  xds_obj$nStrata[i] = Ns*Nw
  
  treat_ix <- catcat(1:Nw, 1:Ns)
  xds_obj$XH_obj[[i]]$treat_ix = treat_ix
  
  xds_obj <- rep_XH_inits_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_XH_pars_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_time_away_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_travel_eir_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_death_rates_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_blood_search_weights_by_strata(Nw, xds_obj, i)
  xds_obj <- stretch_timespent(Nw, xds_obj, i)
  xds_obj <- split_birth_rates(splits, xds_obj, i)
  xds_obj <- split_H_by_fracs(splits, xds_obj, i)
  xds_obj <- make_indices(xds_obj)
 
  xds_obj <- update_XY_interface(xds_obj)
  
  return(xds_obj)
}