

#' @title Split by travel
#' 
#' @description
#' Using a base model, split each population stratum 
#' into new strata that differ by time away and the eir 
#' experienced while traveling. 
#'
#' 
#' @param splits the fraction in each
#' @param time_away time away 
#' @param travel_eir travel EIR
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @export
split_base_by_travel = function(splits, time_away, travel_eir, xds_obj, i=1){
  Nt = length(splits)
  stopifnot(sum(splits)==1)
  stopifnot(length(time_away)==Nt)
  stopifnot(length(travel_eir)==Nt)
  
  Ns <- xds_obj$nStrata[i]
  xds_obj$nStrata[i] = Ns*Nt

  
  trait_ix <- catcat(1:Nt, 1:Ns)[,1]
  ta <- time_away[trait_ix]
  xds_obj <- setup_time_away(ta, xds_obj, i=i)
  
  teir <- travel_eir[trait_ix]
  xds_obj <- setup_travel_eir(teir, xds_obj, i=i)

  membership = splits[trait_ix]
  xds_obj <- update_births_by_membership(membership, xds_obj, i)
  
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
  
  return(xds_obj)
}


#' @title Update XH time objects
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_time_away_by_strata = function(xds_obj, i=1){
  stopifnot(length(search_by_age) == xds_obj$XH_obj[[i]]$strata$age$N)
  # Update timespent matrix
  # Update time away
  return(xds_obj)
}


#' @title Update XH time objects
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_travel_eir_by_strata = function(xds_obj, i=1){
  # Update timespent matrix
  # Update time away
  return(xds_obj)
}
