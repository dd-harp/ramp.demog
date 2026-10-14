#' @title Split by search_weights
#' 
#' @description
#' Using a base model, split each population stratum 
#' into new strata with modified search weights. 
#' The new weights are the product of the old search weights
#' and `search_facs` 
#'
#' 
#' @param splits the fraction in each
#' @param factors search weight modification factors 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @export
split_base_by_search_weights = function(splits, factors, xds_obj, i=1){
  
  Nw = length(splits)
  stopifnot(sum(splits)==1)
  stopifnot(length(factors)==Nw)
  
  Ns <- xds_obj$nStrata[i]
  xds_obj$nStrata[i] = Ns*Nw

  trait_ix <- catcat(1:Nw, 1:Ns)
  wts <- get_blood_search_weights(xds_obj, i)  
  M <- make_membership_matrix(Ns, trait_ix[,2])
  new_weights <- wts %*% M %*% diag(factors[trait_ix[,1]])
  check_blood_search_weights(new_weights, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$search_weights <- new_weights
  
  xds_obj <- rep_XH_inits_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_XH_pars_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_time_away_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_travel_eir_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_death_rates_by_strata(Nw, xds_obj, i)
  xds_obj <- stretch_timespent(Nw, xds_obj, i)
  xds_obj <- split_birth_rates(splits, xds_obj, i)
  xds_obj <- split_H_by_fracs(splits, xds_obj, i)
  xds_obj <- make_indices(xds_obj)
  xds_obj <- update_XY_interface(xds_obj)
  
  return(xds_obj)
}

#' @title Update search weights
#' 
#' @param membership the membership vector
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_search_weights_by_membership = function(membership, xds_obj, i=1){
  M = make_membership_matrix(membership)
  wts <- get_blood_search_weights(xds_obj, i)
  xds_obj$XH_obj[[i]]$search_weights = wts %*% M 
  return(xds_obj)
}

#' @title Modify search weights by age
#' 
#' @param search_by_age search weights by age
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_search_wts_by_strata = function(search_by_age, xds_obj, i=1){
  N = length(search_by_age)
  stopifnot(N == xds_obj$XH_obj[[i]]$strata$age$N)
  wts <- get_blood_search_weights(mod, i, s=1)
  wts <- as.vector(t(replicate(N, wts)%*% diag(search_by_age)))
  xds_obj <- setup_blood_search_weights(wts, xds_obj, i=i)
  return(xds_obj)
}

