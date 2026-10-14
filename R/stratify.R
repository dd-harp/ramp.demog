
#' @title Replicate XH initial values 
#' 
#' @param N the number of replicates
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
rep_XH_inits_by_strata = function(N, xds_obj, i=1){
  base_inits <- get_XH_inits(xds_obj)
  for(j in 2:length(base_inits))
    base_inits[[j]] = as.vector(t(replicate(N, base_inits[[j]])))
  xds_obj <- change_XH_inits(xds_obj, i, base_inits)
  
  return(xds_obj)
}

#' @title Replicate XH parameters
#' 
#' @param N the number of replicates
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
rep_XH_pars_by_strata = function(N, xds_obj, i=1){
  
  base_pars <- get_XH_pars(xds_obj, i)
  for(j in 1:length(base_pars))
    base_pars[[j]] = as.vector(t(replicate(N, base_pars[[j]])))
  xds_obj <- change_XH_pars(xds_obj, i, base_pars)
  
  return(xds_obj)
}

#' Replicate time away
#' 
#' @param N the number of replicates
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
rep_time_away_by_strata = function(N, xds_obj, i=1){
  time_away <- get_time_away(xds_obj, i)
  new_time <- as.vector(t(replicate(N, time_away)))
  check_time_away(new_time, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$time_away_obj$time_away <- new_time
  xds_obj$XH_obj[[i]]$time_away <- new_time
  return(xds_obj)
}

#' Replicate travel eir
#' 
#' @param N the number of replicates
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
rep_travel_eir_by_strata = function(N, xds_obj, i=1){
  travel_eir <- get_travel_eir(xds_obj, i)
  new_eir <- as.vector(t(replicate(N, travel_eir)))
  check_travel_eir(new_eir, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$travel_eir_obj$travel_eir <- new_eir
  xds_obj$XH_obj[[i]]$travel_eir <- new_eir
  return(xds_obj)
}

#' Replicate death rates
#' 
#' @param N the number of replicates
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
rep_death_rates_by_strata = function(N, xds_obj, i=1){
  death_rates <- get_death_rates(xds_obj, i)
  new_dr <- as.vector(t(replicate(N, death_rates)))
  xds_obj$XH_obj[[i]]$death_obj$death_rates = new_dr
  return(xds_obj)
}

#' Replicate blood search weights 
#' 
#' @param N the number of replicates
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
rep_blood_search_weights_by_strata = function(N, xds_obj, i=1){
  blood_search_weights <- get_blood_search_weights(xds_obj, i)
  new_weights <- as.vector(t(replicate(N, blood_search_weights)))
  check_blood_search_weights(blood_search_weights, xds_obj$nStrata[i]) 
  xds_obj$XH_obj[[i]]$search_weights_obj$search_weights <- new_weights
  return(xds_obj)
}

#' @title Update the time spent matrix
#' 
#' @param N the number of replicates
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
stretch_timespent = function(N, xds_obj, i=1){
  Nh <-  dim(get_timespent_matrix(mod))[2]
  membership <- catcat(1:N, 1:Nh)[,2]
  xds_obj <- stretch_timespent_by_membership(membership, xds_obj, i)
  return(xds_obj)
}

#' @title Update the time spent matrix
#' 
#' @param membership the membership vector
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
stretch_timespent_by_membership = function(membership, xds_obj, i=1){
  old_timespent <- get_timespent_matrix(xds_obj, i)
  Nh <-  dim(old_timespent)[2]
  M = make_membership_matrix(Nh, membership)
  xds_obj$XH_obj[[i]]$timespent = old_timespent %*% M 
  return(xds_obj)
}
