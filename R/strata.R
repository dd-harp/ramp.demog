# Functions to set up and use the strata object

#' @title Setup strata
#'
#' @description Set up the membership list, if
#' it doesn't already exist
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
setup_dynamic_strata = function(xds_obj, i=1){
  with(xds_obj$XH_obj[[i]],{
    N = xds_obj$nStrata[1]
    if(!exists("strata")){
      strata = list() 
      strata$dim = 1
      strata$base = list(list(N=N, ix=1:N))
      strata$base$pars <- get_XH_pars() 
      strata$base$inits <- get_XH_ints() 
      xds_obj$XH_obj[[i]]$strata = strata
    }
    return(xds_obj)
  })}

#' @title Setup strata
#'
#' @description Set up the membership list, if
#' it doesn't already exist
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal 
#' @export
update_strata_by_membership = function(xds_obj, i=1){
  xds_obj <- make_strata_membership(xds_obj, i)
  xds_obj <- update_XH_inits_by_strata(xds_obj, i)
  xds_obj <- update_XH_pars_by_strata(xds_obj, i)
  xds_obj <- update_timespent_by_strata(xds_obj, i)
  xds_obj <- update_time_away_by_strata(xds_obj, i)
  xds_obj <- update_travel_eir_by_strata(xds_obj, i)
  xds_obj <- update_search_by_strata(xds_obj, i)
  xds_obj <- update_births_by_strata(xds_obj, i)
  xds_obj <- update_deaths_by_strata(xds_obj, i)
  return(xds_obj)
}

#' @title Get strata
#'
#' @description Get the membership lists
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns the strata
#' @export
get_dynamic_strata = function(xds_obj, i=1){
    xds_obj$XH_obj[[i]]$strata 
}

#' @title Make membership vectors
#' 
#' @description 
#' Using the information on `strata`, set up
#' the membership vector for every 
#' 
#' If called with `name = "as_is"`, the deaths
#' must be at `options$death_rates
#' 
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
make_strata_membership = function(xds_obj, i){
  with(xds_obj$XH_obj[[i]], {
    stopifnot(strata$dim>1)
    membership <- catcat(strata[1]$ix, strata[2]$ix)
    if(strata$dim>2)
      for(j in 3:strata$dim)
        membership <- catcat(membership, strata[j]$ix)
    
    for(j in 1:strata$dim)
      xds_obj$XH_obj[[i]]$strata[j]$membership = membership[,j]
    
    return(xds_obj)
  })}

#' @title Make a memberhsip matrix
#' 
#' @description
#' Create a membership matrix from a membership vector
#' 
#' @return the membership matrix, \eqn{M}
#' 
#' @export
make_membership_matrix = function(N, membership){
  M = matrix(0, N, length(membership))
  M[cbind(membership, 1:length(membership))]=1
  return(M)
}



