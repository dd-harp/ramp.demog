
#' @title Update birth rates by membership
#' 
#' @param membership the age class membership vector
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_birth_rates_by_age = function(membership, xds_obj, i=1){
  ix = which(membership_by_age>1)
  membership_by_age[ix] = 0 
  M = make_membership_matrix(membership)
  births <- xds_obj$XH_obj[[i]]$birth_obj$base_birth_rates
  xds_obj$XH_obj[[i]]$birth_obj$birth_rates <- births %*% M
  return(xds_obj)
}

#' @title Split birth rates 
#' 
#' @description Split the birth rates 
#'  
#' 
#' @param splits the splits
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
split_birth_rates = function(splits, xds_obj, i=1){
  Nh <- length(get_birth_rates(xds_obj,i))
  Ns <- length(splits)
  mvec = catcat(1:Ns, 1:Nh)
  M = make_membership_matrix(Nh, mvec[,2]) %*% diag(mvec[,1])
  births <- xds_obj$XH_obj[[i]]$birth_obj$birth_rates
  xds_obj$XH_obj[[i]]$birth_obj$birth_rates <- births %*% M
  return(xds_obj)
}

