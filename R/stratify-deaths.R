#' @title Update death rates
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_deaths_by_membership = function(membership, xds_obj, i=1){
  M = make_membership_matrix(membership)
  deaths <- xds_obj$XH_obj[[i]]$death_obj$death_rates
  xds_obj$XH_obj[[i]]$death_obj$death_rates <- deaths %*% M
  return(xds_obj)
}

#' @title Update XH time ob jects
#' 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
update_deaths_by_strata = function(xds_obj, i=1){
  # Update timespent matrix
  # Update time away
  return(xds_obj)
}


#' @title Human/host deaths
#'
#' @inheritParams ramp.xds::Deaths
#' 
#' @return mortality rate for \eqn{X}
#' 
#' @keywords internal
#' @export
Deaths.strata <- function(t, X, xds_obj, i){
  with(xds_obj$XH_obj[[i]]$death_obj,{
    return(D_matrix %*% X)
  })}

#' @title Set up death rates
#' 
#' @description 
#' Pass pre-configured constant death rates. If it passes 
#' the checks, it replaces the current death rates.
#' 
#' If called with `name = "as_is"`, the deaths
#' must be at `options$death_rates
#' 
#' @inheritParams ramp.xds::setup_deaths
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_deaths.strata = function(name, xds_obj, options=list(), i=1){
  xds_obj <- make_membership(xds_obj, i)

  D_matrix = with(xds_obj, matrix(0, nStrata[i], nStrata[i]))
  for(j in 2:strata$dim){
    M = create_statification_matrix(strata[j]$membership)
    D_matrix = D_matrix + t(M) %*% strata[j]$matrix %*% M 
  }
  xds_obj <- setup_deaths(D_matrix, xds_obj, i=i)
  return(xds_obj)
}
