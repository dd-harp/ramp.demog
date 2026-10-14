
#' @title Split H by age
#' 
#' @param mu_a survival by age
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
split_H_by_age = function(mu_a, xds_obj, i=1){
  H <- get_H(xds_obj, i)
  surv_a = c(1,cumprod(1-mu_a), 0)
  props_a = -diff(surv_a)
  return(split_H_by_fracs(props_a, xds_obj, i))
}

#' @title Split H by fracs
#' 
#' @param fracs the fractions 
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
split_H_by_fracs = function(fracs, xds_obj, i=1){
  H <- get_H(xds_obj, i)
  H_new =  as.vector(t(replicate(length(fracs), H)%*%diag(fracs)))
  xds_obj$XH_obj[[1]]$inits$H = H_new
  return(xds_obj)
}
