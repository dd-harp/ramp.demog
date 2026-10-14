
#' @title Set up deaths 
#' 
#' @description 
#' Set up the `D_matrix`
#' 
#' @inheritParams ramp.xds::setup_deaths
#' 
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_deaths.D_matrix = function(name, xds_obj, options=list(), i=1){
  if(is.matrix(name)){
    D_matrix = name 
  } else {
    D_matrix = options$D_matrix
  }
  
  N <- xds_obj$nStrata[i]
  stopifnot(dim(D_matrix) == c(N,N))
  
  xds_obj$XH_obj[[i]]$death_obj$D_matrix = D_matrix 
  class(xds_obj$XH_obj[[i]]$death_obj) = "D_matrix"
  
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
Deaths.D_matrix <- function(t, X, xds_obj, i){
  with(xds_obj$XH_obj[[i]]$death_obj,{
    return(D_matrix %*% X)
  })}
