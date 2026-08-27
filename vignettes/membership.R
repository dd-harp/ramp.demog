## -----------------------------------------------------------------------------
catcat = function(A, B){
  if(is.null(dim(A))) AB <- catcat_1(A,B) 
  else AB <- catcat_n(A,B)
  return(AB)
}

catcat_1 = function(A, B){
  newA <- matrix(A, nrow=length(A), ncol = length(B), byrow = F)  
  newB <- matrix(B, nrow=length(A), ncol = length(B), byrow = T)  
  AB <- cbind(as.vector(newA), as.vector(newB))
  return(AB)
}

catcat_n = function(A, B){
  newA <- c()  
  for(i in 1:length(B)) 
    newA <- rbind(newA,A)
  newB <- matrix(B, nrow=dim(A)[1], ncol = length(B), byrow = T)  
  cbind(newA, as.vector(newB))
}

## -----------------------------------------------------------------------------
AB <- catcat(1:2, 1:3)
AB

## -----------------------------------------------------------------------------
ABC <- catcat(AB, 1:5)
ABC

## -----------------------------------------------------------------------------
ABCD <- catcat(ABC, 1:7)
ABCD

## -----------------------------------------------------------------------------
#' @title Create a strata transition matrix, \eqn{\cal M}
#' @description 
#' @param membership a vector describing the patch index for each habitat
#' @return the residence [matrix], denoted \eqn{\cal J} where \eqn{\left|\cal J\right|= n_p \times n_h}
#' @seealso see [setup_BLOOD_FEEDING]
#' @seealso see [view_residence_matrix]
#' @export
create_statification_matrix = function(A){
  M = matrix(0, max(A), length(A))
  M[cbind(A, 1:length(A))]=1
  return(M)
}

## -----------------------------------------------------------------------------
mA <- create_statification_matrix(AB[,1])

## -----------------------------------------------------------------------------
mB <- create_statification_matrix(AB[,2])

## -----------------------------------------------------------------------------
matrixA = matrix(c(0, .1, .1, 0),2,2)
matrixA

## -----------------------------------------------------------------------------
matrixB = matrix(c(0, .1, .05, .1, 0, 0.03, 0.05, .1, 0),3,3)
matrixB

## -----------------------------------------------------------------------------
matrixAB <- t(mA) %*% matrixA %*% mA + t(mB) %*% matrixB %*% mB
matrixAB 

