
#' @title CatCat
#' 
#' @description 
#' Create and return a CatCat object with every combination
#' of `A` and `B`
#' 
#' @param A a vector or a CatCat object
#' @param B a vector
#'
#' @returns a matrix
#' @export
#'
#' @examples
#' a <- catcat(c(1:3), c(2:8))
#' b <- catcat(a, c(1:4))
catcat = function(A, B){
  if(is.null(dim(A))) AB <- catcat_1(A,B)
  else AB <- catcat_n(A,B)
  return(AB)
}

#' @title CatCat
#' 
#' @description 
#' Create and return a CatCat object with every combination
#' of `A` and `B`
#' 
#' @param A a vector
#' @param B a vector
#'
#' @returns a matrix
#' @keywords internal
#' @export
catcat_1 = function(A, B){
  newA <- matrix(A, nrow=length(A), ncol = length(B), byrow = F)
  newB <- matrix(B, nrow=length(A), ncol = length(B), byrow = T)
  AB <- cbind(as.vector(newA), as.vector(newB))
  return(AB)
}

#' @title CatCat
#' 
#' @description 
#' Create and return a CatCat object with every combination
#' of `A` and `B`
#' 
#' @param A a CatCat object
#' @param B a vector
#'
#' @returns a matrix
#' @keywords internal
#' @export
catcat_n = function(A, B){
  newA <- c()
  for(i in 1:length(B))
    newA <- rbind(newA,A)
  newB <- matrix(B, nrow=dim(A)[1], ncol = length(B), byrow = T)
  return(cbind(newA, as.vector(newB)))
}
