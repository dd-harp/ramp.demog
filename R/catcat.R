
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
