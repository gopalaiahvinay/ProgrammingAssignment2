## Put comments here that give an overall description of what your
## functions do

#Global Matrix
values = c(1,5,4,3,2,6,7,5,0)
A = matrix(values,3,3)
# Minor and cofactor to calculate Adjoint Matrix
minor <- function(A, i, j) det( A[-i,-j] )
cofactor <- function(A, i, j) (-1)^(i+j) * minor(A,i,j)

cacheA = NULL;
## Write a short comment describing this function


#Just returns the cache value 
makeCacheMatrix <- function(x = matrix()) {
  
      if(!is.null(cacheA)){
        print("Fetching from cache")
        return(cacheA)
      }
      else{
        
        cacheA = cacheSolve(A);
        
      }
  

}

#function to calculate only adjoincy
adjoint1 <- function(A) {
  n <- nrow(A)
  B <- matrix(NA, n, n)
  for( i in 1:n )
    for( j in 1:n )
      B[j,i] <- cofactor(A, i, j)
  B
}



## Write a short comment describing this function
#Computes the inversion
cacheSolve <- function(A) {
        ## Return a matrix that is the inverse of 'x'
    
  #Adjoint of A 
  adjA = adjoint1(A);
  #Inverse determinant of A
  detInvA = 1/det(A)
  
  #Inverse Matrix 
  invA = detInvA * adjA
  return(invA)
  
}

result = makeCacheMatrix(A)



