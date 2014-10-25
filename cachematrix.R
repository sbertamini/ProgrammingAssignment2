## makeCacheMatrix is a function that creates a special "matrix" object 
## that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL # Initialize the inverse matrix to NULL value when calling makeCacheMatrix
  
  set <- function(y) {
    x <<- y # change the matrix that must be inverted
    INV <<- NULL  # previous inverted matrix result has to be cleared  
  }
  
  get <- function() {x} # return the matrix that must be inverted
  inverse <- function(invM) {INV <<- invM} # update the current value of the inverse matrix
  getINV <- function() {INV} # return the inverse matrix
  
  list(set = set, get = get, inverse = inverse, getINV = getINV)
}



## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache


cacheSolve <- function(x, ...) {
  INV <- x$getINV() # get the inverse matrix of x from the cached memory (if present)
  
  if(!is.null(INV)){ # check if the inverse has been calculated
    message("getting cached data")
    return(INV) # return of the cached value
  }

  INV <- solve(x$get(), ...) # calculate the inverse
  x$inverse(INV) # update the value of the inverse matrix in the parent enviroment
  INV # return the inverse matrix
}
