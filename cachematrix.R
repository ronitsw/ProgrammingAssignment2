## Put comments here that give an overall description of what your
## functions do

## This function, makeChacheMatrix, creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set <- function(y){
    x <<- y
    p <<- NULL
  }
  get <- function()x
  setInv <- function(inv) p <<- inv
  getInv <- function() p 
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
  }

## This function, chacheSolve, is able to calculate the inverse of the matrix from makeCacheMatrix. 

cacheSolve <- function(x, ...) {
 
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInv()
  if(!is.null(p)){
    message("retrieving data...")
    return(p)
  }
  mat <- x$get()
  p <- solve(mat,...)
  x$setInv(p)
  p
}
