## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix -
## This function creates a special "matrix" object that can cache its inverse.
##
## cacheSolve
## Return a matrix that is the inverse of 'x'


## creates a cacheable matrix, ie. one that
## will cache it's inverse and return that if
## possible.  This will hold true as long as
## set() is not called.
makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set <- function(y) {
    x<<- y
    inv<<- NULL
  }
  get <- function() x 
  setinv<- function(i) inv<<- i
  getinv<- function() inv
  list(set = set, get = get,
       setinv= setinv,
       getinv= getinv)
}


## given a cacheMatrix, solves for the inverse
## matrix.  Will check the cached result first.
cacheSolve <- function(x, ...) {
  if (!is.null(x$getinv()))
  {
    message("using cached result")
    return(x$getinv())
  }
  
  x$setinv(solve(x$get()))
  x$getinv()
}
