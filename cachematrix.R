## Put comments here that give an overall description of what your
## functions do


# 1.
## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #GET the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #GET the value of the matrix
  get <- function() x
  #SET the value of the inverse 
  setinv <- function(inv) m <<- inv
  #GET the value of the inverse 
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# 2.
## This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
## (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse 
## from the cache.

## It returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inv(data, ...)
  x$setinv(m)
  m   
}

################################
# how to get inverse & an example
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
A

AI  <- inv(A)
AI
