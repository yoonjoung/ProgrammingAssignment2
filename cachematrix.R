## there are two functions. 
## 1. makeCacheMatrix 
## It creates a list of four objects. "setinv" calculates inverse.
## 2. casheSolve
## It calls saved (or cached) inverse already calculated for a matrix, which is defined with "set"

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
  setinv <- function(inv) m <<- solve
  #GET the value of the inverse 
  getinv <- function() m
  list(set = set, 
       get = get,
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
  m <- solve(data, ...)
  x$setinv(m)
  m   
}

################################
# example of a matrix and its inverse 
A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)
A

AI  <- solve(A)
AI

################################
# SOLUTION for the assignment 

aMatrix <- makeCacheMatrix(1:10)
aMatrix$get()       # retrieve the value of x
aMatrix$getinv()    # retrieve the value of m, which should be NULL

aMatrix$set(A)      # reset x with a new matrix A
aMatrix$getinv()    # still Null   

aMatrix$set(A)          # reset x with a new matrix A
cacheSolve(aMatrix)     # inverse calculated for the matrix A, not x - from the saved/cashed values. Check if it's same with AI
AI