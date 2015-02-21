## Put comments here that give an overall description of what your
## functions do

## Basic usage scenario example
## ma<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
## macache<-makeCacheMatrix(ma)
## cacheSolve(macache)
## If 'ma' has been previously solved - cacheSolve(macache) will return  the inverted matrix from cache
## Otherwise cacheSolve(macache) solves 'ma', saves inverted matrix in cache and returns the same


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix (x = matrix())
  ## Takes a matrix as an input
  ## and creates a list of 4 functions
  ## get() - returns matrix currently stored in "cache"
  ## set(y = matrix()) - replaces matrix currently stored in cache with 'y' and 
  ## clears the storage of inverted matrix, i.e. brings getSolve() to NULL
  ## setSolve(solve = matrix()) - replaces inverted matrix currently stored in cache with 'solve'
  ## getSolve() - returns inverted matrix currently stored in 'cache'

  s <- NULL
  set <- function(y = NULL) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve = NULL) s <<- solve
  getSolve <- function() s
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Takes a list created with makeCacheMatrix(x)    
    ## Return a matrix that is the inverse of 'x'
    ## If 'x' has been previously solved - returns  the inverted matrix from cache
    ## Otherwise solves 'x', saves inverted matrix in cache and returns the same
    
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}

