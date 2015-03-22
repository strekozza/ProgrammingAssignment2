## Script for caching of the calculated inverse matrix for a provided matrix.
## makeCacheMatrix creates an object containing data and functions for setting
## and getting both data and the inverse matrix.
## cacheSolve gets the cached inverse matrix if it is already available in cache, 
## otherwise the inverse matrix will be calculated.

## NOTE: Inverse matrices can be calculated for regular matrices only.
## According to the requirements of this assignment it's assumed,
## that every matrix provided to the makeCacheMatrix function 
## is invertible.


## Create and return an object containing provided matrix data
## and functions for setting and getting of data and inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get,
	     setsolve = setsolve,
	     getsolve = getsolve)
}


## Return the inverse matrix for the provided matrix.
## Return the cached inverse matrix if available,
## otherwise calculate the inverse matrix,
## put it into the cache and return the inverse matrix.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

      s <- x$getsolve()
      if (!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
