## Together these two functions allow the user to calculate the inverse of a matrix quickly
## by saving the inverted matrix in the global environment and looking for it there rather 
## than recalculating it every time.

## The first function, makeCacheMatrix, takes a user-defined matrix ('x' as an example) and returns a list of four functions
## x$set allows the user to set the elements of the  matrix
## x$get returns the inverse of that function
## x$setinverse solves (inverts) the matrix and sets its value in the global environment (cache)
## x$getinverse returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve is a function that returns the inverse of the matrix x defined in makeCacheMatrix
## cacheSolve first checks cached data for the inverse of x so that it
## does not require recalculating the inverse. If cacheSolve does not find the inverse of x in 
## the cache it calculates it and returns it.

cacheSolve <- function(x, ...) {
      m <- x$getinverse
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
