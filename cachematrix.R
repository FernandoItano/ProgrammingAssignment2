## These two functions can be used to calculate and cache the inverse of
## a square matrix, saving time and resources if recalculating is
## necessary.

## makeCacheMatrix creates a special matrix and four functions to cache 
## its inverse. The four functions are:
## 'set' set the value of the matrix
## 'get' get the value of the matrix
## 'setinverse' set the inverse of the matrix
## 'getinverse' get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y){
                    x <<- y
                    i <<- NULL
          }
          get <- function() x
          setinverse <- function(inverse) i <<- inverse
          getinverse <- function() i
          list(set = set, get = get, 
               setinverse = setinverse,
               getinverse = getinverse)
}

## cacheSolve returns the inverse matrix. If the computation was already done,
## simply returns the inverse matrix, otherwise it calculates the inverse matrix
## and caches it before returning.
cacheSolve <- function(x, ...) {
          i <- x$getinverse()
          if(!is.null(i)){
                    message("getting cached data")
                    return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinverse(i)
          i
          ## Return a matrix that is the inverse of 'x'
}