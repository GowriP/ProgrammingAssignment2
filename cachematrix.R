## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Function to create a cached matrix
makeCacheMatrix <- function(x = matrix()) {
         inverse <- NULL
         set <- function(y) {
                 x <<- y
                 inverse <<- NULL
         }
         get <- function() x
         setinverse <- function() inverse <<- solve(x)
         getinverse <- function() inverse
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)

}


## Write a short comment describing this function
## Function to cache inverse matrix
cacheSolve <- function(x, ...) {
         inverse <- x$getinverse()
         if(!is.null(inverse)) {
                 message("getting cached data")
                 return(inverse)
         }
         data <- x$get()
         inverse <- mean(data, ...)
         x$setinverse(inverse)
         inverse
}
