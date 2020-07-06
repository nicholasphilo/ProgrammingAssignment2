## These pair of functions allow you to calculate the inverse of a matrix (a computer intensive task)
## and then cache the resultant matrix so that the result can be called rather than recalculating.
## makeCacheMatrix caches the result and cacheSolve checks if the cache is usable and returns an inverse matrix



## makeCacheMatrix produces a list of 4 functions:
## set function allows you to set or change the matrix in its parent environment (makeCacheMatrix)
## get allows you to retrieve the x matrix from in its parent environment
## setinverse and getinverse let you change and save the value of an inverse matrix

makeCacheMatrix <- function( x = matrix() ) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## cacheSolve recieves the list output from makeCacheMatrix along with other arguments
## It checks to see whether the inverse of the same matrix has already been calculated.
## If it hasn't then the function calculates the inverse, then caches it and returns the inverse.
## If it has then then it prints "getting cached inverse data" and returns the cached inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
