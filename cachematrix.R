## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix creates a special matrix:

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y   #asign values
    inverse <<- NULL #asign values
  }
  
  get <- function() x
  setinverse <- function(inverse) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}


## Write a short comment describing this function

#CacheSolve calculates the inverse of the matrix created before, but if it has been already calculated, it will not calculate it again and return it from the cache. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse #return the inverse of the original matrix
}
