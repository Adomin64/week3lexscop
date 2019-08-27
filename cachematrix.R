
# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.

# Below are a pair of functions that are used to create a special object that 
# stores a matrix and caches its inverse.

#The "makeCacheMatrix" function creates a special array object,

makeCacheMatrix <- function(x = matrix()) {
  
  inverse_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_x <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_x <<- inverse
  getinverse <- function() inverse_x
  list(set = set,
       get = get,
       setinverse = setinverse ,
       getinverse = getinverse)
  
}

#The "cacheSolve" function calculates the inverse of the array.

cacheSolve <- function(x, ...) {
  inverse_x <- x$getinverse()
  if(!is.null(inverse_x)) {
    message("Getting cached data.")
    return(inverse_x)
  }
  data <- x$get()
  inverse_x <- solve(data)
  x$setinverse(inverse_x)
  inverse_x
}
