# makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y    
      m <<- NULL 
    }
   
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get,setInverse = setInverse,  getInverse = getInverse)
  }

#cacheSolve: This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve will retrieve the 
#inverse from the cache.




cacheSolve <- function(x, ...) {
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
