## These are pair of functions which cache the inverse of a matrix. It uses the concept of lexical scoping. The first function
## makeCacheMatrix remembers the value of the matrix and its inverse once it is calculated by the cacheSolve function so that the need
## of repeating the calculation is eliminated.

## This function creates a special "matrix" object that can cache its inverse. It is a list containing a function to 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
    
      inv <- NULL
      set <- function(y){
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x,...)
    x$setinverse(inv)
    inv
}
