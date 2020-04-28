## Creating a pair of functions that can cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ini<- NULL
  set <- function(m) {
    x <<- m
    ini <<- NULL
  }
  get<- function(){
    x
  }
  setInverse<- function(inverse){
    ini<<- inverse
  }
  getInverse <- function() {
    ini
  }
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv) ) {
    message("retrieve cached data")
    return(inv)
  }
  
  ## Return a matrix that is the inverse of 'x'
  getData <- x$get()
  inv <- solve(getData) %*% getData
  x$setInverse(inv)
  inv
}