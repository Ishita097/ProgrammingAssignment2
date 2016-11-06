#makeCacheMatrix stores a matrix and a cached value of the inverse of its matrix.
makeCacheMatrix <- function(x = matrix()) {
  # holds the cached value,or NULL if nothing is cached
  # initially cache is set to NULL since nothing is cached.
  cache <- NULL
  
  #storing a matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    #cache is again set to NULL since a new value is given to the matrix.
    cache <<- NULL
  }
  
 #returns the above stored matrix
   getMatrix <- function() {
    x
  }
  
  # cache the given argument 
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # getting the cached value
  getInverse <- function() {
    cache
  }
  
  # return a list. Each element of the list is a function
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

#calculating inverse of the special matrix created by makeCacheMatrix

cacheSolve <- function(y, ...) {
        ## Return a matrix that is the inverse of 'y'
  inverse <- y$getInverse()
  # if its cached value exists then return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # else get the matrix
  data <- y$getMatrix()
  #caclulate the inverse 
  inverse <- solve(data)
  #store it in the cache
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}

