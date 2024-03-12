## FUNCTIONS - 1) makeCachedmatrix          2) cacheSolve


makeCacheMatrix <- function(x = matrix()) {
##make cachematrix function is used to gebnerate a cached matrix using x matrix as an object. it caches the inverse of the matrix x   
  cacheMatrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  getMatrix <- function() x
  setCache <- function(inverse) cacheMatrix <<- inverse
  getCache <- function() cacheMatrix
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
}

cacheSolve <- function(x, ...) {
## This function is used to compute the inverse of the makeCacheMatrix function's output 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
  cacheMatrix <- x$getCache()
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
}
