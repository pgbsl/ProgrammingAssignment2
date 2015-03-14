########################################################
# Create a CacheMatrix that allows the inverted matrix 
# to be stored in a cache.  This function does not 
# calculate the inverse, but if an inverse is evaluated,
# then it can be cached.  The cache is cleared if the 
# matrix is updated
########################################################

makeCacheMatrix <- function(mtx = matrix()) {
  invertedMtx <- NULL
  print(invertedMtx)
  set <- function(y) {
    mtx <<- y
    invertedMtx <<- NULL
    message("In set")
  }
  get <- function() {
    message("In get")
    print(mtx)
    mtx
  }
  
  setInvertedMatrix <- function(invMtx) invertedMtx <<- invMtx
  getInvertedMatrix <- function() invertedMtx
  list(set = set, get = get,
       setInvertedMatrix = setInvertedMatrix,
       getInvertedMatrix = getInvertedMatrix)
}

######################################################
# Given a CacheableMatrix, evaluates the inverse of 
# the matrix.  This inverse will be retrived from the 
# cache (held with the CacheableMatrix, if the inverse
# has already been caculated AND the matrix has not 
# changed, otherwise, the inverse will be evaluated
# (and then strored in the cache)
########################################

cacheSolve <- function(mtx, ...) {
  # Get the inverse matrix from the cache, if available
  invertedMtx <- mtx$getInvertedMatrix()
  
  if(!is.null(invertedMtx)) {
    # Cache value found.  return immediately
    message("getting cached data")
    return(invertedMtx)
  }
  
  # Data not cached, retrieve the (non-inverted) matrix..
  data <- mtx$get()
  
  # .. and evaluate the inverse
  invertedMtx <- solve(data, ...)
  
  # Finally, set the inverse matrix (in the cache) before returning it
  mtx$setInvertedMatrix(invertedMtx)
  invertedMtx
}
