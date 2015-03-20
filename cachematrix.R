  ## Put comments here that give an overall description of what your
## functions do

# Create a matrix that will have the ability to store its inverse.
# If a value is not passed into this function; then the matrix
# will default to an empty matrix.
makeCacheMatrix <- function(mtrx = matrix()) {
  # Create a place to store the inv matrix.
  invMatrix <- NULL
  set <- function(inMatrix) {
    # The <<- assignment operator reaches into the parent env to set the 
    # mtrx value to the newly given parameter 'inMatrix'
    mtrx <<- inMatrix
    # The previous inverse matrix is invalid; reset it to null
    invMatrix <<- NULL
  }
  # retrieve the stored matrix;
  get <- function() {
    mtrx
  }
  # store the calculated inverse matrix.
  setInverse <- function(mtrxParam) {
    invMatrix <<- mtrxParam
  }
  # retrieve the stored inverse matrix.
  getInverse <- function() {
    invMatrix
  }
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

# This look for the inverse matrix in the object created
# by makeCacheMatrix.  If the stored matrix is present
# then the code will return that value otherwise it will
# calculated the inverse matrix and store that value.
cacheSolve <- function(cacheMatrix, ...) {
  invMatrix <- cacheMatrix$getInverse()
  if (is.null(invMatrix)) {  # If NULL, calculate the value.
    origMatrix <- cacheMatrix$get()  # matrix we want the inverse of.
    invMatrix <- solve(origMatrix, ...)  # Inverse matrix calculation.
    cacheMatrix$setInverse(invMatrix) # store in our object.
  } else {
    # A message for clarity; not really needed for functionality.
    message("We found a hit in our store, return that inverse matrix")
  }
  return(invMatrix)  # I really prefer one exit from a function when possible.
}
