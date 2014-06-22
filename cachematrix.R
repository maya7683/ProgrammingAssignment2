## Two functions that cache the inverse of a matrix and retrieve the cached matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize inverse matrix
  m <- NULL
  
  # set the original and inverted matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the original matrix
  get <- function() x
  
  # set inverse matrix using solve() function
  setmatrix <- function(solve) m <<- solve
  
  # get cached inverse matrix
  getmatrix <- function() m
  
  # returns a list of functions
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not been changed),
## cacheSolve retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
  
  # tries to get the cached matrix
  m <- x$getmatrix()
  
  # returns inverse if cached matrix is already set
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  
  # calculates inverse matrix
  matrix <- x$get()
  m <- solve(matrix, ...)
  
  # caches the inverse matrix
  x$setmatrix(m)
  
  # returns the inverse
  m
  
}
