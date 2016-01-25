## Put comments here that give an overall description of what your
## functions do
##  These functions provide the inverse of a matrix with caching. If the inverted matrix has 
##  already been calculated, the function returns the cached result without recalculating. If the 
##  inverted matrix has not been calculated, the inverted matrix is then calculated, cached, and 
##  returned.
## Write a short comment describing this function
##  This function returns the response to the inverted matrix request. It calls the underlying
##  function to calculate the inverted matrix if necessary.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) m <<- invert
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## Write a short comment describing this function
##  This function calculates the inverted matrix and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- invert(data, ...)
  x$setinvert(m)
  m
}
