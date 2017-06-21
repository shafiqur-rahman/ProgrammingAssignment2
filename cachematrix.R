## Put comments here that give an overall description of what your
## functions do

## This function is returning a list of function for getting and setting values to the special Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function will return the inverse of the matrix.
## It will fisrt look into the chache if the inverse is already in the cache or not.
## If it get the inverse in the cache then it will return the cache. Otherwise compute the 
## Inverse of matrix and put it into the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
