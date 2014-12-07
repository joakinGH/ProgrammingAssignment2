## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The first function, makeCacheMatrix creates a special "matrix", 
# which is really a list containing several functions
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
#
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


a <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)))
cacheSolve(a)
cacheSolve(a)%*%a$get()