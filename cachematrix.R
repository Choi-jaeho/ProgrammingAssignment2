## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly
## makeCacheMatrix and cacheSolve implement caching the inverse of a matrix

## makeCacheMatrix function creates a special "Matrix"
## that can cache its inverse.
## It is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) m <<- inverse
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve function computes the inverse of  the special "matrix"
## returned by makeCacheMatrix function
## If the inverse has already been calculated(and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setsolve(m)
  m
}
