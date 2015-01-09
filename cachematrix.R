## The following functions are used to create a special object that stores an
## invertible matrix and caches its inverse matrix

## The first function, makeCacheMatrix creates a special "matrix", which is
## really an array containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

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


## This function calculates the  inverse of the special "matrix" created with
## the previous function. First it will check if the inverse has already been 
## calculated, if it has, it will return the cached value, if not, the 
## function will calculate the inverse, store it in cache and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data))
  x$setsolve(m)
  m
}