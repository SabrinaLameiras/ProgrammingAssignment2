## Caching Matrix and calculating Inverse
## This functions is usefull to retain in memory Matrices
## and calculate inverses

## 1. makeCacheMatrix create a object based on a Matrix pre-existing
## Using: obj<-makeCacheMatrix(matrix)  to create object
##        obj$get()                     to get object
##        obj$getinverse()              to get inverse matrix of object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 2. cacheSolve calculate inverse matrix of object and set it to object
## Obs: if inverse matrix has been calculated previously, this function
##      just show it.
## Obs2: this function are using library MASS (function ginv above), from Brian Ripley et al.
##       see cran's page http://cran.r-project.org/web/packages/MASS/index.html
##       so, make sure you already installed and loaded this library
## Using: cacheSolve(obj)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Calculating inverse
  m <- ginv(data)
  x$setinverse(m)
  message("calculating inverse matrix for object")
  m
}
