## TASK SUMMARY: 
## 1) create makeCacheMatrix
## 2) input makeCacheMatrix into cacheSolve

## makeCacheMatrix creates a matrix, the inverse of which can be cached

## cacheSolve comuptes the inverse of the created matrix
## the inverse of the matrix can be retrieved from the cache

## this method is useful in time-consuming computations

makeCacheMatrix <- function(x = matrix()) {
  ## 1) set the matrix, 2) get the value of the matrix
  ## 3) set the inverse, 4) get the value of the inverse from the cache
  ## x is the matrix
  ## makeCacheMatrix returns a list of functions containing 1)-4)
  
  cminv_x <- NULL
  set <- function(y) {
    x <<- y
    cminv_x <<- NULL
  }                                                 #1
  get <- function() x                               #2
  
  setcminv<- function(inverse) cminv_x <<-inverse   #3
  getcminv <- function() cminv_x                    #4
  list(set = set, get = get,
       setcminv = setcminv,
       getcminv = getcminv)
  ## returns the created functions to the working environment
}

## cacheSolve first checks if the inverse of the matrix has been created already
## if available -- will be retrieved from the cache, calculations skipped
## if not available -- inverse of the object will be calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  cminv_x <- x$getcminv()
  if (!is.null(cminv_x)) {
    message("getting cached inverse matrix")
    return(cminv_x)
  } else {
    
    ## returns cached result if available, OR
    ## proceeds with calculations
    ## solve() computes the reverse of a matrix
    cminv_x <- solve(x$get())
    x$setcminv(cminv_x)
    return(cminv_x)
  }
}

