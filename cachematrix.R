## This module demonstrates the use of lexical scoping to cache 
## the result of a function, in this case `solve` which calculates
## the inverse of a matrix.
##
## A better solution is to use the `memoise` package, which
## uses the same technique but has a much more usable interface,
## and was written and debugged by an experienced R programmer.
##
# example:
#  y = matrix(c(1,1,0,2),nrow=2,byrow=TRUE)
#  yc = makeCacheMatrix(y)
#  yi = cacheSolve(yc)
#  identical(round(yi%*%y,6),diag(2))
#
## cacheSolve(yc) will only calculate the inverse once.
## to clear the cache:
#
# yc$set(NULL)
#
##
##
## The memoise equivalent is
#
# library(memoise)
# if(!is.memoised(solve)){
#   solve <- memoise(base::solve)
# }
#
# y = matrix(c(1,1,0,2),nrow=2,byrow=TRUE)
# yi = solve(y)
# identical(round(solve(yi)%*%y,6),diag(2))
#
## Again, solve only actually does the inverting once.
##
## The biggest advantage here is that the you don't need
## to change your code to use memoise, just add the little
## preamble and enjoy the benefits.
##
## to clear the cache:
#
# forget(solve)
#


## This "object constructor" creates a closure in which you can store
## the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cache <<- inverse
  getinverse <- function() cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function inverts the matrix if necessary, and caches the result.
## Later calls will return the previously computed value.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## Test the above code
y = matrix(c(1,1,0,2),nrow=2,byrow=TRUE)
ym = makeCacheMatrix(y)
yi = cacheSolve(ym)
identical(round(cacheSolve(ym)%*%ym$get(),6),diag(2))
