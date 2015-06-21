## R Programming Assignment 2: Lexical Scoping
##
## These functions will work together to cache the time consuming computation
## of the inverse of a matrix. The first time through the inverse will be
## calculated and will stored. When called again, as long as the matrix in 
## the calling function has not changed, the cached solution of the inverse
## will be returned instead of being re-computed.
##
## The makeCacheMatrix function creates an object that stores a matrix and
## caches its inverse.

makeCacheMatrix <- function(x = matrix()) {

## initialize the inverse matrix to NULL  
  inv <- NULL
## The set function is inside of the makeCacheMatrix to set the matrix inverse
  set <- function (y){
    x <<- y
    inv <<- NULL
  }
## The get function is inside of the makeCacheMatrix and gets the cached inverse
## matrix.
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get,
         setinverse=setinverse, getinverse=getinverse)
}


## This function will return the cached inverse matrix if it exists, otherwise
## it will compute the matrix, cache it and return it.

cacheSolve <- function(x, ...) {
  
## set the inv variable to the cached matrix.
##  If inv is not null, the cached matrix will be returned.  
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
## If the matrix has not already been cached, the variable will be set to null
## and execution will continue here. The matrix will be computed, cached and
## returned to the user.
  mat.data <- x$get()
  inv <-solve(mat.data, ...)
  x$setinverse(inv)
  return(inv)
}
