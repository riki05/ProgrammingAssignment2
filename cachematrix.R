## The two functions shown here cache the inverse of a matrix


## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
  set<- function(y) {
    x <<- y
    inv <<- NULL
  }
  get<- function() x
  setinverse<- function(inverse) inv<<- inverse
  getinverse<- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function calculates the inverse of "matrix" or retrieves cached inverse if already calculated

cacheSolve <- function(x, ...) {
inv<- x$getinverse()
  if(!is.null(inv)){
    message (“Getting Cached Matrix”)
    return(inv)
  }
  data<- x$get()
  inv<- solve(data,...)
  x$setinverse(inv)
  inv
}
