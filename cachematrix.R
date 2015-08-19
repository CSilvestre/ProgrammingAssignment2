## Name: CSilvestre
## Date: August 2015
## Thanks to the example given (makeVector, cachemean) and to 
## https://class.coursera.org/rprog-031/forum/thread?thread_id=579 in special to Robert Hadow
## was possible to make the task and was more ease to understand and to test the two functions


## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.


makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix A created with the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if not, it computes, caches, and returns it.
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}


## Was tested with:
## m<-matrix(c(1,2,0,4,5,0,7,8,9),3,3)
## n<-matrix(c(9,8,7,6,5,0,3,2,0),3,3)
## t1<-makeCacheMatrix(m)
## t2<-makeCacheMatrix(n)
## cacheSolve(t1)
## cacheSolve(t2)
## cacheSolve(t1)
## cacheSolve(t2)