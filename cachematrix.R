## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

  r <- NULL
  set <- function(y) {
    x <<- y
    r <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) r <<- inverse
  getinverse <- function() r
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.


cacheSolve <- function(x, ...) {
  r <- x$getinverse()
  if(!is.null(r)) {
    message("getting cached data")
    return(r)
  }       ## end for IF
  data <- x$get()
  r <- solve(data)
  x$setinverse(r)
  r
} 
## Return a matrix that is the inverse of 'x'

