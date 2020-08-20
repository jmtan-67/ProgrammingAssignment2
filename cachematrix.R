## makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## if the inverse has been calculated, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    print("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
