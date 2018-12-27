## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  my_matrix <- NULL
  set <- function(y) {
    x <<- y
    my_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) my_matrix<<- inverse
  getinverse <- function() my_matrix
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_inverse <- x$getinverse()
  if (!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get()
  mat_inverse   <- solve(data, ...)
  x$setinverse(mat_inverse)
  mat_inverse
}
