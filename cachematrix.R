## Put comments here that give an overall description of what your
## functions do

# The below functions together calculates and stores the inverse of a matrix in a cache so that it is easily retrieved

## Write a short comment describing this function

# This function is used to store the input matrix so that it could be returend on a function call
# This function also stores the inverse of the matrix so that it is easily retrieved during the successive call function

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
# First for the given matrix the function tries to check whether the inverse is cached using getinverse
# If the cache is available then that is pulled and returned as output
# If the cache is not available then the inverse is calculated with SOLVE function
# The calculated inverse matrix is then assigned to the global variable using setinverse function
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


#my_matrix <- matrix(c(1,2,3,4),2,2)

#cache_matrix <- makeCacheMatrix(my_matrix)
#inverse_matrix <- cacheSolve(cache_matrix)
#inverse_matrix