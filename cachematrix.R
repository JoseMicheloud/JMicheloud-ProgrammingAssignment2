## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#the function create a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  TO.INVERSE <- NULL
  set <- function(y) {
    x <<- y
    TO.INVERSE <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) TO.INVERSE <<- inverse
  get_inverse <- function() TO.INVERSE
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## Write a short comment describing this function
# The function calculate the inverse of tre matrix created with de above function (makeCacheMatrix).
# In the case that the matrix was previously inversed, and the matrix no change, the function
# can retrieve the result from the cache

cacheSolve <- function(x, ...) {
  TO.INVERSE <- x$get_inverse()
  if(!is.null(TO.INVERSE)) {
    message("getting inverse matrix data")
    return(TO.INVERSE)
  }
  MATR <- x$get()
  TO.INVERSE <- solve(MATR, ...)
  x$set_inverse(TO.INVERSE)
  TO.INVERSE
}
