# Assignment 2 script

makeCacheMatrix <- function(x = matrix()) { # default matrix x
  inverse_local <- NULL
  set_local <- function(set_matrix) { # assign set_matrix to the parent environment
    x <<- set_matrix
    inverse_local <- NULL
  }
  get_local <- function() x
  get_inverse_local <- function() inverse_local
  set_inverse_local <- function(inverse_caculated) inverse_local <<- inverse_caculated
  #objects of type list() also allow access to any other objects defined in the environment of the original function
  list(set_local=set_local, get_local=get_local, 
       get_inverse_local=get_inverse_local,
       set_inverse_local=set_inverse_local)
}

cacheSolve <- function(makeCacheMatrix.object, ...) {
  i_local <- makeCacheMatrix.object$get_inverse_local()
  if(!is.null(i_local)) {
    message("getting cached data my friend")
    return(i_local)
  }
  my_matrix <- makeCacheMatrix.object$get_local()
  my_matrix.inverse.caculated <- solve(my_matrix)
  makeCacheMatrix.object$set_inverse_local(my_matrix.inverse.caculated)
  my_matrix.inverse.caculated # return the inverse value
}
