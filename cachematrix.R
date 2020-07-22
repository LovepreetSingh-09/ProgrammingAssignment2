## Put comments here that give an overall description of what your
## functions do

## This function returns a list comprising of four functions 
# 1.) set, which is used to set the new value of the matrix
# 2.) get, which is used to retrieve the already existed value of the matrix
# 3.) setinv, which is used to set the new inverse of the matrix
# 4.) getinv, which is used to retrirve the already existed inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y){
    x <<- y
    inv_matrix <<- NULL
  }
  get <- function() x
  set_inv <- function(inv) inv_matrix <<- inv
  get_inv <- function() inv_matrix
  list(set=set, get=get, setinv=set_inv, getinv=get_inv)
}


## This function returns the inverse of a matrix. Firstly, it will check if the inverse
# is already calculated by using $getinv() function and if it exists, it will return 
# that cached inverse whereas, if inverse doesn't exist, then the inverse wil be
# caculated using solve() and that inverse will be set by using setinv() for later use.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)){
    print('Retrieving cached Inverse')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}

