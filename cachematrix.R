## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  # set the value of the matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  # get the inverse of the matrix
  getinv <- function() inv
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)  
}


## Write a short comment describing this function
# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # get the inverse of the matrix 
  inv <- x$getinv()
  
  # If the inverse is already calculated, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not: get the inverse of the matrix 
  data <- x$get()
  inv <- solve(data, ...)
  
  # Cache the inverse
  x$setinv(inv)
  
  # Return it
  inv
}

# Example 1:
# x <- matrix(rnorm(16), nrow = 4)
# cx <- makeCacheMatrix(x)
# cx$get()
# cacheSolve(cx)
# cacheSolve(cx)

# Example 2:
# a <- makeCacheMatrix()
# a$set(matrix(1:4,2,2))
# cacheSolve(a)
# cacheSolve(a)

