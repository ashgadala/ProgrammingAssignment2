## This function creates a special "matrix" object that can cache its inverse called makeCacheMatrix
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

## The function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Check if xis a metrix
  if (!is.matrix(x)) {
    stop(" input should be a matrix, Choose matrix as input")
  }
  
  inv.x <- NULL
  
  set <- function(y) {
    x <<- y
    inv.x <<- NULL
  }
  
  # Getting and setting cached inv. matrix value
  get <- function() x
  # Inversing the matrix 
  #https://youtu.be/1QYdrMRhNJs?t=52
  #solve() function in R is a built in function that will invert the matrix
  set.inverse <- function(solve) inv.x <<- solve
  get.inverse <- function() inv.x
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  
  }
