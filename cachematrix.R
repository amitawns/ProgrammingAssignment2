##function to create a list of functions to set, get the value of matrix 
##and to set and get value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL ##set inverse to null in the beginning
  
  set <- function(y) {
    x <<- y
    minv <<- NULL ##set inverse to null once matrix is updated 
  }
  
  get <- function() x  ##return the value of matrix
  setinv <- function(solve) minv <<- solve ##use solve function to get inverse of matrix
  getinv <- function() minv                ##return inverse of matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv) ## return the list with all 4 functions
}


## This function checks the cache to see if inverse of the matrix is availble
## If yes, prints from cache, else calculates inverse

cacheSolve <- function(x, ...) {

  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data") # print messsage it was cached
    return(minv) ## inverse matrix returned from cache
  }
  data <- x$get() ## gets the value of the matrix
  minv <- solve(data, ...) ## calculated inverse using solve function
  x$setinv(minv)## sets the value of inverse to just calculated
  minv## returns inverse matrix
}
