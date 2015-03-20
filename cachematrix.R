## makeCacheMatrix - a function to create a special "matrix" object that can cache 
##   the inverse of the square invertible matrix.

## cacheSolve - a function which computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix. If the inverse has already been calculated 
##   and the matrix has not changed, the inverse matrix will be retrived from 
##   the cache. Otherwise, the inverse matrix will be calculeted and saved in 
##   this special "matrix" object istead of the future calculations.  


makeCacheMatrix <- function(x = matrix()) {
  
  inv_m <- NULL
  
  ## a function to set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  
  ## a function to get the value of the matrix
  get <- function() x
  
  ## a function to set the value of the inverse matrix
  setinverse <- function(inverse) inv_m <<- inverse
  
  ## a function to get the value of the inverse matrix
  getinverse <- function() inv_m
  
  ## creating a special object which can cache the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}



cacheSolve <- function(x, ...) {
  
  ## retrieving the value of the inverse matrix from the cache
  inv_m <- x$getinverse()
  
  ## returning the value of the inverse matrix from the cache (when it differs 
  ## from NULL)
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  
  ## getting the value of the matrix to calculate its inverse form
  data <- x$get()
  ## calculating the inverse matrix
  inv_m <- solve(data, ...)
  ## setting the value of the inverse matrix to the cache
  x$setinverse(inv_m)
  
  ## returning the value of the inverse matrix
  inv_m
  
}
