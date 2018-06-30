##Functions for coding matrix inverse
makeCacheMatrix <- function( m = matrix() ) {
  
##Initialize the inverse 
  invm <- NULL
  
##Set the matrix
  set <- function( matrix ) {
    m <<- matrix
    invm <<- NULL
  }
  
##Get the matrix
  get <- function() {
    m
  }
  
##Set matrix inverse
  setInverse <- function(inverse) {
    invm <<- inverse
  }
  
##Get matrix inverse
  getInverse <- function() invm
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


##cacheSolve function computes the inverse of the matrix produced by the makeCacheMatrix function.
##If the inverse has already been found, then it will be retrieved from the cache. 

cacheSolve <- function(x, ...) {
  invm <- x$getInverse()
  
##Return inverse if set
  if(!is.null(invm) ) {
    message("getting cached data")
    return(invm)
  }
  
##Get matrix
  data <- x$get()
  
##Calculate inverse
  invm <- solve(data) %*% data
  
##Set inverse to object and get matrix
  x$setInverse(invm)
  invm
}