## Assignment:- Caching the inverse of a matrix
## Matrix  inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly

## As per the question this function will create a special "matrix" 
## object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invrs <- NULL
  set <- function(y){
    
    x <<- y
    invrs <<- NULL
    
  } 
  get <- function() x
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list(set = set,
       get = get,
       setInverse = setInverse
       getInverse = getInverse)
  
}


## As per the question this function below function will compute 
## the inverse of the special "matrix" created above. If the inverse has
## already been calculated, then it will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if(!is.null(invrs)){
      
      message("Getting CACHED Data")
      return(invrs)
    
  }

  mat <- x$get()
  inv <- solve(mat,...)
  x$setInverse(invrs)
  invrs
  
}
