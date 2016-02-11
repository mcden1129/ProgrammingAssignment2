## cachematrix.R attempts to speed up the computation of matrix
## inverses by caching the value of inverse that has already
## been computed

## makeCacheMatrix creates a list containing functions to set and 
## get the values of the matrix and the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinv <- function(inverse){
    inv <<- inverse
  }
  
  getinv <- function(){
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## cacheSolve takes a matrix argument and try to look up in the
## cache to see if its inverse has already been computed. If so
## the inverse is retrieved and returned, otherwise, cacheSolve
## calls the solve function to compute the matrix inverse and
## store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        
        if(!is.null(inv)){
            message("getting cached data")
            return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
}
