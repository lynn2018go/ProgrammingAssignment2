##These two functions that cache the inverse of a matrix 
## are able to cache potentially time-consuming computations. 


## 1,makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##define function with matric as default args
  
  makeCacheMatrix <- function(x = matrix()) {
  
#initialize null
 i <- NULL                           
     #assign set, get, setInverse and getInverse functions
    set <- function(y) {
       x <<- y
      i <<- NULL
       }
      get <- function() x
     setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
     #next line allows for $ operators
       list(set = set, 
            get = get,
            setInverse = setInverse, 
            getInverse = getInverse)
  }
  

## 2.cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
  
    cacheSolve <- function(x, ...) {		  cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 'x'		 
          ## Return a matrix that is the inverse of 'x'
          i <- x$getInverse()
          #retrieve from cache if available
            if(!is.null(i)){
                message("getting cached data")
               return(i)
              }
          data <- x$get()
         i <- solve(data, ...)
          x$setInverse(i)
         i
         ## Return a matrix that is the inverse of 'x'
    }
