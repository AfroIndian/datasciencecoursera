## The functions in this file contains methods for caching variables  
## and calculating matrix inverse's

## Caching of a matrix variable for reuse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
 
      set <- function(y) {

        x <<- y
   
        m <<- NULL
    
      }


      get <- function() x

      setmatrix <- function(matrix) m <<- matrix

      getmatrix <- function() m

      list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Calulate the inverse of a function (using the Solve method)

cacheSolve <- function(x, ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
