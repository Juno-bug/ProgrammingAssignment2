## makeCacheMatrix creates a function that contains a list that sets the value
## of a matrix, gets the value of the matrix, and sets and gets the inverse of 
## the matrix, which is assigned to the object "i"

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function (y){
        x <<- y
        i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cashSolve calculates the inverse of the matrix returned by makeCasheMatrix 
## function. If the inverse was already calculated, then it is retrieved from 
## cache, if it isn't the function calculates the inverse of the matrix with the
## solve function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)){
        message("getting inversed matrix")
        return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$getinverse(i)
      i
}
