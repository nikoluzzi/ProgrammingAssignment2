## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing functions to set and get the value of a matrix
## and to set and get its inverse value( but not calculate it, just storinga)

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL ## creating the variable i to store the inverse matrix
      set <- function(y) { 
            x <<- y ## assign y to x in its parent environement
            i <<- NULL ## reset the inverse matrix (from parent environment)
      }
      get <- function() x ## get the value of x
      setinverse <- function (inverse) i <<- inverse ## store the inverse in its parent envrionment
      getinverse <- function () i ## gets the value i (from parent environment)
      list(set = set, get = get, 
           setinverse = setinverse,
           getinverse = getinverse) ## create a list to access the functions from outside.
}


## cacheSolve first check if the inverse matrix of x exists and display it
## if the inverse matrix has never been calculated, it calculates it and cache it, 
## using functions of makeCacheMatrix

cacheSolve <- function(x, ...) {
      i <- x$getinverse() ## get the value stored in makeCacheMatrix
      if(!is.null(i)) { ## if the value is not null, we will just displaying it.
            message("getting cached data")
            return(i)
      }
      data <- x$get() ## else, we will get the value of the matrix and store it in the variable 'data'
      i <- solve(data, ...) ## then we solve it and store it in i
      x$setinverse(i)  ## the result is sent as an argument to makeCacheMatrix to cache it.
      i  ## Return a matrix that is the inverse of 'x'
      
}
