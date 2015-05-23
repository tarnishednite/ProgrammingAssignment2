## Assignment 2 RProgramming
##
## makeCacheMatrix - extends the matrix function of R to contain a inverse cache
##
## cacheSolve - returns the cached inverse when available, sets the inverse if 
##              it is not defined

## makeCacheMatrix
## extends the matrix functionality to cache the inverse of the matrix for 
## working with the inverse over multiple operations
##
## inputs 
## x: square matrix
##
## methods
## set: sets the value of the matrix and initializes the inverse
## get: gets the value of the matrix
## setinverse: sets the value of the inverse
## getinverse: gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
   inverse <- NULL
   set <- function(value)
   {
      x <<- value 
      inverse <<- NULL
   }
   get <- function() x
   setinverse <- function(i) inverse <<- i
   getinverse <- function() inverse
   list( set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse )
}


## cacheSolve
## calculates the inverse/solution of a square matrix and stores the matrix
## and inverse/solution in a cache structure
##
## inputs
## x: cache metrix
## ...: values to be passed to the solve() function

cacheSolve <- function(x, ...) 
{
   i <- x$getinverse()
   if( !is.null(i) )
   {
      message("getting cached matrix solution")
      return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
}
