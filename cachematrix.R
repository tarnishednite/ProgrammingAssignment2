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
   ## set default value for the inverse
   inverse <- NULL

   ## set function - does not return 
   set <- function(value)
   {
      x <<- value 
      inverse <<- NULL
   }

   ## get function - returns the matrix
   get <- function() x

   ## set the inverse - does not return
   setinverse <- function(i) inverse <<- i

   ## get the inverse - returns the inverse value
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
   ## retrieve and check if cache is set
   i <- x$getinverse()
   if( !is.null(i) )
   {
      ## return cached result
      message("getting cached matrix solution")
      return(i)
   }
   ## cache was not set
   ## retrieve the matrix from the makeCacheMatrix
   data <- x$get()
   ## solve the matrix - passing in additional variables
   i <- solve(data, ...)
   ## set the cache and return the solution
   x$setinverse(i)
   i
}
