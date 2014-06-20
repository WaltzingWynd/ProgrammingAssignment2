## The goal of these two functions is to compute the inverse of a matrix and to
## cache the results so that if the same matrix is inputted, it will return that saved result.

## The function makeCacheMatrix creates a "matrix" object that can cache its inverse.
## Cache means to save the result of a program to memory so that if the same calculation is run again,
## the result is pulled from memory instead of recalculating.

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  ## set the value of the matrix
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverted matrix
  setsolve <- function(solve) m <<- solve
  
  ## get the value of the inverted matrix
  getsolve <- function() m
  
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
}


## The function cacheSolve will attempt calculate the inverse of the matrix returned by the function
## makeCacheMatrix.  If the inverse has already been computed then this function will retrieve the saved 
## result.

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) 
{
  m <- x$getsolve()
  
  ## check to see if result already exists.  If so, returns result from memory.
  if(!is.null(m)) 
  {
    message("getting cached data")
    return(m)
  }
  
  ## if result does not exist, compute the inverse of the matrix.
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  
## Return a matrix that is the inverse of 'x'
  m
}

data <- 1:4
row <- 2
col <- 2

x <- matrix(data, row, col)
x
solve(x)

v <- makeCacheMatrix()
v$set(matrix(data, row, col))
v$get()
cacheSolve(v)
