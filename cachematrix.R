  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## Write a short comment describing this function
  ##Create matrix with ability to cache itself
  
  makeCacheMatrix <- function(x = matrix()) 
    {
        inverse <- NULL
        set <- function(arg)
        {
          x <<- arg
          inverse <<- NULL
        }
  
        get <- function() x
        {
          setVal <- function(solveMatrix) inv <<- solveMatrix
          getVal <- function() inv
        }
        
       values <- list(set = set, get = get, setInverse = setVal, getInverse = getVal) 
       values 
    }
  
  
  ## Write a short comment describing this function
  ## Calculates the inverse of the given matrix x
  cacheSolve <- function(x, ...) 
  {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(is.null(inverse) == FALSE)
    {
      return (inverse)
    }
    else
    {
      values <- x$get()
      inverse <- solve(values)
      x$setInverse(inverse)
    }
    inverse
  }
