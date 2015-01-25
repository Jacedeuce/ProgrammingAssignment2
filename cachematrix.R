## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can store its inverse in a "cache"

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL   ## sets default of inv to NULL
    y <- NULL     ## sets default of y to NULL
    setmatrix <- function(y) {    ## function to set x to y and reset inv to null
        x <<- y       ## stores the value of y in x
        inv <<- NULL  ## flushes the cache when a new matrix is set
    }
    getmatrix <- function() x     ## returns the matrix x
    setinverse <- function(inverse) inv <<- inverse  ##sets value of inv to the inverse of the matrix
    getinverse <- function() inv    ## returns the inverse
    list(setmatrix = setmatrix, ## creates a "list" of the functions
         getmatrix = getmatrix, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, y = NULL, ...) { ## Return a matrix that is the inverse of 'x', optionally can pass original matrix to test for changes(y)
    if(!is.null(y)){  ## sets y to be an optional argument
        if(is.matrix(y)){  ## ensures that optional y is matrix
            if(!identical(x$getmatrix(), y)){  ## checks for matrix changes and updates inverse if there are any
              x$setmatrix(y)
              message("Matrix has changed, recalculating inverse.")
            }
        }
        else{
            message("'y' must be a matrix, using previous cache.") ## warning if optional argument is not a matrix
        }
    }
        
    inv <- x$getinverse()  ## returns the value of inv from makeCacheMatrix
    if(!is.null(inv))  {   ## checks to see if the inv has been calculated
          message("getting cached data...")
          return(inv)   ## returns the cached matrix
    }  
    else{             ## calculates the inverse of the matrix if the cache is NULL and stores it in inv
    data <- x$getmatrix() ## imports the original matrix to the object data
    inv <- solve(data, ...)  ## calculates the inverse and stores the result in inv
    x$setinverse(inv)  ## caches the result
    }
    inv   ## prints the inverted matrix
}