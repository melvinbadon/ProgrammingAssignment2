## Matrix inversion is usually a costly computation and their may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix provides the methods to for manipulatinfg the given matrix.
## cacheSolve returns the inverse of the matrix


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse by:
## 1. Variable matrix_inverse will store the inverse of matrix x
##    It is initially set to null.
## 2. Set the value of the special matrix
## 3. Get the value of the special matrix
## 4. Set the inverse value of the special matrix  
## 5. Get the inverse value of the special matrix  

  makeCacheMatrix <- function(x = matrix()) {
    matrix_inverse <- NULL
    set <- function(y)  {
        x <<- y
        matrix_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(solve) matrix_inverse <<- solve  
    get_inverse <- function() matrix_inverse
    list(set = set, get = get, 
       set_inverse = set_inverse, get_inverse = get_inverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## ()and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        matrix_inverse <- x$get_inverse()
          
        if(!is.null(m))   {
                message("getting cached data")
                return(matrix_inverse)
        }
        data <- x$get()
        matrix_inverse <- solve(data, ...)
        x$set_inverse(matrix_inverse)
        matrix_inverse
}
