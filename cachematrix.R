## Allows for the creation of a matrix, the calculation of the inverse 
## of the matrix, and the storing/retrieval of said inverse

## Initializes the variable
m <- NULL

## Creates a list of functions for the created matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Retrieves the cached value of the inverse of a matrix (if exists) or
## Calculates/stores the value of the inverse of a matrix (if does not exist)

cacheSolve <- function(x, ...) {
        ## Retrieving the inverse of matrix 'x' (if exists)
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        ## Calculating the inverse of matrix 'x' (if does not exist)
        m <- solve(data, ...)
        ## Recording the inverse of matrix 'x' (if did not previously exist)
        m <<- m
        ## Prints the value of the inverse of matrix 'x'
        m
}


