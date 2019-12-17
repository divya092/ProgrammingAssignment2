## Functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    ## Initialize the variable for inverse property
    inv <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            inv <<- NULL
    }

    ## Method to get the matrix
    get <- function() {
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        inv
    }

    ## Return a list of the methods
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special matrix returned by "makeCacheMatrix" above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Return the inverse if its already set
    if( !is.null(m) ) {
            message("Retrieving cached data")
            return(m)
    }

    ## Retrieve the matrix from our object
    var <- x$get()

    ## Calculating the inverse using matrix multiplication
    m <- solve(var) %*% var

    ## Set the inverse to the object
    x$setInverse(m)

    ## Return the matrix
    m
}
