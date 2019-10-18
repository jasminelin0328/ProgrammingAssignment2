## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	    # Initialize the inverse property
        i <- NULL
        
        # method to set the matrix
        set <- function( matrix ) {
                x <<- matrix
                i <<- NULL
        }
        
        # method the get the matrix
        get <- function() {
                ## Return the matrix
                x
        }
        
        # method to set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        # method to get the inverse of the matrix
        getInverse <- function() {
                ## Return the inverse property
                i
        }
        
        # return a list of the methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        # return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        # return the inverse if its already set( if m isn't a null value)
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        
        # otherwise get the matrix from our object
        data <- x$get()
        
        # calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        # solve(data, default:b = identity matrix): x => data %*% x = identity matrix(if not defined)
        # x = -data*identity matrix = -data
        # m = -data %*% data = identity matrix
        # if there is no inverse matrix set, return the identity matrix as inverse
        
        # set the inverse to the object
        x$setInverse(m)
        
        # Return the matrix
        m
}
