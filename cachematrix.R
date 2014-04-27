## This function creates a special "matrix" object that 
## can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    # Sets/Updates the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # Gets the value of the matrix
    get <- function() x
    
    # Caches the matrix
    cacheMatrix <- function(matrix) m <<- matrix
    
    # Gets the cached matrix
    getCachedMatrix <- function() m
    
    # Return a list of functions
    list(set = set, get = get,
         cacheMatrix = cacheMatrix,
         getCachedMatrix = getCachedMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function above.
cacheSolve <- function(x, ...) {
    # Get the cached inverted matrix
    invertedMatrix <- x$getCachedMatrix()
    
    if (!is.null(invertedMatrix)) {
        message("getting cached matrix")
        return(invertedMatrix)
    }
    
    matrix <- x$get()
    
    # Invert the matrix
    invertedMatrix <- solve(matrix)
    
    # cache the invertedMatrix
    x$cacheMatrix(invertedMatrix)
    
    # Return inverted cached matrix
    invertedMatrix
}