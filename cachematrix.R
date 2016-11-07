## These are two functions which can cache the inverse of a matrix

## This function creates a specical matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Define the 'invs' variable
    invs <- NULL
    set <- function(y){
        x <<- y
        invs <<- NULL
    }
    # Return data to 'get'
    get <- function() x
    # Set inverse to 'invs'
    setinvs <- function(inverse) invs <<- inverse
    # Return the result 'invs'
    getinvs <- function() invs
    # Return a list containing four elements which are four functions defined before
    list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## This function computes the inverse of the specical "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
    # Call the function getinvs() defined in makeCacheMatrix
    invs <- x$getinvs()
    # Check if the matrix has been computed already or not
    if(!is.null(invs)){
        message("getting cached matrix")
        return(invs)
    }
    # Compute the inverse of matrix
    data <- x$get()
    invs <- solve(data,...)
    x$setinvs(invs)
    invs
}
