## This functions cache a matrix and then calculate de inverse


## This function create a cache matrix and save a list with the operation

makeCacheMatrix <- function(x = matrix()) {

    mat_inv <- NULL
    set <- function(y) {
        x <<- y
        mat_inv <<- NULL
    }
    get <- function() x
    set_inv <- function(inverse) mat_inv <<- inverse
    get_inv <- function() mat_inv
    list(set = set_inv, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
    
}


## This function calculate the inverse of a matrix. If the matrix is cache return this

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat_inv <- x$get_inv()
    if(!is.null(mat_inv)) {
        message("getting cached data")
        return(mat_inv)
    }
    inv <- x$get()
    mat_inv <- solve(inv, ...)
    x$set_inv(mat_inv)
    mat_inv
}
