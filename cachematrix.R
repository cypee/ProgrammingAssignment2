## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(invmatrix) invmat <<- invmatrix
    getInvMatrix <- function() invmat
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    invm <- x$getInvMatrix()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    rc = dim(data)
    if(rc[1] != rc[2]) {
        message("Not an nxn matrix. The matrix is not invertible")
        return(NULL)
    }
    invm <- solve(data)
    x$setInvMatrix(invm)
    invm
}