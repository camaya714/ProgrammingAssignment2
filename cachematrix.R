## set the value for matrix 
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

## following function  assigns value to matrix 'x' and applies the 'solve' function to it to get inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
           x <<- y
           m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## function calculates the inverse of the above matrix 'x'
## it first checks to see if the inverse has already been calculated.
## if so, it gets the mean from the cache by returning 'm' from above.
## if not, it calculates the inverse of the matrix and sets the inverse of the matrix usin 'setinv'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
            message("Getting chached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
