## This is the week 3 assignment
## Lexical Scoping

## The function will create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) minv <<- inverse
        getinv <- function() minv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function to compute the inverse of the matrix from cache if its already calculated

cacheSolve <- function(x, ...) {
        minv <- x$getinv()
        ## Check if the matrix is calculated then return cache matrix
        if(!is.null(minv)) {
                message("retrieving cached data")
                return(minv)
        }
        
        ## Return a matrix that is the inverse of 'x'
        mdata <- x$get()
        minv <- solve(mdata)
        x$setinv(minv)
        minv
}
