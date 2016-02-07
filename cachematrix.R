## Courser DS Programming Assinment 2 (vmalino)
## Functions to calculate matrix inversion and to cache the result

## Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(m) {
        x <<- m
        minv <<- NULL ## Clear matrix inversion
    }
    get <- function() {
        x
    }
    setinv <- function(inv) {
        minv <<- inv  
    } 
    getinv <- function() {
        minv  
    }
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matr <- x$get()
    inv <- solve(matr, ...)
    x$setinv(inv)
    inv ## Return a matrix that is the inverse of 'x'
}
