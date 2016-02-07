## Courser DS Programming Assinment 2 (vmalino)
## Functions to calculate matrix inversion and to cache the result
## in order to improve perfomance

## Function creates a special "matrix" object that can cache its inverse
## The object is a list of 4 functions: getter and setter for the matrix,
## getter and setter for matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL ## Init matrix inverse with null
    set <- function(m) { ## Matrix setter
        x <<- m ## Stores the matrix
        minv <<- NULL ## Clear matrix inversion
    }
    get <- function() { ## Matrix getter
        x
    }
    setinv <- function(inv) { ## Matrix inverse setter
        minv <<- inv  
    } 
    getinv <- function() { ## Matrix inverse getter
        minv  
    }
    ## Return the list of 4 functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## It checks if the matrix inverse is already stored. If not - caculates and stores
## in provided matrix object created by means of makeCacheMatrix function
## Note: the function does not check if the cached object is matrix inverse indeed,

cacheSolve <- function(x, ...) {
    inv <- x$getinv() ## Get the value from matrix object (assumed to be matrix inverse)
    if(!is.null(inv)) { ## Check if stored object exists
        message("getting cached data") ## If yes, return the message
        return(inv) ## and the cached object
    }
    matr <- x$get() ## Otherwise retrieves initial matrix
    inv <- solve(matr, ...) ## and calculates its inverse
    x$setinv(inv) ## Stores the matrix invers into provided matrix object
    inv ## Return a matrix that is the inverse of 'x'
}
