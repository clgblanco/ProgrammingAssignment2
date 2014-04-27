## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function takes an argument "x", and returns a list with 4 functions,
##as described below:

makeCacheMatrix <- function(x = numeric()) { ##this function creates the "matrix object"
                m <- NULL
                set <- function(y) { ##this function sets the value of the matrix.
                x <<- y
                m <<- NULL
                }
                get <- function() x  ##this function returns the value set for the matrix 
                setinv <- function(s) { ##this function inverts the matrix previously set
                s <- solve(x)
                m <<- s
                }
                getinv <- function() m ##this function returns the value of the inverted matrix
                list(set = set, get = get,
                    setinv = setinv,
                    getinv = getinv)
}

## Write a short comment describing this function
##This is a function based on "makeCacheMatrix", in which it searches for values of
##inverted matrices. If none is found, then it creates a new inverted matrix.

cacheSolve <- function(x, ...) {
            m <- x$getinv()           ##query the X vector's cache         
            if(!is.null(m)) {           ##if there is a cache...
            message("getting cached data!! :)") 
            return(m)                ##...return the cache, do not do anything more
            }
            data <- x$get()             ##if there's no cache...
            m <- solve(data, ...)        ##...compute them here
            x$setinv(m)                ##save the result back to the cache
            m                           ## Return a matrix that is the inverse of 'x'

}
