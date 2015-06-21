## The makeCacheMatrix function contains 4 sub-functions which are set, get, setinv, getinv
## The function takes matrix and caches it in the list of the 4 sub-functions
## The <<- operator which can be used to assign a value to an object in an environment that 
## is different from the current environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## inverse values will be stored here, initialized to null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x ## return input matrix
        setinv <- function(inverse) m <<- inverse ## set the inverse matrix
        getinv <- function() m ## get the inverse matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function "cacheSolve" calculates the inverse of matrix keyed in. If the result already exists 
## (stored), the message "getting cached data" will be printed. If the result is new, the result will
## be generated and will be stored

cacheSolve <- function(x, ...) {
        m <- x$getinv() ## get the inverse matrix from x
        if(!is.null(m)) { ## if the results stored previously, get this message
                message("getting cached data")
                return(m)
        }
        data <- x$get() ## if the results not stored previously, get the result and store
        m <- solve(data, ...)
        x$setinv(m)
        m
}

## testing with a squared matrix
x<-matrix(runif(9,1,50),3,3)
y<-makeCacheMatrix(x)
z<-cacheSolve(y) ## get the result of inverse and store
z<-cacheSolve(y) ## already stored previously
