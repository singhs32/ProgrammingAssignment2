## makeCacheMatrix function caches inverse of matrix for later re-use
## cacheSolve function reads the cached inverted matrix, if not available, it creates inverse of matrix and stores it in cache using makeCacheMatrix

## This function returns a list of four elemens 1) "set" element caches the original matrix 
## 2) "get" element returns cached original matix 3) "setinversematrix" caches inverted matrix
## 4) "getinversematrix" returns cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(inversematrix) m <<- inversematrix
        getinversematrix <- function() m
        list(set = set, get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}


## cacheSolve function returns the cached inverted matrix (if existing), else reads original matrix, inverts it, caches it and returns it

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinversematrix(m)
        m
}
