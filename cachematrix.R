## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and returns a list inlcuding accessors such as setter and getter

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                ## the setter functions assings variables in a different environment by ussing << operator
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        
        ## list to be returned by the function, which includes accessors for the matrix
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}


## Returns the inverse of the matrix X, while previously checking is not present on the cache already

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        
        ## Check whether inverse is already presenrt in the cache, if so returns cache version
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## If not present in the cache, then calculates the inverse using solve
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}


makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}