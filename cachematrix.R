## MakeCacheMatrix accepts a matrix argument and caches the input matrix into the parent environment


## Used the guidelines of the cachemean example and replaced mean with matrix and used function(solve) 
## rather than function(mean)

makeCacheMatrix <- function(x = matrix()) {
    m <-NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m<<-solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)    
}


## Function returns a matrix that is the inverse of the input matrix from cache if it exists

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
