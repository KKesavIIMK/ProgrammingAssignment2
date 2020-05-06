## Creating a pair of functions that will cache the inverse of a matrix


## makeCacheMatrix function is designed to create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                            inv <- NULL
                            set <- function(y){
                                  x <<- y
                                  inv <<- NULL
                                }
                         get <- function() x
                         setInverse <- function(solveMatrix) inv <<- solveMatrix
                         getInverse <- function() inv
                         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function above inverse of the special "matrix" and cacheSolve will use this value when computing inverse whenever cache is available

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}
