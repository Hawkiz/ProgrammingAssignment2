## It calculates the inverse of a quadratic matrix, caching it.

## makeCacheMatrix is a function that contains set and get functions needed
## for caching the inverse of matrix x and it also creates an i that will cache
## the inverse of matrix x.

makeCacheMatrix <- function(x = matrix()) {
	i = NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) i <<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}

##cacheSolve uses the function solve with the variable data that contains the
##matrix x. Using functions get and set from makeCacheMatrix, it will returns
##the inverse of matrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}
