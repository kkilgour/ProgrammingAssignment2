## Code for creating a special matrix and retriving/caching its inverse

## Make a special matrix that allows for caching certain function results 
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL ## Clear previous cached inverse
	}
	get <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the inverse of a matrix x; use cached result if available
cacheSolve <- function(x, ...) {
	i <- x$getinv()
        ## Is the inverse already cached?
	if(!is.null(i)) {
		message("Getting cached data")
		return(i)
	}
	## Inverse was not cached; calculate it and store the result
	data <- x$get()
	i <- solve(data)
	x$setinv(i)
	i
}

