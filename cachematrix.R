## Stores the inverse of a matrix in lexical scoping

## returns a special "vector", which contains 4 functions to and the calculated inverse
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	
	set <- function(y) {
		x <<- y
		m<<- NULL
	}
	get<- function() x
	setinv<- function(inv) i<<- inv
	getinv<- function() i
	
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i<- x$getinv()
	
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	
	data<- x$get()
	i<-solve(data,...)
	x$setinv(i)
	
	i
}
