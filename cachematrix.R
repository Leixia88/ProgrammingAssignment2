## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	x_inverse <- NULL
	set <- function( y ){
		x <<- y
		x_inverse <<- NULL
	}
	get <- function() x
	setinverse <- function( inverse ) x_inverse <<- inverse
	getinverse <- function() x_inverse
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		x_inverse <- x$getinverse()
		if( !is.null( x_inverse ) ){
			message( "getting the cached inverse" )
			return( x_inverse )
		}
		xmatrix <- x$get()
		x_inverse <- solve( xmatrix, ... )
		x$setinverse( x_inverse )
		x_inverse
}
# uncomment below lines, and run this file with "Rscript cachematrix.R" in your linux shell, gives a quick test of this script.
#x <- makeCacheMatrix( matrix( 1:4, nrow = 2, ncol = 2 ) )
#x1 <- cacheSolve(x)
#y <- matrix( 1:4, nrow = 2, ncol = 2 )
#y1 <- solve(y)
#if( identical(x1,y1) ){
#	print( "x1 == y1")
#}else{
#	print( "x1 != y1" )
#}
