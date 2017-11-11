## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse = NULL
	set <- function(y){
		x <<- y
		inverse = NULL
		}
	get <- function() x
	setInv <- function(inv) inverse <- inv
	getInv <- function() inverse
	list(set =set, get=get, setInv = setInv, getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix function provided above. If the inverse has already been
## calculated (and the matrix has not changed), then the cacheSolve will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	XM <- makeCacheMatrix(x)
	inverse <- XM$getInv()
	if(!is.null(inverse)){
		message(" Getting the matrix inverse from the Cache")
		return(inverse)
		}
	mat <- XM$get()
	inverse <- solve(mat)
	XM$setInv(inverse)
	inverse
}
