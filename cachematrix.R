## makeCacheMatrix
## ---------------
## This function create a cache matrix, which is list containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	## Initiate the empty inverse matrix
	invMatrix <- NULL		## invMatrix is defined within the current function	
	## Set the matrix
	set <- function(y){
		x <<- y			## x is defined in outer function
		invMatrix <<- NULL	## invMatrix is defined in outer function
	}
	
	## Get the matrix
	get <- function() x		## is equivalent to "get <- function() { x }"
	
	## Set the inverse matrix
	setInvMatrix <- function(inverse) invMatrix <<- inverse
	
	## Get the inverse matrix
	getInvMatrix <- function() invMatrix
	
	list(set=set, get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
}

## cacheSolve
## ----------
## This function computes the inverse of the matrix.

cacheSolve <- function(x, ...) {
        invMatrix <- x$getInvMatrix()
	
	if (!is.null(invMatrix)){
		## If the inverse is already calculated before, 
		## it returns the cached inverse.
		message("getting cached data")
		return(invMatrix)
        }
	
	## Compute the inverse
        data <- x$get()
        invMatrix <- solve(data, ...)
	## Set and save the inverse 
        x$setInvMatrix(invMatrix)
        invMatrix
}

# Testing scenario and output for testing/validating the code
# -----------------------------------------------------------
# > x <-  matrix(c(4, 7, 2, 6), 2, 2)
# > x
#      [,1] [,2]
# [1,]    4    2
# [2,]    7    6
# > cx <- makeCacheMatrix(x)
# > cx$get()
#      [,1] [,2]
# [1,]    4    2
# [2,]    7    6
# > cacheSolve(cx)
#      [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > cacheSolve(cx)
# getting cached data
#      [,1] [,2]
# [1,]  0.6 -0.2
# [2,] -0.7  0.4
# > 
