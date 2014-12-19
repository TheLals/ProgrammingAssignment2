## This function creates a special "matrix" object, creates and caches its inverse.
## It also returns the 'matrix' and the inverse of the 'matrix'
## 

## ****************************************************************************************************
## Function:  makeCacheMarix()
## Author:  Binu Lal
## Input: a matrix
## Outpu: a list containing 4 functions - setmatrix(), getmatrix(), setrinverse(), & getinverse()
##
## This function create and returs a 'matrix'.
## It also creates and returns the inveser of the 'matrix'
##
##

makeCacheMatrix <- function(mat1 = matrix()) {

	inversemat <- NULL  # set null value to the variable 'inversemat'

	set <- function (mat2) {  #  function to set the matrix

		mat1 <<- mat2

		inversemat <<- NULL
	}

	get <- function() mat1  # function to get the matrix

	setinverse <- function(solve) inversemat <<- solve # function to create inverse of the matrix

	getinverse <- function() inversemat  # function to get the inverse

	list(setmatrix = set, getmatrix = get, setinverse = setinverse, getinverse = getinverse)

}

## ****************************************************************************************************
## Function:  makeCacheMarix()
## Author:  Binu Lal
## Input: a matrix
## Outpu: a list containing 4 functions - setmatrix(), getmatrix(), setrinverse(), & getinverse()
##

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache

cacheinversemat <- function(mat1, ...) {

	inversemat <- mat1$getinverse()  # get the inverse

	if(!is.null(inversemat)) {   # get the cached inverse

		message("Getting cached inverse")

		return(inversemat)  # returnes the cached value
	}

	mat <- mat1$getmatrix()  # get the matrix
	
	inversemat <- solve(mat, ...)  # calculate the matrix, if it is not in the cache

	mat1$setinverse(inversemat)  # set the inverse
 	
	message ("No cached value.  Getting the calculated inverse")

	inversemat   # returns the calculated value

}

## ****************************************************************************************************
##
## Sample Output
##

##  > source("cachematrix.R")

##  > amat = makeCacheMatrix(matrix(c(5,6,7,8), 2,2))

##  > amat$getmatrix()

##       [,1] [,2]
##  [1,]    5    7
##  [2,]    6    8

##   > cacheinversemat(amat)

##   No cached value.  Getting the calculated inverse
##        [,1] [,2]
##   [1,]   -4  3.5
##   [2,]    3 -2.5

##   > cacheinversemat(amat)

##   Getting cached inverse
##        [,1] [,2]
##   [1,]   -4  3.5
##   [2,]    3 -2.5
