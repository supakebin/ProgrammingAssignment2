## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(Mat = matrix()) {
        iMat <- NULL
        
        ##set the value of the function
        set <- function(y) {
                x <<- y
                iMat <<- NULL
        }
        
        ##get the value of matrix
        get <- function() Mat
        
        ##set the inverse value of the matrix
        setInverse <- function(Inverse) iMat <<- Inverse
        
        ##get the inverse value of the matrix
        getInverse <- function() iMat
        
        ##list out the functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Write a short comment describing this function

##cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(Mat, ...) {
        
        ##assigns inverse value of matrick with getInverse function
        iMat <- Mat$getInverse()
        
        ##checks for Is Not NULL values in iMat
        if(!is.null(iMat)) {
                ##if value Is Not NULL, message is printed and return cahced data for iMat
                message("getting cached data")
                return(iMat)
        }
        
        ##if iMat is NULL, then following is ran
        ##get the matrix
        data <- Mat$get()
        
        ##computes the inverse of the matrix
        iMat <- solve(data, ...)
        
        ##sets the inverse value to cache with setInverse function
        Mat$setInverse(iMat)
        iMat
}
