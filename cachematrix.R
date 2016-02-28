## The inverse of a square matrix A, sometimes called a reciprocal matrix, is a matrix A^(-1) such that AA^(-1)=I (Identity Matrix)
## Caching the Inverse of a Matrix: Inversion of a matrix is a time consuming computation
## Caching of the inverse of a Matrix, may prove benefetial instead of computing it.
## I have two functions, 
## a- The function that creates special object to store a Matrix .
## b- The function which Caches the stored matrix inverse.

## a- This below function creates a matrix object and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
           setinverse = setinverse,
             getinverse = getinverse)
}


## b-The function which Caches inverse of the Matrix created by function "makeCacheMatrix". In case the inverse is has been caluclated
## this function has to retrive the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("Processing Cached Data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinverse(inv)
        inv
}

## My test Results:
## > test<-makeCacheMatrix(matrix(1:4, 2, 2))
## > test$get()
  ##   [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > test$getinverse()
## NULL
## > cacheSolve(test)
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(test)
## Processing Cached Data
   ##  [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > test$getinverse()
   ##  [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## This is to Test, if the Identity Matrix is arrived by A %*% A^(-1)=I
## > test$get()%*%test$getinverse()
     ## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## The above result is a Indentity Matrix, hence the test is PASS.
