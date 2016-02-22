## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. It serves the following purposes: 
## (1)set the value of the matrix
##(2) get the value of the matrix
##(3) set the value of the inverse
##(4) get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <-NULL
    set <- function(y) {                            ## (1)set the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x                             ##(2) get the value of the matrix      
    setinverse <- function(mean) m <<- solve(x)     ##(3) set the value of the inverse
    getinverse <- function() m                      ##(4) get the value of the inverse
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)     ##(5) return a list
                                                                             ## containing above functions
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)) {
           message("getting cathced data")
           return(m)
       }
       data <-x$get()
       m <- solve(data, ...)
       x$setinverse(m)
       m
     ## Return a matrix that is the inverse of 'x'
}
