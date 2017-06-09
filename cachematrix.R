## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix takes a matrix and returns 
## a special matrix and also it stores the inverse of the matrix 
## in cache 

makeCacheMatrix <- function(x = matrix()) {
        mat_in <- NULL
        set <- function(y){
                x <<- y
                mat_in <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) mat_in <<- solve
        getinverse <- function() mat_in
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## caheSolve takes the output matrix of the makeCacheMatrix
## and returns the inverse matrix. However, first it checks
## to see if the inversed matrix is already saved in cache and 
## if so it returns the saved one instead of computing
## it again which saves computing resourses and time 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_in <- x$getinverse()
        if(!is.null(mat_in)){
                message("getting cached data")
                return(mat_in)
        }
        data <- x$get()
        mat_in <- solve(data,...)
        x$setinverse(mat_in)
        mat_in
}
