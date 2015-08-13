## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function transform an R matrix into an augmented object
## capable of holding private properties. In also
## provides object mutators and getters.

## This matrix store the inverse within its private attributes inv_matrix
## whose access is regulamentead by setInverse() and getInverse() 
## methods.
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix<-NULL
    
    set<-function(y)
    {
        x<<-y
        inv_matrix<<-NULL
    }
    
    get<-function() x
    
    setInverse <- function(inv) inv_matrix <<- inv
    
    getInverse <- function() inv_matrix
    
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function computes the inverse of the augumented "matrix"
## objects type, produced by the makeCacheMatrix function.
## The inverse matrix is store within a private object property
## so to speed up the inverse matrix computation.

## The function accesses the private property of the matrix using
## the provided getters and setter methods.

## If you call twice the cacheSolve() function, you will see a
## "getting cached data" message printed to the console.

cacheSolve <- function(x=matrix(), ...) {
    
    inv <- x$getInverse()
    
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    matrix<-x$get()
    inv<-solve(matrix, ...)
    x$setInverse(inv)
    inv
}