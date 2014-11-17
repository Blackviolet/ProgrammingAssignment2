## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function
#### x - special matrix, which caches its inverse
#### inv - cached inverse
## function involves get & set functions: x accessors
## function involves getinv & setinv functions: inv accessors
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set 
    #### initialize x with initval
    set <- function(initval) {
        x <<- initval
        inv <<- NULL
    }
    ## get 
    #### return x
    get <- function() x
    ## setinv
    #### cache x inversem
    setinv <- function(solve) inv <<- solve
    ## getinv
    #### return cached x inverse
    getinv <- function() inv
    list(set=set, get=get,
         setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## cachSolve function
## calculates inverse matrix for "spesial" matrix,
## created with makeCacheMatrix
#### if inversion has been already calculated - return it
#### overwise calculate it
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inversion...")
        return(inv)
    }
    mtr <- x$get()
    inv <- solve(mtr,...)
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv        
}
