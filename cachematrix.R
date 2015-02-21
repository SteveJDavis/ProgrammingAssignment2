## These two functions calculate & store the inverse of a matrix.  When next run
## the stored inverse is recalled from memory rather than recalculated. If the 
## matrix is changed the inverse is recalculated.
## SJD Feb 2015

## Creates four functions (set(), get(), setinv(), getinv())

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y           ## pass matrix to x from global environment  
        inv <<- NULL      ## sets inv to NULL as the matrix is changed
    }
    get <- function() x  
    setinv <- function(inverse) inv<<-inverse  ## assigns the global variable
                                               ## inverse to local inv
    getinv <- function() inv        ## retrieves the value of inv      
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve checks to see if there is a stored value (inv) for the inverse 
## matrix.  If there is no stored value the inverse matrix is calculated & the 
## result stored in inv.

CacheSolve <- function(x, ...) {
    ## retrieves the value of the stored inverse. If not NULL returns this value
    inv<-x$getinv()                         
    if(!is.null(inv)){
        message("Getting cached inverse")
        return(inv)
    }
    ## If no stored inverse matrix, inverse is calculated (solve()) & the result
    ## stored.
    localmatrix<-x$get()
    inv<-solve(localmatrix)
    x$setinv(inv)
    inv
}
