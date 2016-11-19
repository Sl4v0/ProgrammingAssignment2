##The functions below are a solution to assignment from Sl4v0 on demonstrating
##the lexical scoping functionalities in R 

##makeCacheMatrix creates a special "matrix" object that can cache its inverse 
##and return a list of functions as a result that can set/get results of its 
##internal variables (matrix and its inverse)

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL               ## initializes the inverse matrix with NULL 
        set <- function(y) { 
                x <<- y                 ## sets the parent variable with value passed to set function
                invMatrix <<- NULL      ## sets the inverse to NULL as value of the matrix changed and inverse needs to be recalculated
                                        
        }
        get <- function() x             ## function to retreive value of a matrix
        setInv <- function(z) invMatrix <<- z ## function to set value of the inverse of the matrix
        getInv <- function() invMatrix  ## function getting retreiving value of the inverse
        list(set = set, get = get,      ## return a list with named functions as values (getter/setter for matrix and inverse)  
             setInv = setInv,
             getInv = getInv)
}


##cacheSolve: This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. If the inverse has 
##already been calculated (and the matrix has not changed), 
#then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {        
        
        invMatrix <- x$getInv()         ##gets the value of the Inverse matrix 
        if(!is.null(invMatrix)) {       ##checks the value of matrix inverse not zero (cached)  
                message("getting cached data") ## if cached - writing the info on screen
                return(invMatrix)       ## and returning a cached version
        }
        data <- x$get()                 ## getting value of matrix
        invMatrix <- solve(data, ...)   ## calculating the inverse of the matrix
        x$setInv(invMatrix)             ## setting the value of inverse and returning
        invMatrix
        
}

