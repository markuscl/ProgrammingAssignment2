## FUN makeCacheMatrix to create out of a matrix a matrix object with several functions
## to get, set values and getinverse setinverse
## FUN cacheSolve modified solve function to use cached inverse value of created cached matrix object

makeCacheMatrix <- function(x = matrix()) {
        #inverse is initialized, all existing values are assigned NULL
        inv  <- NULL
        #setting function for matrix object is defined
        set <- function (y) {
                #existing values x are replaced by new values y
                x <<-y
                #if matrix object had inverse it is now set to NULL due to new set values
                inv <<- NULL
        }
        #get function for matrix object is defined
        get <- function() x
        #setinverse function for matrix object is defined inverse can be assigned to object
        setinverse <- function(inverse) inv <<- inverse
        #getinverse function for matrix object just giving value of inverse
        getinverse <- function() inv
        #list containing all functions
        list(get=get, set=set, getinverse=getinverse, setinverse = setinverse)

}

##cacheSolve function taking a list object that can be seen as a matrix object with
cacheSolve <- function(x, ...) {
        ## checking if object as inverse by reading out
        inv <- x$getinverse()
        #if value already exist then this cached value is taken in other case the inverse is calculated on the actual data
        if (!is.null(inv)) {
                message("get cached inverse")
                return(inv)
        } 
        #if no cached inverse then actual data is read in 
        data <- x$get()
        #then the inverse is calculated
        inv  <- solve(data, ...)
        #and set as new cached inverse
        x$setinverse(inv)
        inv       
}
