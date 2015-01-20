## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                first <<- y
                inv <<- NULL
        }
        get <-function() first
        setinv <- function(x) inv<<-(solve(x))
        getinv<- function() inv
        list(set = set, get = get,
                setinv = setinv,  getinv = getinv)

}


## Write a short comment describing this function

cacheSolve<- function(x, ...){
        inv <-x$getinv()
        if (!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
        
}
## Examples
m1 <- makeCacheMatrix() # generates functions
a <- matrix(c(7,5,0,4),2,2)
m1$set(a) # load matrix
b <- cacheSolve(m1) # Read matrix inverse a
a %*% b # test
