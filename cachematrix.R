## Source contains two functions
## makeCacheMatrix creates a matrix that can cache the inverse matrix
## cachSolve creates an inverse of an input matrix for makeCacheMatrix to cache

## Create a matrix to cache the result of inverse
## Input should be a square matrix that can be inverted
## Format of input x <- makeCacheMatrix (matrix(c(1,2,3,4), nrow=2, ncol=2))

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL #set m to zero
        set <- function(y) {
                x <<- y #sets value of x outside the function to input
                m <<- NULL #sets value of m outside the function to null
        }
        get <- function() x #gets the value of x from within the function
        setinverse <- function(inverse) m <<- inverse # 
        getinverse <- function() m #puts the cached value to the cache matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) #lists the internal functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse() #gets the input
        if(!is.null(m)) {
                message("getting cached data") 
                return(m) #checks the cache matrix to check it is empty 
        }
        data <- x$get() #gets the input data
        m <- solve(data, ...) #calculates the inverse
        x$setinverse(m) #sets the inverse as the input to cache matrix
        m

}