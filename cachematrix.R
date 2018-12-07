##Waqas Mansoor: 2nd attempt to solve this assignment...R Prog..Date: 06th Dec 2018 
##First step: This function creates a special "matrix" object that can cache its inverse.
## Example of operation:
##    m <- matrix(c(1,2,3,4),2,2)
##    cachematrix1 <- makeCacheMatrix(m)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(z) {
        x <<- z
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Now this function will compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## Example:
##    m <- matrix(c(1,2,3,4),2,2)
##    cachematrix1 <- makeCacheMatrix(m)
##    cacheSolve(cachematrix1)

cacheSolve <- function(x, ...) {
    inv <- x$getinverse() #inv is inverse 
    ##if condition to check whether the inverse has been already calculated or not     
    if (!is.null(inv)){ 
        message("getting cached data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data) #%*% data
    x$setinverse(i)
    inv
}
