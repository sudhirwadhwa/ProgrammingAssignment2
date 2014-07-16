## July 2014, USA , makeCacheMatrix and cacheSolve functions
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache ( message :getting cached data )
## Test run 1 and 2 at the bottom shows how to test. 

## Code Starts here, first I will create makeCacheMatrix
## Checking if matrix is input , else STOP 
## use get , set 
## use solve() to calculate inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
        
        if (!is.matrix(x)) {
                stop("expecting matrix, please try again...")
        }
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse= setinverse,
             getinverse= getinverse)
}


## Fucntion below computes the inverse of the cacheable matrix returned by makeCacheMatrix()
## If the inverse has already been calculated and there's no change in the matrix
## then the cacheSolve() returns the cached inverse with the message 


cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}




## Test Run 1 
## Testing the function, first create an instance of makeCacheMatrix , for example X
## > X <- makeCacheMatrix(matrix(1:4, 2))
## Then use cacheSolve to calculate the inverse of X
## > cacheSolve(X)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## Now without changing X , use cacheSolve(X) again, 
## result should come from cache ( message should say:getting cached data )
## > cacheSolve(X)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## Test Run 2, Cut / paste 5 commands below to test
## x = rbind(c(1, -1/4), c(-1/4, 1))
## m = makeCacheMatrix(x)
## m$get()
## cacheSolve(m)
## cacheSolve(m)
##
## commands with Output - Test Run 2
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > cacheSolve(m)
## getting cached data
## [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667



