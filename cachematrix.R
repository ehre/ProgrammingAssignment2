## Coursera R-programming course assignment: Caching the Inverse of a Matrix
## Forked from https://github.com/rdpeng/ProgrammingAssignment2
## Assumption: Matrix is square and invertible (no error checking).
## Example code:
# test <- makeCacheMatrix(matrix(1:4,2,2))
# cacheSolve(test)

## Creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                               ## Initially set inverse(i) to null
        
        set <- function(y) {                    ## Set matrix value from user input
                x <<- y                         # Value assigned to enclosing environment
                i <<- NULL                      # Inverse set to null in enclosing environment
        }
        get <- function() {                     ## Get matrix value - used in cacheSolve()
                x
        }
        setinverse <- function(inverse) {       ## Set value of inverse - returned from cacheSolve()
                i <<- inverse                   # Inverse set in enclosing environment
        }
        getinverse <- function() {              ## Get value of inverse - used in cacheSolve()
                i
        }
        
             list(set = set, get = get,         ## Return function list
             setinverse = setinverse,
             getinverse = getinverse)
}     

## Retrieves inverse from cache if it exists, otherwise computes inverse and caches it.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {                               ## If cached inverse exists...
                message("getting cached matrix")        # a. Write message
                return(i)                               # b. Return cached inverse
        }
        
        data <- x$get()                                 ## Else if no cached inverse...
        i <- solve(data, ...)                           # a. Compute inverse
        x$setinverse(i)                                 # b. Cache inverse
        i                                               # c. Return the result
}
