## After having read several times through the assignment and the examples, which by the way seem very heavy and complicated to me, 
##   I decided to simply adapt the presented functions to a matrix. In reality, I am missing lots of contextual information to 
##   really understand the task.
##
## 1) makeCacheMatrix outputs a list with 4 sub-vectors, well actually 2 sub-vector-pairs. It serves a sstorage/retrieval facility and it
##    defines the cache values.
##
## 2) cacheSolve retrieves the inverse vector, - provided there is one and the original vector did not change. If not this function loads
##    inverse vector into makeCacheMatrix and outputs it too.

makeCacheMatrix <- function(x = matrix()) {
       
        m <- matrix()                           ## sets the inverse matrix to empty
        
        set <- function(y) {                    ## defines the cache matrix in the global environment 
                x <<- y
                m <<- matrix()
        }
        
        get <- function() x                     ## outputs the cache vector, when requested
        
        setinv <- function(inv) m <<- inv       ## puts the inverse matrix into the global environment
        
        getinv <- function() m                  ## retrieves the inverse matrix 
        
        list(set=set, get=get, setinv=setinv, getinv=getinv)    ## outputs the list to makeCacheMatrix

}

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()                         ## retrieves inverse and cached matrix from makeCacheMatrix
        data <- x$get()
        
        if(!identical(inv, matrix()) & identical(x, data)) {      ## checks against 1) inverse matrix is not empty ... and
                message("getting cached data")                    ##   2) vector didn't change in the meantime
                return(inv)                                       ##   to use the cache matrix when true
        }
        inv <- solve(data, ...)                                   ## when false it calculates the inverse value, puts it into
        x$setinv(inv)                                             ##    makeCacheMatrix
        inv                                                       ##    and outputs the newly calculated inverse matrix.

}