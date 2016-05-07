##Function that creates a "cacheable" matrix.
makeCacheMatrix <- function(m = matrix()){
        
        inv <- NULL
        
        set <- function(m_input = matrix()){
                m <<- m_input
                inv <<- NULL
        }
        
        get <- function() m
        
        setinverse <- function(inv_input = NULL) inv <<- inv_input
        
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##Function that calculates the inverse of a "cacheable" matrix.
cacheSolve <- function(M, ...){
        
        inv <- M$getinverse()
        
        if(!is.null(inv)){
                message("getting cached matrix's inverse")
                return(inv)
        }
        
        inv <- solve(M$get(), ...)
        M$setinverse(inv)
        inv
}
