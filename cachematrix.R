makeCacheMatrix <- function(x=matrix(nrow=0, ncol=0)){
#creating a 2 X 2 special matrix where each element is a function
#x[1,1] = set,  x[2,1] = get, x[1, 2] = setinverse,  x[2,2] = getinverse
        
        cachedinversematrix <- NULL # cached inverse matrix
        
        set <- function(y) {
                x <<- y
                cachedinversematrix <<- NULL
        } # set function
        
        get <- function() x
        
        setinverse <- function(a1) cachedinversematrix <<- a1
                
        getinverse <- function () cachedinversematrix
        
        tmp <-matrix( nrow=2, ncol=2)
        x1 <- c(set = set, get = get)
        y1 <- c(setinverse = setinverse, getinverse = getinverse)
        tmp <- cbind(x1, y1)

        return(tmp)
} # end of function makeCacheMatrix

cacheSolve <- function(x, ...) {
#this function takes the special matrix. if already cached return cached value with a message
#else computes the inverse, stores it in special matrix and returns the inverse
        
        m <- x[[2, 2]]() # call "getinverse" function
        
        if(!is.null(m)) { # found cached inverse matrix
                message("getting cached data")
                return(m)
        } # end if
        
        data <- x[[2,1]]() #call "get" function

        mrow <- nrow(data)
        mcol <- ncol(data)
        
        if ( ( is.null(mrow) ) && ( is.null(mcol) ) ) {
           # default matrix or empty matrix passed by user
                m <- NULL
                return(m)    
        } # end if 
        
        m <- solve(data)
        x[[1,2]](m) # call "setinverse" function
        return(m)

} # end of function cachesolve

