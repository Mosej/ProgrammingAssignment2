## This program gets an invertible matrix and caches it 
## it can then claculate the inverse of this matrix, if its not yet been calculated  
##or it retrieves the cached value and stores it

## this function claculates the inverse of a matrix it creates, and caches it

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        
        setMat<- function(y=matrix( z, nrow, ncol )){ 
                x<<-y        }
        getMat<- function() x
        setInv<- function(solve) inv<<- solve(x)
        getInv<- function() inv
        
        list(setMat = setMat, getMat = getMat,setInv = setInv,
             getInv = getInv)
        
}

## this function calculates the inverse of the matrix in makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        inv<- x$getInv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$getMat()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}