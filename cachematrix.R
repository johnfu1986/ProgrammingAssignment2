##create a special "matrix", which is a list containing a function to
##1. set the matrix
##2. get the data of matrix
##3. set the inverse of matrix
##4. get the inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
    
	#assign NULL to inverse, initialization.  
    inverse <- NULL
    
	#set the matrix
    set <- function(y) {
            x <<- y
            inverse <<- NULL
    }
	
	#get the stored matrix. 
    get <- function() x
	
	#set and store the inverse matrix. 
    setinverse <- function(solve) inverse <<- solve
	
	#get the stored inverse matrix, if the inverse matrix hasn't been stored, it will return NULL.
    getinverse <- function() inverse
    
	#function list
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}



## return a matrix that is the inverse of 'x'
## if the inverse matrix data has been cached before, it will directly return the cached inverse.
## if the inverse matrix data hasn't be cached, it will compute the inverse, then store the inverse to cache and return the inverse.
cacheSolve <- function(x, ...) {
        
		#get the stored inverse matrix, if no inverse matrix been stored, it will get NULL
		inverse <- x$getinverse()
        
		#if the inverse matrix has been cached, show the message and directly return the cached inverse.
		if(!is.null(inverse)) {
                message("getting cached inverse matrix data")
                return(inverse)
        }
		
		#if the inverse matrix hasn't be cached, get the original matrix.
        data <- x$get()
		
		#computing the inverse
        inverse <- solve(data, ...)
		
		#store the inverse matrix data to cache
        x$setinverse(inverse)
        
		#return the inverse data
		return(inverse)

}
