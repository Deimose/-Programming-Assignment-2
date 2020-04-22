##These functions that cache the inverse of a matrix.


## The function, makeCacheMatrix creates a special matrix objet , which is really a list containing a function to cache its  inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL   ## Initialize the inverse property
  
  
  
  ##Set the matrix
  
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        
   ##Get the matrix
        
        get <- function() x
        
        
   ##Define the funtion setinverse 
     
        setinverse <- function(inverse) m <<- inverse
  
   ##Define the funtion getinverse
         
        getinverse <- function() m
        
  ##Return the list of defined function
  
  
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)




}


## The function cacheSolve calculates the inverse od the special matrix return in makeCacheMstrix

cacheSolve <- function(x, ...) {
	
## Cached calue fot the inverse
	
 m <- x$getinverse()

## Conditional for not empty than return it

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
      ##Get matrix value
        
        data <- x$get()
        
  ##Calculate inverse
  
        m <- solve(data, ...)
        
        ##Cache the result
        
        x$setinverse(m)
        
     ## Return de inverse matrix
        m
}
