## Overall, the two functions are trying to cache the inverse of a matrix so that
## if the inverse is alreaddy calculated, they will get from the cache and skip
## the computation. 

## The first function creates a special "matix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		m_i <- NULL
		set <- function(y){
			x <<- y
			m_i <<- NULL
		}
		get <- function() x
		setInverse <- function(solve) m_i <<- solve
		getInverse <- function() m_i
		list(set=set, get=get, 
                 setInverse=setInverse, 
                 getInverse=getInverse)

}


## The second function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m_i <- x$getInverse()
	  if (!is.null(m_i)){
		message("getting cached data")
		return(m_i)
	  }
	  data <- x$get()
	  m_i <- solve(data, ...)
	  x$setInverse(m_i)
	  m_i 
}