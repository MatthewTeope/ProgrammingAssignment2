#To store and cache the matrix, the functions below will be created.
#Utilizing makeCacheMatrix will be a function for the first matrix creation

makeCacheMatrix <- function(functionA = matrix()) 
{
nullReceiver <- NULL
  	set <- function(functionB) 
  	{
            	functionA <<- functionB
            	nullReceiver <<- NULL
   	}
  	get <- function() functionA
  	setInverse <- function(inverse) nullReceiver <<- inverse
 	getInverse <- function() nullReceiver
 	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#Use this function to compute the inverse specific matrix from the original makeCacheMatrix command.
#After finding the inverse, utilize the CacheSolve command to retrieve the cache

cacheSolve <- function(functionA, ...) 
{
nullReceiver <- functionA$getInverse()
if (!is.null(nullReceiver)) 
{
message("getting cached data")
 		return(nullReceiver)
}
  	data <- functionA$get()
 	nullReceiver <- solve(data, ...)
  	functionA$setInverse(nullReceiver)
  	nullReceiver
}

