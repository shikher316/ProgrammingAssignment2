## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to create a special vector which consist of list of sub functions for the purpose of calculating the inverse of the matrix 
# The function consists of the following inline functions - 
# Get the value of the special vector 
# Set the value of the Vector 
# Get the value of the inverse of matrix
# Set the value inverse of matrix 

makeCacheMatrix <- function(x = matrix()) {
  inverse_temp <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # defining function to set the value of inverse of matrix         
  setinverse <- function(solve) inverse_temp <<- solve
  # defining function to get the value of inverse of matrix         
  getinverse <- function() inv
  # defining the vector list 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function
# Function to call the inverse function of matric. In case the matrix is already present in the cache, the pre-calculated value for the inverse of the matrix will be printed
# In case there is no inverse present for the matrix, the inverse will be calculated and stored for further computation 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #calling the function to get the cached data set 
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #In case the there is no cached matrix inverse, calculate the dataset and save it for future reference
  data <- x$get()
  # calling solve function to calculate the inverse 
  m <- solve(data, ...)
  # storing the matrix calculation in case cache was not available
  x$setmean(m)
  m
}
