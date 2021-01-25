#this function creates a CacheMatrix object that can be instantiated to allow for the getting/setting of a matrix object within the cache as well as potentially the getting/setting of its inverse
makeCacheMatrix <- function(x = matrix()) {
  #empty inv variable setup to cache inverse if calculated
  inv <- NULL
  #function that allows the creation of a matrix object and empty variable for inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #a get function to return the matrix object
  get <- function() x
  #function to assign the inverse of x as the inv variable
  setinv <- function() inv <<- solve(x)
  #function to return the inverse of the matrix
  getinv <- function() inv
  #returns a list of the larger CacheMatrix object's attributes
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



#this function checks if the CacheMatrix object has already calculated the inverse of its matrix and retrieves it if so and sets and gets the inverse if not
cacheSolve <- function(x) {
  # assigns the inverse of the matrix to the inv variable
  inv <- x$getinv()
  #if the variable is not empty, then it was previously calculated and cached. This loop prints a message saying so and returns the variable
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #this is if the inverse was not cached. The inverse matrix is calculated and set and then retrieved and returned.
  x$setinv()
  x$getinv()
}
