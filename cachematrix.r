makeCacheMatrix <- function(iMatrix = numeric()) {
  m <- NULL
  #creates an object of type list, set to NULL
  set <- function(y) {
    iMatrix <<- y
    m <<- NULL
  }
  #This function sets the entries of our matrix
  get <- function() iMatrix
  #This function prints the contents of our matrix
  setinverse <- function(inverse) m <<- inverse
  #This function sets the content of our inverse matrix
  getinverse <- function() m
  ##This function prints the contents of our inverse matrix
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  #Values of our list are the four functions we have just defined
}
cacheSolve <- function(iMatrix, ...) {
  m <- iMatrix$getinverse()
  #iMatrix$getinverse() will be NULL unless we have already computed the inverse of this matrix and stored it
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Tests if it is NULL - if not, we already have all of our information
  data <- iMatrix$get()
  m <- solve(data, ...)
  iMatrix$setinverse(m)
  m
  #Here we actually compute the inverse matrix, if necessary
}