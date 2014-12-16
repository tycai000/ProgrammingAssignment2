#Two functions used to solve and cache matrices in order to speed up processing of matrix data

#Creates a special matrix which produces a list to set and get the matrix, set and get the solution of the matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) { #sets the matrix
    x <<- y
    s <<- NULL
  }
  get <- function() x #returns the value of original matrix
  setinverse <- function(solve) s <<- solve # function used by cacheSolve in first instance to solve the original matrix
  getinverse <- function() s #function used by cacheSolve in order to retrieve a cached matrix solution
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#Input is a matrix created by makeCacheMatrix, output is its solution
cacheSolve <- function(x, ...) {
  s <- x$getinverse() #if matrix solution has been previous solved and cached, then use getinverse() function to retrieve result
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get() #if matrix solution has not been solved previously, then solve matrix,cache matrix solution, and then print
  s <- solve(data, ...)
  x$setinverse(s)
  s
}