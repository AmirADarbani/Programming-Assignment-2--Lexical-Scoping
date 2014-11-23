# Programming Assignment 2
# Caching the Inverse of a Matrix
#
makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  m <- NULL
  # If cacheSolve has not been used yet, value of m sets to Null.
  set <- function(y){
    # set the value of the matrix
    x <<- y
    # Cacheing the inputtes matrix
    m <<- NULL
  }
  get <- function()
    x
  setINVmtrx <- function(solve)
    m <<- solve
  # Return inverse of the same matrix that has already been calculated 
  getINVmtrx <- function() 
    m
  #
  list(set = set, get = get,setINVmtrx = setINVmtrx,
       getINVmtrx = getINVmtrx)
  # Create a list of four mentioned functions
}
cacheSolve <- function (x = matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getINVmtrx()
  #  this gets inverse of above matrix (calculated)
  if (!is.null(m)){
    # Check to see whether chachSolve has been run before
    message ("Getting Cached Data")
    return(m)
  }
  # In otherwise ... (if !is.null(m) ... = FALSE)
  data <-x$get()
  # Get the value of input function
  m <- solve(data, ...)
  # Calculate inverse of the input matrix
  x$setINVmtrx (m)
  # Run the setINVmtrx function
  m 
  # Return the inverse
}
