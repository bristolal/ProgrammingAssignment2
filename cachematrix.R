# this code contains two main functions: makeCacheMatrix and cacheSolve.  Between them they calculate 
# the inverse of a matrix and cache the value so that it can be recalled at any time without recalculating.


# makeCacheMatrix: creates an empty matrix (x) and then creates several sub-functions which variously;
# - set the value of the matrix (set)
# - retrieve the value of the matrix (get)
# - set the value of the inverse of the matrix (setinverse)
# - retrieve the value of the inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## set: caches the value of y to empty matrix x; i variable continues to be NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get: retrieves the value of matrix x
  get <- function() x
  
  ## setinverse: takes the inverse of matrix x and caches it to variable i
  setinverse <- function(inverse) i <<- inverse
  
  ## getinverse: retrieves the value of variable i
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}



# cacheSolve: checks to see if the inverse of x has already been calculated.  If it has it prints the 
# value; if it hasn't it calculates it, caches the value and prints it.

cacheSolve <- function(x) {
  
  ## if the inverse has already been calculated then it will be returned and stored in variable i; otherwise
  ## i will remain NULL
  i <- x$getinverse()

  
  ## if the value of i is not NULL (i.e. the inverse of x has already been calculated), the message "getting
  ## cached data" will be printed, followed by the value for i (which is the inverse).  The function is then
  ## exited so the code below does not get run.
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ## if the value of i is NULL (the inverse of x has NOT yet been calculated), then the matrix x is
  ## stored in the variable data, the inverse is found and stored as i, i is then cached and the value printed
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
