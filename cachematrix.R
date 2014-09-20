#The code calculates the inverse matrix of an input matrix. The result is retrieved from cached value if has already been calculated. Otherwise is calculated by use of function solve().


#makeCacheMatrix function creates a list of four functions:
#Function "set" sets the inverse matrix "m' to null and input matrix "x" equal to y
#Function "get" is the input matrix "x'
#Function setsolve sets the value of variable m to be equal to variable solve 
#Function getsolve is the variable m

makeCacheMatrix<-function(x=matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#cacheSolve function uses as input the makeMatrix function and calculates the inverse of the input matrix. If inverse matrix calculation has been done it is invoked from the cached m variable. Otherwise it is calculated by use of solve() function and sets it in the cache by use of setsolve function. 
#cacheSolve calls or may call get(), setsolve() and getsolve() functions from the makeMatrix function list

cacheSolve <- function(x,...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}