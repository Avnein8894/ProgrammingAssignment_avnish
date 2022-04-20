
##This code is written by Avnish

makeCacheMatrix <- function(var_x = matrix(sample(1:100,9),3,3)) {
## we are taking value of var_s as null
  var_s <- NULL
  func_set <- function(y) {
    var_x <<- var_y
    var_s <<- NULL
  }
  var_get <- function() var_x
  func_setsolve <- function(solve) var_s <<- solve
  func_getsolve <- function() var_s
  list(func_set = func_set, func_get = func_get,
       func_setsolve = func_setsolve,
       func_getsolve = func_getsolve)
}

cacheSolve <- function(x, ...) {
  var_s <- var_x$func_getsolve()
  if(!is.null(var_s)) {
    message("getting inversed matrix")
    return(var_s)
  }
  var_data <- var_x$var_get()
  var_s <- solve(var_data, ...)
  var_x$func_setsolve(var_s)
  var_s
} 
