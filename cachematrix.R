## Initial examples provided by the professor to serve as a template for the solution

##this function creates a special vector which is really a list containing a function to 
##set the value of the vector, get the value, set the value of the mean, get the value of the mean

makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

##the following function calculates the mean of the special vector after checking whether
##it's already been calculated

cachemean <- function(x, ...) {
m <- x$getmean()
if(!is.null(m)) {
  message("getting cached data")
  return(m)
}
data <- x$get()
m <- mean(data, ...)
x$setmean(m)
m
}

## My solutions below
##Following the code template provided to cache the inverse of the matrix
##The function is supposed to do a few things - set & get the elements of the matrix
##set & get the elements of the matrix inverse

makecachematrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##The next function below is supposed to calculate the inverse of the matrix
##created through the previous function (it closely matches the initial template)
##The basic idea being if the inverse has already been calculated, skip it,
##otherwise, we'll need to calculate the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("fetching inverse from cache")
    return(inv)
  }
  inverted_matrix <- x$get()
  inv <- solve(inverted_matrix, ...)
  x$setinverse(inv)
  inv
}

#Test example
test_matrix <- makecachematrix(matrix(5:8,2,2))
test_matrix$get()
test_matrix$getinverse()
cacheSolve(test_matrix)
cacheSolve(test_matrix)