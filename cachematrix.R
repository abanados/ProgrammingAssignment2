## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCasheMatrix function will first initial the x as a matrix type and 
## set m to NULL (m is the inverse of matrix)
## 4 basic sub-functions has been defined as set, get, setmean, 
## and getmean.  makeCasheMatrix will return a list.

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function
## cacheSolve function will first check if inverse of matrix is Null
## or not.  Since makeCacheMatrix() sets the cached inverse to NULL 
## whenever a new vector is set into the object, if the value here is 
## not equal to NULL, we have a valid, cached inverse and can return it 
## to the parent environment. After validation, the function will then 
## inverse the matrix and return the data.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
