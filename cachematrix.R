#Creating the special inverse matrix funtion
makeCacheMatrix <- function(x = matrix()) {
  
  #create a variable to store the inverse 
  m <- NULL 
  
  #Setting matrix equal to the input of makeCacheMatrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #Getting the value of the matrix
  get <- function() x
  
  #setting the inverse of matrix x to the variable m
  setinv <- function(solve) m <<- solve
  
  #Getting the value for the inverse
  getinv <- function() m
  
  #A list with functions set, get, setinv, getinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#Compute the inverse of a "special" inverse matrix returned by the above function, if m is NULL(the inverse has aready been calculated), return  matrix x.
cacheSolve <- function(x, ...) {
  
  #m would be the inverse of matrix x if the matrix was already computed
  # if we get x$getinv = NULL then the inverse has not been computed before
  m <- x$getinv()
  
  #return inverse of x (m) stored by makeCacheMatrix if m not NULL
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #if m is NULL, return value of the matrix x, using $get()
  data <- x$get()
  
  #...and create m
  m <- solve(data, ...)
  
  #use set inverse of x to the variable m
  x$setinv(m)
  
  #return inverse of x(m)
  m
}