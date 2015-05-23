#the code to compute the inverse is written by using the sample code provided as the template.  
#The makeCacheMatrix stores 4 functions as a list namely setmatrix,getmatrix,setinverse and getinverse
#getmatrix gets the matrix stored in makeCacheMatrix and setmatrix is the function which will store the matrix in cache.

#makeCacheMatrix is only function definitions
#when we give a matrix as input, and a simple call for makeCacheMatrix$getmatrix will simply return the input matrix 
#setmatrix will get the input y and change the original matrix x and m will be set to NULL
#getinverse will get the new m if m was changed or will return null
#setinverse will assign the inverse to m and m will not be nUll and will be the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  browser()
  m <- NULL
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve is the function call to trigger makeCacheMatrix()
#cacheSolve will check for matrix in cache if assigned using setmatrix in makeCacheMatrix().
#If not, it will calculate the inverse of the input matrix if there is no matrix in cache. 
#the getinverse will be similar to getmatrix
#if no matrix is stored in cache, that is, m is NULL, then the function proceeds to calculate the inverse by getting the matrix using
#getmatrix() and calculating the inverse using solve() and store it in m
#this inverse will then be set to cache using setinverse()
#if there is already a matrix stored in the main function as cache then getmatrix will return it as the inversematrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getmatrix()
  m <- solve(data)            
  x$setinverse(m)
  m
}
