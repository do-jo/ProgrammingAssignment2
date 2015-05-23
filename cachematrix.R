###########################################################
##These functions solve for and cache the inverse of a matrix, allowing efficient access.
#########################################################


## The maKeCacheMatrix function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #reset the inverse to null
  inverse = NULL 
  
  #define variable x in new environment; make inverse NULL since a new matrix is defined
  set <- function(y) {
    x <<- y  
    inverse <<- NULL 
  }
  
  #retrieve the matrix
  get <- function() {
    x
  }
  
  #set the value of the inverse
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  #retrieve the value of the inverse
  getInverse <- function() {
    inverse
  }

  #return these variables
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## The cacheSolve function computes the inverse of the matrix returned by the makeCacheMatrix function, or retrieves it from the cache if it exists. ***The function assumes that the matrix is invertible

cacheSolve <- function(x, ...) {
  #retrieve cached matrix inverse if it exists
  inverse = x$getInverse()
  if(!is.null(inverse)) { 
    message('Retrieving cached data')
    return(inverse)
  } else {
  #calculate and set the cached inverse it if does NOT yet exist
    data = x$get()
    inverse = solve(data)
    x$setInverse(inverse)
    inverse
  }
}


## Example Usage:

# testMatrix = matrix(c(3,4,2,3), nc = 2, byrow = TRUE) #define a matrix
# a = makeCacheMatrix(testMatrix) #cache the matrix
# a$get() #retrieve the matrix
# cacheSolve(a) #solve for the matrix inverse
# a$getInverse() #retrieve the matrix inverse


