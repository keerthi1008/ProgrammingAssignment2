
## This function creates a special "matrix" object that can cache its inverse.

#  gets the value of the matrix, sets the inverse Matrix and gets the inverse Matrix. The matrix object can cache its own object. 

#  <<- operator is used to assign a value to an object in an environment that is different from the current environment 


#  makeCacheMatrix function definition starts

#  takes the matrix as an input

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL

#  set the value of the Matrix

  set <- function(y){

    x <<- y

    inv <<- NULL
  }

  get <- function() x                                       #gets the value of the Matrix

  setInverse <- function(solveMatrix) inv <<- solveMatrix   #sets the value of the invertible matrix

  getInverse <- function() inv                              #gets the value of the invertible matrix

  list(set = set, get = get, 

       setInverse = setInverse, getInverse = getInverse)

}

#  makeCacheMatrix function definition ends




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

#  The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an 

#  input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.

#  In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 

#  and set the invertible  matrix by using the solve function.

#  In case inverse matrix from makeCacheMatrix((matrix) has some value in it then

#  it returns a message  "Fetching Cached Invertible Matrix" 

#  and the cached object


#  cacheSolve function definition starts


cacheSolve <- function(x, ...) {

## Returns a matrix that is the inverse of 'x'

#get the value of the invertible matrix from the makeCacheMatrix function

inv <- x$getInverse()

  if(!is.null(inv)) {                                 #checks if inverse matrix is not NULL
                                     
    message("Fetching Cached Invertible Matrix")      #Prints message: Fetching Cached Invertible Matrix 

    return(inv)                                       #returns the invertible matrix

  }

  data <- x$get()                                     #gets the original Matrix Data 

  inv <- solve(data)                                  #uses solve function to inverse the matrix

  x$setInverse(inv)                                   #sets the invertible matrix 

  return(inv)                                         #returns the invertible matrix

}

#  cacheSolve function  definition ends
