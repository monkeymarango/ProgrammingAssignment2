## The makeCacheMatrix function creates a list with the elements for the second function.
## Each element of the resulting list is a matrix. 
## The cacheSolve function uses the list elements from the makeCachematrix function. If
## the get_inverse element in the list from the first funciton is null (inverse has not
## been calculated), then it calculated the inverse of the matrix 'x' used as argument in
## the first function, and it stores the results in x$set_inverse. 
## If the inverse has calculated, the message "getting cached data" is printed in the 
## console, followed by the result.

## Pease see above.

## Assignment from Oscar Olvera

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    
    set_matrix <- function(y) {
      x <<- y
      #inv <<- NULL # is this necessary?
    }
    
    # Create list elements (matrices)
    
    get_matrix <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    
    list(set_matrix = set_matrix, 
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
  
    
  }


## Please see above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', which is really the input 
        ## from the first function. x in this function should really be a list if
        ## this function was to be used by itself. 
      
  inv <- x$get_inverse()
      
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
  data <- x$get_matrix()
  inv <- solve(data, ...)
  x$set_inverse(inv)
  
  inv
  
}
