
# makeCacheMatrix


## makeCacheMatrix is a function used to cache an inverse of a matrix that is calculated by cacheSolve() 
## The function argument is a matrix (x). makeCacheMatrix creates a special object containing functions
## that can be called by cacheSolve():
##  - "get" to get the data from the original matrix 
##  - "getsolve" to get a cached inverse of that matrix 
##  - "setsolve" which contains the function solve() to calculate the inverse
## The function set() can be used to store a different matrix in makeCacheMatrix 


makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL                     #  the 'inverse' of the matrix 
  
    set <- function(y) {            # function to change the stored value
            x   <<- y
            inv <<- NULL
  }
  
  ## object methods can be called by cacheSolve()
  get <- function() { x }          # get the original matrix 
  
  setsolve <- function(solve)  { inv <<- solve }
                                   # calculates the inverse of the matrix using the 
                                   # function solve() and assign to the "inv" object
  
  getsolve <- function() { inv }   # gets a cached value of the object "inv" 

  ## combine the functions in a list
  list(get = get,
       set=set,
       setsolve = setsolve,  
       getsolve = getsolve)                         
}
  

# cacheSolve

## cacheSolve is a function that can access list of functions stored in makeCacheMatrix to:
##  - retrieve the inverse of a matrix from a cache in makeCacheMatrix
##  - or, when the fetched object is NULL, it solves the inverse of the matrix 
##    and then sets this result in the cache of makeCacheMatrix
## The function argument is an object assigned by makeCacheMatrix (x)



cacheSolve <- function(x, ...) {
     
      inv <-  x$getsolve()               # fetch the inverse from the cache in makeCacheMatrix
      if(!is.null(inv)) {         
        message("getting cached data")
        return(inv)
      } else {                           # calculate the inverse if no value was assigned to "inv"
      data <- x$get()
      inv <- solve(data, ...) 
      x$setsolve(inv)                    # store the inverse to the object "inv" in makeCacheMatrix.
      inv                                # Return a matrix that is the inverse of 'x'
      }
}
