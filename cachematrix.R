# Following is the function defined for creating cache of Matrix:
#
# Reason: Why to cache Martix ?
#       : Matrix inversion is usually a COSTLY computation 
#       : and there may be some benefit to caching the inverse of a matrix 
#       : rather than compute it repeatedly.
#       : FOR EXAMPLE: IT SAVE TIME, etc

#Below are a pair of functions that are used to create a special object that
#stores a matrix and caches its inverse.
makeCacheMatrix <- function(x = matrix()) {
                          # INPUT: x = a square invertible matrix
                          
                          # OUTPUT: a list containing functions to
                          #         -> set the matrix
                          #         -> get the matrix
                          #         -> set the inverse
                          #         -> get the inverse
                          
                          # NOTE: OUTPUT(i.e. list) is used as the input to cacheSolve()
                              inv <- NULL
                            
                            set <- function(y) {
                                  x <<- y
                                  inv <<- NULL
                            }
                            
                            get = function() x
                            setinv = function(inverse) inv <<- inverse 
                            getinv = function() inv
                            list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        # INPUT : x = output of makeCacheMatix function [ref line 11 of same file for function code]
        #OUTPUT : Return a matrix that is the inverse of 'x'
                            
          inv = x$getinv()
  
        # in case cache is already present, will use that only (SAVES TIME!)
        
          if (!is.null(inv)){
                # Obtain form cached data
              message("Cached Data already present, using the sme")
              return(inv)
          }
          
        # Else: if cached data not present:
          mat.data = x$get()
          inv = solve(mat.data, ...)
        
          # Following will save the value of inverse in cache
          x$setinv(inv)
          
          # finally this will return the value of inv 
          # NOTE: even just writing 'inv' would have worked
          return(inv)
          
}
