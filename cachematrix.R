#############
##
## cachematrix.R: A pair of functions that cache the inverse of a matrix.
##
##              
#############

##      inv_mat <<- inverse of the original matrix
##      org_mat <<- copy of the original matrix

##      makeCacheMatrix: This function creates a special "matrix" object 
##                       that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the "holder" for a copy of the original x matrix
        ## as well as the inverse of x
        inv_mat <<- NULL 
        org_mat <<- x
        
        ## If x is a valid matrix argument
        if (is.matrix(x)) {                
                ## Calculate the invese of x and save the results in inv_mat
                inv_mat <<- solve(x)
        }
        
        ## return the matrix that was passed in
        x
}

##      cacheSolve:     This function computes the inverse of the special 
##                      "matrix" returned by makeCacheMatrix above. If the 
##                      inverse has already been calculated (and the matrix 
##                      has not changed), then cacheSolve should retrieve the 
##                      inverse from the cache.

cacheSolve <- function(x, ...) {        
        
        ## check to see that ...
        ##    x is a valid matrix
        ##    copy of the original matrix is a valid matrix
        ##    x and original matrix have the same dimention
        ##    x and original matrix are equivalent 
        ##    the cached inverse of x is set
        ## 
        ##  if all these are true, return the cached inverse of x
        if (is.matrix(x) && 
            is.matrix(org_mat) && 
            dim(x) == dim(org_mat) && 
            all(x == org_mat) &&
            is.matrix(inv_mat)) { 
                return (inv_mat)
        }
        
        
        ## If we get here, either the cache is not set or x has changed.
        ## In either case, re-calculate the cache
        makeCacheMatrix(x)
        
        ## Return the cached inverse of x
        inv_mat
}
