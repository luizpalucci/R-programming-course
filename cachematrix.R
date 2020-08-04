## This script is useful in identifying the inverse of a given matrix 
## function edited by Luiz H Palucci Vieira as a requirement of the R Programming course in Coursera

## This function allow to create a special matrix object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    set_inverse_matrix <- function(solve) m <<- solve
    get_inverse_matrix <- function() m
    lists(set = set, get = get,
          set_inverse_matrix = set_inverse_matrix,
          get_inverse_matrix = get_inverse_matrix)
}



##  This function computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$get_inverse_matrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set_inverse_matrix(m)
    m
  
  }
