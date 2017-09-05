## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special matrix 
## it is a list that coontaining function to set or get the value of the vector
## and set or get the value of its inverse


makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    
    set <- function(y){
            x <<-y
            xinv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) xinv <<- solve
    getinverse <- function() xinv 
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the matrix created by makeCacheMatrix
## if the inverse has already been calculated it retrieves it from cache

cacheSolve <- function(x, ...) {
       ##print("1")
        xinv <- x$getinverse()
      ##print("2")
         if(!is.null(xinv)) {
               message("saving computing time getting cached inverse")
               return(xinv)
       }
      ##print("3")
       data <- x$get()
       ##print("4")
       xinv <- solve(data, ...)
       ##print("5")
       x$setinverse(xinv)
       ##print("6")
       xinv
       
                ## Return a matrix that is the inverse of 'x'
}


