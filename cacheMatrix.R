makeCacheMatrix <- function(x=matrix()) {
       inv<-NULL
       set<-function(y) {
            x<<-NULL
            inv<<-NULL
       }
       get <- function()x
       setInverse <- function(inverse) inv <<- inverse
       getInverse <- function() inv
       list(set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


cacheSolve <- function(x,...) {
     inv <- x$getInverse()
     if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
     }
     mat <- x$get()
     inv <- solve(mat,...)
     x$setInverse(inv)
     inv
}

source("Desktop/cacheMatrix.R")
my_matrix <- makeCacheMatrix(matrix(0:3,2,2))
my_matrix$get()

cacheSolve(my_matrix)

cacheSolve(my_matrix)

my_matrix$getInverse()









