## 1) set the matrix
## 2) get the matrix
## 3) set the inverse
## 4) get the inverse
makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    
    x <<- y
    z <<- NULL
  }
  message("Matrix Cached")
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function

## Returns the inverse of the original matrix that you input into "makeCacheMatrix"
      
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    #gets computed inverted matrix if its already been cached
    if(!is.null(inv)) {
      message("getting data from cache")
      return(inv)
    }
    ##Computes inverse  
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

##Usage
## x<- matrix(c(1,3,2,4), 2, 2)
## m<- makeCacheMatrix(x)
## > cacheSolve(m)



  