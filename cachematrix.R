
## The makeCacheMatrix sets x as a matrix. 
## A list is created to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      s<- NULL
      set <- function (y) {
            x<<-y
            s<<-NULL
      }
      get <- function()x
      setsolve <- function (solve) s <<- solve
      getsolve <- function () s
      list (set =set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve calculates the inverse of the matrix setting in makeCacheMatrix. 
## If the inverse has already been calculated, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      s <- x$getsolve()
      if(!is.null(s)){
            message("getting cached data")
            return (s)
      }
      data <- x$get()
      s <- solve(data,...)
      x$setsolve(s)
      s
}
