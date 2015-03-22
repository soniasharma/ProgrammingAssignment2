## We use <<- operator to create a special object that stores a matrix and caches its inverse.

## makeCacheMatrix :- creates a vector which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
	set <- function(y){
		x <<- y
		I <<- NULL
	}
	get <- function()x
	setinverse <- function(inverse)I <<-inverse
	getinverse <- function()I
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) 
}


## cacheSolve :- calculates the inverse of the special 'matrix' created by above function. But it first checks if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the vlaue of the inverse in the cache.

cacheSolve <- function(x, ...) {
	I <- x$getinverse()
	if(!is.null(I)){
		message("getting cached data")
		return(I)
	}
	data <- x$get()
	I <-solve(data, ...)
	x$setinverse(I)
	I        ## Return a matrix that is the inverse of 'x'
}
