## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#this function makes the cached matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {  #the set function initialises the matrix x and sets i to null
    x <<- y
    i <<- NULL
  }
  get <- function() x #this get function returns the matrix x
  setinverse <- function(inv) i <<- inv   #sets the inverse of matrix
  getinverse <- function() i    #returns the inverse of matrix
  list(set = set, get = get,    #makes a list that conatains 4 functions specified above
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#this function calculates the inverse of matrix

cacheSolve <- function(x, ...) { 
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse() #stores the inverse in variable i
  if(!is.null(i)) {   #if the inverse is not null, then return the inverse 
    message("getting cached data")
    return(i)
  }
  data <- x$get() #get the matrix
  i <- solve(data)  #calculate inverse using the solve function
  x$setinverse(i) #store the inverse
  i     #return inverse
}


m<- matrix(data = c(1,2,-1,2,1,2,-1,2,1), nrow = 3, ncol = 3, byrow = T)
m

#test example
mat<- makeCacheMatrix(m) 
cacheSolve(mat)

