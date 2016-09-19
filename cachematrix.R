
## These functions are designed to create a matrix, for which if the inverse is calculated, it will be cached. (makeCacheMatrix) 
## The object created by makeCacheMatrix has functions that can be accessed, due to being returned as a list.
## Otherwise, the inverse will first be calculated and then stored. (cacheSolve)


#This function creates a special "matrix" object that can cache its inverse. #Returns list of functions, which can be accessed from outside using the obj created by the function.
makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
    
        set <- function(y) {  # changes the vector stored in the main function.
                x <<- y  #subs the vector with the y only in the set function. 
                inv_x <<- NULL  # old inv_x not needed anymore, so reset.
        }
        get <- function() x # a function that returns the vector x stored in the main function. No input req'd.
        setinv <- function(invrse) {  #doesn't calc the inverse, simply store the value of the input in a var.
                inv_x <<- invrse   # the <<- is to change value of something that's already def'd outside of this envmt
        }

        getinv <- function() inv_x  #returns the value of the input in above func in main function. if we 've stored something with
        #setinv then will get that back. 
        
        list( set = set, get = get, # to store the 4 functions in the makeCacheMatrix func, need function list()P, so when we assign makeCacheMatrix to an object, the object has all the 4 funcs.
              setinv = setinv, 
              getinv = getinv)
}


## Write a short comment describing this function
# This function computes inverse of special "matrix" returned by makeCacheMatrix() above. If the matrix
# inverse is already calculated, and matrix is the same/not changed, then returns the inverse from cache.

cacheSolve <- function(x, ...) {  #input is the object where makeCacheMatrix is stored. 
        #first thing the function does is verify the value inv_x, stored previously with getinv, exists or doesn't.
       # if it exists in mem, it returns a msg and the value inv_x, that is sup to be the inv, but not neccess.
         ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinv()
                if (!is.null(inv_x)) {
                        message("getting cached data")
                        return (inv_x)  # this would end the func if that's the case. 
                }
        
        data <- x$get()  #get the matrix (if above was null)
        inv_x <- solve(data, ...) #get the inverse matrix using the solve func
        x$setinv(inv_x)   #store the cacl'd inverse in the obj gen'd by makeCacheMatrix
        inv_x  
        
}



#try out the functions : 
d <- matrix(data = c(1,1,4,0,3,1,4,4,0), 3,3)
d
x <- makeCacheMatrix(d)
x$getinv()
cacheSolve(x)

d <- matrix(data = c(1:4), 2,2)
d
x <- makeCacheMatrix(d)
x$getinv()
cacheSolve(x)

