####### Put comments here that give an overall description of what your
####### functions do

# The makeCacheMatrix function produces a special matrix that store
# in it his inverse produced in R by the solve() function.
# Then the cacheSolve calculate the inverse of the matrix or print the
# one already calculared and cached in the object produced by makeCacheMatrix.

####### Write a short comment describing this function
# In details, makeCacheMatrix has a matrix as argument and return a list containing
# 4 functions to set and get the matrix and to set and get the inverse of the matrix
# which is stored in an object named "inv".

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set <-function(y){
                x<<-matrix(y,..)
                inv<<-NULL
        }
        get<- function() x
        setinverse<-function(solve) inv<<-solve
        getinverse<-function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


####### Write a short comment describing this function
# The function cacheSolve pass the special matrix made by the makeCacheMatrix
# function as an argument. CacheSolve is checking if in the getinverse element
# in makeCacheMatrix is stored the matrix inverse. If yes, cacheSolve retrieves it.
# If not, it calculates it by the solve function and stores it in "inv" object then
# used by makeCacheMatrix in the setinverse and subsequently getinverse functions.

cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinverse(inv)
        inv
}
