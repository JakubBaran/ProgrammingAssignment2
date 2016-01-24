#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. These pair of functions cache the inverse of a matrix.

# this function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL # a place holder for a future value of m

 set <-function(y) {x<<-y; m<<-NULL} # This function sets the matrix 'x' to a new matrix 'y' and restarts m.

 get<-function() {x} #returns matrix x

 setinversion <-function(minverse) m <<-minverse #sets m to minverse

 getinversion<-function() m #returns m
 
 list(set=set, get=get, setinversion=setinversion,getinversion=getinversion) # returns a list with all the functions defined above

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

 m<-x$getinversion()

 if(!is.null(m)) {mesage("getting cached data"); return(m)} # checking if the inverse has been already calculated and if so getting it from the cache.

m_data <-x$get()
m <-solve(m_data,...) #if not it calculates it

 x$setinversion(m) # uses setinversion function to set the value of minverse in the cache
 
return(m)

}
