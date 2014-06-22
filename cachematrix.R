## inv is the inverse of m

## This function do the list of things:
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y,r,c) {
                x<<-matrix(y,r,c)
                inv<<-NULL
        }
        get<-function() x
        setinv<- function(solve) inv<<-solve
        getinv<- function() inv
        list (set = set, get=get,setinv=setinv, getinv=getinv)
}


## This function first look if the cache has the inverse value, if it does
## it returns the cache data, if not, it calculate the inverse.

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}


m<-makeCacheMatrix()
m$set(1:4,2,2)
m$get()
cacheSolve(m)

