## This function sets the value of the matrix and gets the value of the matrix
## and sets the inverse of the mtarix and gets the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
m<-NULL
        set<-function(y)
                {
                x<<-y
                m<-NULL
        }
        get<-function()x
        setinverse<-function(inverse)m<<-inverse
        getinverse<-function()m
        list(set=set,get=get,getinverse=getinverse,setinverse=setinverse)
                
}


## This function returns the inverse if not already cached and if its already
## cached, it will return the existing inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
                }
        data<-x$get()
        m<-solve(data)
        x$setinverse(m)
        m
}
