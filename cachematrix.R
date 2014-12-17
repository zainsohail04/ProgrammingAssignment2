## Matrix Inverse

## Takes Matrix input and gives output as inverse. Cache's for faster processing speed.

makeCacheMatrix <- function(x = matrix()) {
        mat<-NULL
        assign<- function(l){
                x<<-l
                mat<<-NULL
        }
        get<- function() x
        assigninv<-function(solve) mat<<-solve
        getinv<-function() mat
        list(assign = assign, get = get,
             assigninv = assigninv, 
             getinv = getinv)
}



## Uses Cached inverse, if available else calculates the inverse itself

cacheSolve <- function(x, ...) {
        mat<-x$getinv()
        if(!is.null(mat)){
                message("getting cached data")
                return(mat)
        }
        data<-x$get()
        mat<-solve(data, ...)
        x$assigninv(mat)
        mat
}
