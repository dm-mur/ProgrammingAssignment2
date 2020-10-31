#Assignment 2 Week 3
#Make cache matrix
makeCachematrix<-function(x=matrix()){
        inv<-NULL
        set<-function(y){
                x<<-y
                inv<<-NULL #Initializing inverse as NUL
        }
        get<-function() {x}
        setInverse<-function(inverse) {inv<<-inverse}
        getInverse<-function(){inv}
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}
#Cache the data
cacheSolve<-function(x,...){
        inv<-x$getInverse() 
        if(!is.null(inv)){  #Checking if inverse is NULL
                message("getting cached data")
                return(inv) #returns inverse value
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        inv
        
}
#Formulating a matrix
dmatrix<-makeCachematrix(matrix(3:6,2,2))
dmatrix$get()
#checking if the inverse of the matrix is NULL
dmatrix$getInverse()
#cache the matrix

cacheSolve(dmatrix)

cacheSolve(dmatrix)
#Getting the inverse of the matrix
dmatrix$getInverse()

