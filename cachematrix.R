## Samitha Madduma Patabendige
## cacheSolve is a function to calculate matrix inversion.
## makeCacheMatrix is a function to cache( internally store)
## a given matrix and its inverse (if the inverse exist)



## makeCacheMatrix is function that takes invertible matrix
## internally it creates a list that stores functions to get,set
## the original matrix and get,set the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invs) inv <<- invs
    getinv <- function() inv
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve takes in an invertible matrix
## if the input matrix has its inverse stored,
## it returns the inverse with message 
## "**** GETTING CACHED mAT ****"
## otherwise it calculates the inversion of the input matrix and 
## return the calculated inversion with message 
## "**** GETTING CALCULATED INVERSION MAT ****"
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("**** GETTING CACHED mAT ****")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    message("**** GETTING CALCULATED INVERSION MAT ****")
    inv
}

##mySimpleTest is not part of the assignment, but I used it to test the 
## retrieval of calculated inversion and cached inversion
mySimpleTest<-function(){
#for testing, taken the "hilber" part from solve() help page
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); #input matrix
sh8 <- solve(h8)  #inverse of h8
res.ident<-round(sh8 %*% h8, 3) #save the multiplication of mat and inverse mat (i.e identity) in a variable

if(dim(h8)[1]==dim(sh8)[1] & dim(h8)[2]==dim(sh8)[2] ){
    expected_res_mat<-diag(dim(h8)[1])#diag(matrix) makes an identity matrix with given dim
    if(identical(res.ident,expected_res_mat)){
        print("h8 inverse is sh8 because the result of h8*sh8 is Identity matrix")
    }
}


m.without.cache<-makeCacheMatrix(x = h8)

calculatedInverse<-cacheSolve(x = m.without.cache)    

cachedInverse<-cacheSolve(x = m.without.cache)

if(identical(  round(calculatedInverse%*%m.without.cache$get(),3),
               diag(dim(calculatedInverse)[1]))) {
    print("TEST1: PASSED! calculated inverse is used to test the mul to identitiy")
}

if(identical(  round(cachedInverse%*%m.without.cache$get(),3),
               diag(dim(cachedInverse)[1]))){ 
    print("TEST2: PASSED! cached inverse is used to test the mul to identitiy")
}
}

##If you want to test the cacheSolve() and makeMatrix() functions
##uncomment the line below and press ALT+CTR+R
mySimpleTest()