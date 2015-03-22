## This function get a matrix and checks it, if the matrix is invertible.
## This fuction is like a class in OOP that, we define functions(set and get,...) and mat_inv is the attribute of our class.  
makeCacheMatrix <- function(x = matrix()) {
	#mat_inv contains the inverse value of a matrix, by default is null
  	mat_inv <- NULL
  	#set value of matrix
  	set <- function(input_mat){
    	#checking if the matrix is invertible using determinant of matrix
    	if(det(input_mat)==0){
      		message("The matrix is not invertible!")
    	}
    	else{
    		x <<- input_mat
    		mat_inv <<- NULL
    		}
  	}
  	#get value of matrix
  	get <- function() x
  	#set inverse value of matrix
  	setinv <- function(inverse) mat_inv <<- inverse
  	#get inverse value of matrix
  	getinv <- function() mat_inv
  	list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## This function gets in input an object of makeCacheMatrix(a Matrix) and the calculates the inverse matrix using Slove()
## function. It checks in the cache if the inverse of input matrix is already in the cache or no. if yes, so it doesn't compute the inverse of input matrix and returns directly the inverse of matrix from cache.   
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	#get the inverse of matrix 
  	inv_val <- x$getinv()
  	#checking if the inverse of the matrix is in cache 
  	if(!is.null(inv_val)) {
    		message("Retrieved Data from cache!")
    		return(inv_val)
  	}
  	else{
    		message("The inverse of matrix is computed!")
  	}
  	data_matrix <- x$get()
  	inv_val <- solve(data_matrix, ...)
  	x$setinv(inv_val)
  	inv_val
}
