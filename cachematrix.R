## Put comments here that give an overall description of what your
## functions do

# Print some instructions
print("", quote=FALSE)
print("Usage:", quote=FALSE)
print(" > my_var <- makeCacheMatrix(my_matrix)", quote=FALSE)
print(" > cacheSolve(my_var)", quote=FALSE)
print("The functions will solve the inverse of the matrix given to my_var", quote=FALSE)
print("once, and will save it. So long as you do not change the matrix given to", quote=FALSE)
print("my_var, the inverse will always be stored. If you want to save multiple", quote=FALSE)
print("matrix inversions, declare a new variable for each.", quote=FALSE)
print("", quote=FALSE)

## makeCacheMatrix takes in a matrix (assumed to be invertible) and return
#   a list of functions:
#   get.mat() when called, returns the matrix passed to makeCacheMatrix
#   set.mat.inv() when called with an inverted matrix as its argument,
#       overwrites the current value of the inverted matix variable 'inv'
#       with the argument it was passed, using lexical scoping
#   get.mat.inv() when called returns the value of the inverted matrix
#       variable 'inv'
makeCacheMatrix <- function(x = matrix()) {
    # Initialize a variable inv to pass around an invert of the matrix x
    inv <- NULL

    # Define a function that returns the matrix that it was passed
    get.mat <- function() x

    # Define a function that passes an inverse matrix to inv according to
    #   lexical scoping rules
    set.mat.inv <- function(inverse.mat) inv <<- inverse.mat

    # Define a function that returns the inverse matrix that it was passed
    get.mat.inv <- function() inv

    # Return a list of the functions that we defined here, to be called up
    #   later as makeCacheMatrix$setMatrix, etc.
    list(
            getMatrix = get.mat,
            setMatrixInverse = set.mat.inv,
            getMatrixInverse = get.mat.inv
        )
}

## cacheSolve takes a list output from makeCacheMatrix and tests to see if
#   the list is carrying an inverted matrix. If it is, it simply retrieves
#   the inverted matrix and returns it. If the inverted matrix is not
#   found (is NULL), cacheSolve retrives the original matrix, solves for
#   its inverse, saves the inverse matrix back to the list from
#   makeCacheMatrix, and returns the inverted matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # Retrieve the saved value of inv (inverse of matrix 'x')
    inv <- x$getMatrixInverse()

    # Test for NULL value; if inv already exists, return it.
    if (!is.null(inv)) {
        print("Retrieving inverted matrix data...", quote=FALSE)
        return(inv)
    } else {
        # If inv is does not exist, call the function that returns the
        #   matrix, solve for the inverse of that matrix, and save it to
        #   inv, then return the inverse.
        print("Solving for the matrix inverse...", quote=FALSE)
        mat <- x$getMatrix()
        inv <- solve(mat)
        x$setMatrixInverse(inv)
        return(inv)
    }
}
