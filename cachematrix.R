## Haciendo que primero revise si existe la matriz en la cache para 
# no reprocesar y así ahorrar memoria de procesamiento.


## La primera función genera una lista para definir y determinar valores 
# de una matriz, luego definir y determinar los valores del inverso de la matriz.

makeCacheMatrix <- function(m = matrix()) {
      invertedmatrix <- NULL
      set <- function(matrix) {
            m <<- matrix
            invertedmatrix <<- NULL
      }
      get <- function() m
      setinverse <- function(inverse) invertedmatrix <<- inverse
      getinverse <- function() invertedmatrix
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## La segunda función primero revisa si la matriz ya se calculó; si es el caso, 
# regresa el valor del resultado en la memoria. Si no, lo determina. 

cacheSolve <- function(m, ...) {
      invertedmatrix <- m$getinverse()
      
      if (!is.null(invertedmatrix)) {
            message("getting cached matrix")
            return(invertedmatrix)
      }
      
      data <- m$get()
      invertedmatrix <- solve(data, ...)
      m$setinverse(invertedmatrix)
      invertedmatrix
}

#### Test ####

#mat <- matrix(rpois(9, 5), nrow = 3, ncol = 3)
#matr <- makeCacheMatrix(mat)
#mat == matr$get()
#cacheSolve(matr)
#cacheSolve(matr) # ya regresa la leyenda "getting cached matrix"
