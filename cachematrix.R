## Juan Pablo Cano H - @pablo0935


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
      # Variable que guarda el acumulado de la inversa
      inv <- NULL
      
      # Setter de la matrix
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      # Getter de la matrix
      get <- function() x
      
      # Setter de la inversa
      setinv <- function(inverse) inv <<- inverse
      # Getter de la inversa
      getinv <- function() inv
      
      # Retorna la matrix con las funciones definidas
      list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        
      ## Retorna una matrix que es la inversa de la matrix 'x'
      inv <- x$getinv()
        
      # si esta ya esta calculada la retorna
      if (!is.null(inv)) {
        #message("Retornando la inv")
        return(inv)
      }
        
      # Si no esta calculada, se procede a ello
        data <- x$get()
        inv <- solve(data, ...)
        
      # se settea en valor de la inversa
        x$setinv(inv)
        
      # Imprime la inversa
        inv
}
