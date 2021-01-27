## Un par de funciones que almacenan en caché la inversa de una matriz

## Crea un objeto de matriz especial que puede almacenar en caché su inverso
makeCacheMatrix <- function( m = matrix() ) {

    ## Inicializar la propiedad inversa
    i <- NULL

    ## Método para configurar la matriz
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Método de obtener la matriz
    get <- function() {
        ## Devuelve la matriz
        m
    }

    ## Método para establecer la inversa de la matriz
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Método para obtener la inversa de la matriz
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Devuelve una lista de los métodos
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



cacheSolve <- function(x, ...) {

    m <- x$getInverse()

 
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Obtener la matriz de nuestro objeto
    data <- x$get()

    ## Calcular la inversa usando la multiplicación de matrices
    m <- solve(data) %*% data

    ## Establecer la inversa al objeto
    x$setInverse(m)

    ## Devuelve la matriz
    m
}