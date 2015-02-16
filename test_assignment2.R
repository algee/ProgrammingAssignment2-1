x <- matrix(c(1,3,1,4),nrow=2,ncol=2)
y <- -0.5*x
z <- y

M <- makeCacheMatrix(x)
cacheSolve(M)

M$set(y)
cacheSolve(M)

M$set(z)
cacheSolve(M)

cacheSolve(M)

M$set(x)
cacheSolve(M)

