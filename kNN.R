# kNN
# 2016/4/3
frame()

cols <- c('red', 'blue')

input <- matrix(scan('./data.txt', sep=','), 3)
X <- rbind(input[1,], input[2,])
b <- input[nrow(input),]
print(X)

plot(X[1,], X[2,], pch=16, cex=2, col=cols[b])

distance <- function(mat, vec)  sqrt(apply((mat-vec)^2, 2, sum))

whichKMin <- function(k, vec) {
  update <- vec
  index <- NULL
  for (i in 1:k) {
    index <- c(index, which(vec==min(update)))
    update <- update[-index]
  }
  return(index[1:k])
}

repeat {
  cat('Input test pattern\n')
  x <- readline()
  x <- as.numeric(unlist(strsplit(x,',')))

  d <- distance(X,x)
  cat('Distance\n')
  print(d)
  pos <- whichKMin(3,d)
  cat('Position\n')
  print(pos)
  class <- as.numeric(names(which.max(table(b[pos]))))
  cat('Judge: Class', class, '\n')

  points(x[1], x[2], pch=16, cex=2, col=cols[class])
  ## In case, use test-data for learning-data
  X <- cbind(X, x)
  b <- c(b, class)
}


