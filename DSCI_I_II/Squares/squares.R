library(grid)
library(png)
library(OpenImageR)
library(igraph)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

normalize <- function(xs) {
  a <- min(xs)
  b <- max(xs)
  (xs - a)/(b - a)
}

gaussian_k <- function(xy_length = 2, sigma = 1.0, range_gauss = 2) {
  gaussian_formula = function(x, y)  1/(2 * pi * (sigma ^ 2)) * (exp(-(x ^ 2 + y ^ 2)/(2 * sigma ^ 2)))
  tmp_seq = seq(-range_gauss, range_gauss, length = xy_length)
  tmp_outer = outer(tmp_seq, tmp_seq, gaussian_formula)
  return(tmp_outer/sum(tmp_outer))
}

logistic <- function(k,x0,x) 1 / (1 + exp(-k*(x-x0)))

sobel <- matrix(c(-100,0,100,-200,0,200,-100,0,100),3,3)

sobelAll <- function(bmp) {
  hs <- convolution(bmp, sobel)
  vs <- convolution(bmp, t(sobel))
  pmax(normalize(abs(hs)),normalize(abs(vs)))
}

blur <- function(bmp, w) {
  convolution(bmp, gaussian_k(w))
}

mkgraph <- function(bmp, t = 0.2) {
  edges <- c()
  ws <- c()
  ei <- 1
  kn <- 2
  ns <- 1:kn
  ns <- c(rev(-ns),ns)
  for (ri in (kn+1):(nrow(bmp)-(kn+1))) {
    for (ci in (kn+1):(ncol(bmp)-(kn+1))) {
      if (bmp[ri,ci] > t) {
        for (i in ns) {
          for (j in ns) {
            if (bmp[ri+i, ci+j] > t) {
              x <- ri + ci*nrow(bmp)
              y <- (ri+i) + (ci+j)*nrow(bmp)
              
              ws[(ei-1)/2+1] <- sqrt(i*i+j*j)
              edges[ei]   <- x
              edges[ei+1] <- y
              ei <- ei + 2
              
              rip <- 1+((y-1) %% nrow(bmp))
              cip <- (y-1) %/% nrow(bmp)
            }
          }
        }
      }
    }
  }
  g <- make_undirected_graph(as.character(edges))
  return(g)
}

mklayout2 <- function(bmp, vs) {
  l <- matrix(nrow=length(vs), ncol=2)
  i <- 1
  for (n in vs) {
    l[i,1] <- ((n-1) %% nrow(bmp) + 1)
    l[i,2] <- ((n-1) %/% nrow(bmp))
    i <- i + 1
  }
  return(l)
}

mklayout <- function(bmp, g) {
  mklayout2(bmp,as.numeric(as.vector(names(V(g)))))
}

getDiameterPath <- function(es, t) {
  sp <- shortest.paths(t)
  sp[sp >= Inf] <- 0
  i <- arrayInd(which.max(sp), dim(sp))
  p <- get.shortest.paths(t,i[1],i[2])
  res <- list(path = p$vpath, points = mklayout2(es,as.numeric(names(unlist(p$vpath)))))
}

# Get paths that are at least n points long.
# Once a path is found, all connected verticies
# are excluded from consideration in further paths.
getSignificantPaths <- function(es, t, n=10) {
  i <- 1
  ps <- list()
  while (diameter(t) > n) {
    cat("Vertices", length(V(t)), "diameter", diameter(t), "\n")
    p <- getDiameterPath(es,t)
    ps[[i]] <- p$points
    ns <- names(unlist(p$path))
    sp <- shortest.paths(t)
    rs <- c()
    cat("Found path with", length(p$points), "points.", "\n")
    rj <- 1
    pj <- which(names(V(t)) == ns[1])
    for (j in 1:length(V(t))) {
      if(sp[j,pj] < Inf) {
        rs[rj] <- names(V(t))[j]
        rj <- rj + 1
      }
    }
    t <- delete.vertices(t,unique(rs))
    i <- i + 1
  }
  return(ps)
}

# Compute the error from fitting a line
# over a window lenght of w on points ps.
# The input is treated as if the start and
# end are connected.
windowFit <- function(ps, w=20) {
  n <- nrow(ps)
  ps <- rbind(ps[(n-w/2+1):n,], ps, ps[1:(w/2),])
  ss <- c()
  for (i in 1:n) {
    is <- (1:w) + i
    fitX <- lm(ps[is,2] ~ ps[is,1])
    fitY <- lm(ps[is,1] ~ ps[is,2])
    ss[i] <- min(summary(fitX)$sigma, summary(fitY)$sigma)
  }
  return(ss)
}

# Given the fit error, find the corners of
# a shape.  This function returns the indices
# of corners.
findCorners <- function(ss) {
  l <- min(ss)
  h <- max(ss)
  t <- (h+l)/2
  n <- length(ss)
  state <- 1
  p <- 0
  start <- 0
  peaks <- c()
  j <- 1
  iStart <- which.min(ss)
  ss <- c(ss, ss)
  for (x in iStart:(iStart + n)) {
    i <- (x %% n) + 1
    if (state == 1) {
      # we are looking for high values.
      if (ss[i] > t) {
        start <- x
        state <- 2
      }
    } else if (state == 2) {
      # we are looking for low values again.
      if (ss[i] <= t) {
        peaks[j] <- 1+((start + which.max(ss[start:x]) - 2) %% n)
        state <- 1
        j <- j + 1
      }
    }
  }
  return(peaks)
}

evaluatePath <- function(filename, blurwidth, edgethreshold, pathlength, w, qualityThreshold){
# Example:  Find the buildings in a farm field.
pic <- readPNG(filename)
# Find the edges
es <- sobelAll(blur(pic[,,1],blurwidth))
# We can visualize the edge find with the following command:
grid.raster(es)
# Connect pixels into a graph based on the given threshold.
g <- mkgraph(es, edgethreshold)
# A layout is the coordinates of the pixels in the graph (you can plot this with plot(l)).
l <- mklayout(es, g)
# find the minimum spanning tree of the graph
t <- mst(g)
# Use the minimum spanning tree to find significant edges as
# sequential collections of connected points.
ps <- getSignificantPaths(es,t,pathlength) # 20 is the minimum length of points

# This code will plot the vertices
# of the graph, then plot lines in red
# for the "significant" paths.
plot(l, main = "Significant Paths", cex=0.2, pch=20)
for (ls in ps){
lines(ls, col="red")
  }

emptlist <- list()
#squarelist <- list()
for (i in 1:length(ps)){
#moved red sig paths

# Now that we have the points separated out, measure
# the error of fiting a line to 10 points in a row.
ss <- windowFit(ps[[i]],w)
# This will find the corners based on the error.
cs <- findCorners(ss)
if (length(cs) == qualityThreshold)
  quality <- 1
else
  quality <-  0
qs <- quality

#store corners and # of corners
emptlist[[i]] <- list(ps=ps[[i]], cs=cs, qs=qs)
#emptlist[[i]] <- emptlist[emptlist[[i]]$qs==1]

}
thelist <- list()
 for (i in 1:length(emptlist)){
  if (emptlist[[i]]$qs==1) 
  thelist[[i]]  <- emptlist[[i]]
  if (emptlist[[i]]$qs==1) 
  plot(emptlist[[i]]$ps)
  points(ps[[i]][emptlist[[i]]$cs,], col="red", pch=2)
 }
#print(emptlist)[[1]]$cs
storenull <- c()
if (length(thelist) > 0)
for(i in 1:length(thelist)){
storenull <- c(storenull, is.null(thelist[[i]]))
}
if (length(thelist) > 0)
print(length(thelist[storenull==FALSE]))
else
  (print(0))
}


evaluatePath("farm-small.png", 10, .2, 20, 10, 4)
dev.off()

######
# Other picture to look for squares in
######## 

# evaluatePath("apple-small.png", 8, .2, 20, 10, 4)
# dev.off()

# evaluatePath("houghton-sat-small.png",10, .25, 20, 10, 4)
# dev.off()

# evaluatePath("9squares.png", 20, .2, 20, 10, 3)
# dev.off()

# evaluatePath("onesquare.png", 10, .2, 20, 10, 4)
# dev.off()

# evaluatePath("6squares.png", 3, .2, 20, 10, 4)
# dev.off()

# evaluatePath("4squares.png", 7, .1, 20, 10, 4)
# dev.off()

# evaluatePath("checkers.png", 1, .2, 20, 10, 1)
# dev.off()

# evaluatePath("awesome.png", 1, .3, 20, 10, 4)
# dev.off()
