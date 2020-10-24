library(png)
library(quantmod)
setwd(paste(dirname(rstudioapi::getSourceEditorContext()$path), "/Data/frames", sep=""))
dev.off()

n <- length(list.files("frames/", "frame.*")) # Number of frames
r <- 30.005392    # Frame rate
ts <- (0:(n-1))/r # Time of each index

# iIitialization
vect=c()
for (i in 1:n) {
  name <- sprintf("frames/frame%05d.png", i)
  frame <- readPNG(name)
  rs <- frame[,,1] # Matrix of red values
  gs <- frame[,,2] # Matrix of green values
  bs <- frame[,,3] # Matrix of blue values
  
  # Per frame computation here
  vect=c(vect, sum(rs))
}

# Final computation and visualization
# Smoothing
smoothvect <- lowess(vect, f=.045)
plot(smoothvect, type="l")
smoothvect <-unlist(smoothvect,use.names = FALSE)
beats <- findPeaks(smoothvect[524:length(smoothvect)])
beats <- length(beats)
beats
# Heart rate
totsec <- 523/r
totsec
bps <- beats/totsec
bpm <- bps *60
heart_rate <- bpm
heart_rate






