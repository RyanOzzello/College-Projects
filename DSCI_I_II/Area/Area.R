# Load png package, frid, and gridExtra
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
library("png")
library("grid")
library("gridExtra")
library("ggplot2")

# Read image
app <- readPNG("apple.png")

# Assigning colors
red <- app[,,1]
green <- app[,,2]
blue <- app[,,3]

# As rasters
redRaster <- as.raster(red)
greenRaster <- as.raster(green)
blueRaster <- as.raster(blue)
appRaster <- as.raster(app)

# View pictures
plot(appRaster)
plot(redRaster)
plot(greenRaster)
plot(blueRaster)

# View by color
app.R = app
app.G = app
app.B = app
# Set 2 of 3 color channels to 0
app.R[,,2:3] = 0
app.G[,,1] = 0
app.G[,,3] = 0
app.B[,,1:2] = 0
# View
image1 = rasterGrob(app.R)
image2 = rasterGrob(app.G)
image3 = rasterGrob(app.B)

grid.arrange(image1, image2, image3, nrow=1)

# Count paper pixels
app <- readPNG("apple.png")
thresh <- 0.6
app[,,2]=0
app[,,3]=0
app[app < thresh]=0
appRaster <- as.raster(app)
plot(appRaster)
paperPxlCnt <- length(app[app > 0])

# Count apple pixels
appOnly <- readPNG("apple.png")
app <- readPNG("apple.png")
thresh2 <- 0.83
appOnly[,,2]=0
appOnly[,,3]=0
appOnly[appOnly < thresh2]=0
appRaster <- as.raster(appOnly)
plot(appRaster)
# Fill in middle
appOnly[80:190,130:280,1]=app[80:190,130:280,1]
app1ras <- as.raster(appOnly)
plot(app1ras)
appPxlCnt <- length(appOnly[appOnly > 0])

pixRatio <- appPxlCnt/paperPxlCnt
paperArea <- 8.5*11
appArea <- paperArea * pixRatio
paste(toString(appArea), "in^2")
