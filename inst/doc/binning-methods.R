## ---- echo=F-------------------------------------------------------------
knitr::opts_chunk$set(
  fig.align="center"
)

## ---- out.width="400px", echo=F------------------------------------------
knitr::include_graphics("Heliconius/Heliconius_08.jpeg")

## ---- fig.width=5, fig.height=4, fig.cap="(See the introduction for more information on the `plotPixels()` function.)"----
colordistance::plotPixels("Heliconius/Heliconius_08.jpeg", lower=rep(0.8, 3), upper=rep(1, 3))

## ---- fig.cap="(For more information in `getImageHist()`, see the 'Histogram method' section.)"----
binnedButterfly <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", bins=2,   lower=rep(0.8, 3), upper=rep(1, 3), plotting=T)

## ---- fig.width=7, fig.height=6------------------------------------------
par(mfrow=c(2,2))

# Generate histogram using all the default settings (3 bins per channel, get average pixel color in each bin, use RGB instead of HSV)
# See introduction vignette or documentation if lower/upper background pixel arguments are unclear
# Short version: getting rid of any pixels where R, G, and B values are ALL between 0.8 and 1 (aka white)
lower <- rep(0.8, 3)
upper <- rep(1, 3)
defaultHist <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, title="3 bins per channel, RGB")

# We already did 8 bins above, so let's do 1 bin just for sarcasm
oneBin <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, bins=1, title="1 bin (pointless but didactic?)")

# Use 2 red and green bins, but only 1 blue bin
unevenBins <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, bins=c(2, 2, 1), title="Non-uniform channel divisions")

# HSV instead of RGB
hsvBins <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, hsv=T, title="HSV, not RGB")

## ------------------------------------------------------------------------
defaultHist[1:10, ]

## ------------------------------------------------------------------------
histList <- colordistance::getHistList("Heliconius/", bins=2, plotting=F)

# Output of getHistList() is (you guessed it) a list of dataframes as returned by getImageHist()
histList[[1]]

# and list elements are named for the image they came from
names(histList)

## ---- fig.width=4, fig.height=5------------------------------------------
lower <- rep(0.8, 3)
upper <- rep(1, 3)
kmeans01 <- colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, n = 3)

## ------------------------------------------------------------------------
# Using default sample size
system.time(colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, n = 3, plotting=F))

# Using 10,000 instead of 20,000 pixels is slightly faster, but not by much
system.time(colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, n = 3, plotting=F, sampleSize=10000))

# Using all pixels instead of sample takes 5x longer - and this is a very low-res image!
system.time(colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, n = 3, plotting=F, sampleSize=FALSE))

## ------------------------------------------------------------------------
kmeansDF <- colordistance::extractClusters(kmeans01)
print(kmeansDF)

## ------------------------------------------------------------------------
# In order to see the clusters for each image, set plotting to TRUE and optionally pausing to TRUE as well
kmeans02 <- colordistance::getKMeansList("Heliconius/", bins=3, lower=lower, upper=upper, plotting=F)
kmeansClusters <- colordistance::extractClusters(kmeans02, ordering=T)

head(kmeansClusters, 3)

## ---- fig.width=4, fig.height=5------------------------------------------
colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper, returnClust = F)

## ---- fig.width=6, fig.height=3------------------------------------------
par(mfrow=c(1, 3))
# Get and plot histogram for a single image
hist01 <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", bins=2, plotting=T, title="RGB, 2 bins per channel", lower=lower, upper=upper)

# Use the bin center as the cluster value instead of the average pixel location (note the difference between this and when binAvg=F)
hist02 <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", bins=2, plotting=T, title="binAvg = F", lower=lower, upper=upper, binAvg=F)

# Use different number of bins for each channel; use HSV instead of RGB
hist03 <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", bins=c(8, 1, 2), plotting=T, hsv=T, title="HSV, 8 hue, 1 sat, 2 val", lower=lower, upper=upper)

# Get histograms for a set of images
histMulti <- colordistance::getHistList("Heliconius/", bins=2, plotting=F, lower=lower, upper=upper)

## ---- fig.width=4, fig.height=5------------------------------------------
lower <- rep(0.8, 3)
upper <- rep(1, 3)

# Use defaults
kmeans01 <- colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", n=3, plotting=F, lower=lower, upper=upper)
kmeansDF <- colordistance::extractClusters(kmeans01)

# Use a larger sample size
kmeans02 <- colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", n=3, plotting=F, sampleSize = 30000, lower=lower, upper=upper)
kmeansDF2 <- colordistance::extractClusters(kmeans02)

# Don't return clusters as a dataframe
colordistance::getKMeanColors("Heliconius/Heliconius_08.jpeg", n=15, plotting=F, returnClust=F, lower=lower, upper=upper)

# For whole dataset
kmeans03 <- colordistance::getKMeansList("Heliconius/", n=3, plotting=F, lower=lower, upper=upper)
kmeansList <- colordistance::extractClusters(kmeans03)

## ------------------------------------------------------------------------
# If we use the same number of clusters for both the histogram and k-means methods, how different do the clusters look?
histExample <- colordistance::getHistList("Heliconius/", lower=lower, upper=upper)

kmeansExample <- colordistance::extractClusters(colordistance::getKMeansList("Heliconius", bins=27, lower=lower, upper=upper))

colordistance::plotClustersMulti(histExample, title="Histogram method")

colordistance::plotClustersMulti(kmeansExample, title="K-means method")

## ---- fig.width=5, fig.height=5, echo=F----------------------------------
scene <- list(xaxis=list(title="Red", linecolor=plotly::toRGB("red"), linewidth=6, range=c(0, 1)), yaxis=list(title="Green", linecolor=plotly::toRGB("green"), linewidth=6, range=c(0, 1)), zaxis=list(title="Blue", linecolor=plotly::toRGB("blue"), linewidth=6, range=c(0, 1)), camera = list(eye = list(x = 0.9, y = -1.5, z = 0.8)))

clusterList <- histExample
p <- names(clusterList)
newList <- vector("list", length(p))
for (i in 1:length(p)) {
    newList[[i]] <- clusterList[[i]]
    names(newList)[i] <- p[i]
}
newDF <- newList[[1]]
  newDF$Image <- rep(names(newList)[1], dim(newList[[1]])[1])

  # Add an "image" ID column for plotting
  for (i in 2:length(newList)) {
    tempDF <- newList[[i]]
    tempDF$Image <- rep(names(newList)[i], dim(newList[[i]])[1])
    newDF <- rbind(newDF, tempDF)
  }
colExp <- apply(newDF[, 1:3], 1, function(x) rgb(x[1], x[2], x[3]))
  plotly::plot_ly(newDF, x = ~newDF[, 1], y = ~newDF[, 2], z = ~newDF[, 3], size=~Pct, text = ~paste("Image: ", Image)) %>%
    plotly::add_markers(color=I(colExp), size=~Pct, sizes=c(10, 5000)) %>%
    plotly::layout(scene = scene, title="Histogram method")

clusterList <- kmeansExample
p <- names(clusterList)
newList <- vector("list", length(p))
for (i in 1:length(p)) {
    newList[[i]] <- clusterList[[i]]
    names(newList)[i] <- p[i]
}
newDF <- newList[[1]]
  newDF$Image <- rep(names(newList)[1], dim(newList[[1]])[1])

  # Add an "image" ID column for plotting
  for (i in 2:length(newList)) {
    tempDF <- newList[[i]]
    tempDF$Image <- rep(names(newList)[i], dim(newList[[i]])[1])
    newDF <- rbind(newDF, tempDF)
  }
colExp <- apply(newDF[, 1:3], 1, function(x) rgb(x[1], x[2], x[3]))
  plotly::plot_ly(newDF, x = ~newDF[, 1], y = ~newDF[, 2], z = ~newDF[, 3], size=~Pct, text = ~paste("Image: ", Image)) %>%
    plotly::add_markers(color=I(colExp), size=~Pct, sizes=c(10, 5000)) %>%
    plotly::layout(scene = scene, title="K-means method")



