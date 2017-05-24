## ---- echo=F, fig.align="center", fig.cap="Eight different species of *Heliconius* butterflies; from @Meyer2006.", out.width="500px"----
knitr::include_graphics("Heliconius_mimicry.jpg")

## ---- echo=F, fig.align="center", fig.cap="The pin is small, but since it's not a part of the butterfly, we should exclude it.", out.width="250px"----
knitr::include_graphics("Heliconius_08_edit.jpg")

## ---- fig.width=5, fig.height=4, fig.align="center", fig.cap="(Note that `plotPixels()` plots a randomly selected subset of the pixels by default in an image to make it easier to see.)"----
colordistance::plotPixels("Heliconius/Heliconius_08.jpeg", lower=NULL, upper=NULL)

## ------------------------------------------------------------------------
lower <- c(0.99, 0.99, 0.99)
upper <- c(1, 1, 1)
H8 <- colordistance::loadImage("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper)

## ------------------------------------------------------------------------
names(H8)

## ------------------------------------------------------------------------
dim(H8$original.rgb)
dim(H8$filtered.rgb.2d)

## ---- fig.width=5, fig.height=4, fig.align="center"----------------------
colordistance::plotPixels(H8)

## ---- fig.width=5, fig.height=4, fig.align="center"----------------------
lower <- rep(0.8, 3)
colordistance::plotPixels("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper)

## ------------------------------------------------------------------------
lower <- rep(0.8, 3)
upper <- rep(1, 3)
H8 <- colordistance::loadImage("Heliconius/Heliconius_08.jpeg", lower=lower, upper=upper)
dim(H8$filtered.rgb.2d)

## ---- fig.width=5, fig.height=4, fig.align="center", echo=F, fig.cap=""----
pix <- H8$filtered.rgb.2d[sample(nrow(H8$filtered.rgb.2d), 10000), ]
colExp <- apply(pix, 1, function(x) rgb(x[1], x[2], x[3]))
xlab <- "Red"; ylab <- "Green"; zlab <- "Blue"
s3d <- scatterplot3d::scatterplot3d(pix, pch=20, xlim=c(0,1), ylim=c(0,1), zlim=c(0,1), color=colExp, grid = F, xlab=xlab, ylab=ylab, zlab=zlab)
a <- 0.5
n <- 3
xy1 <- s3d$xyz.convert(rep(0, n), seq(0, 1, length.out = n), rep(a, n))
xy2 <- s3d$xyz.convert(rep(1, n), seq(0, 1, length.out = n), rep(a, n))
xy3 <- s3d$xyz.convert(seq(0, 1, length.out=n), rep(0, n), rep(a, n))
xy4 <- s3d$xyz.convert(seq(0, 1, length.out=n), rep(1, n), rep(a, n))
yz1 <- s3d$xyz.convert(rep(a, n), rep(0, n), seq(0, 1, length.out=n))
yz2 <- s3d$xyz.convert(rep(a, n), rep(1, n), seq(0, 1, length.out=n))
yz3 <- s3d$xyz.convert(rep(a, n), seq(0, 1, length.out=n), rep(0, n))
yz4 <- s3d$xyz.convert(rep(a, n), seq(0, 1, length.out=n), rep(1, n))
xz1 <- s3d$xyz.convert(rep(0, n), rep(a, n), seq(0, 1, length.out=n))
xz2 <- s3d$xyz.convert(rep(1, n), rep(a, n), seq(0, 1, length.out=n))
xz3 <- s3d$xyz.convert(seq(0, 1, length.out=n), rep(a, n), rep(0, n))
xz4 <- s3d$xyz.convert(seq(0, 1, length.out=n), rep(a, n), rep(1, n))
lty <- "solid"
segments(xy1$x, xy1$y, xy2$x, xy2$y, lty=lty)
segments(xy3$x, xy3$y, xy4$x, xy4$y, lty=lty)
segments(yz1$x, yz1$y, yz2$x, yz2$y, lty=lty)
segments(yz3$x, yz3$y, yz4$x, yz4$y, lty=lty)
segments(xz1$x, xz1$y, xz2$x, xz2$y, lty=lty)
segments(xz3$x, xz3$y, xz4$x, xz4$y, lty=lty)

## ---- fig.width=4, fig.height=3, fig.align="center"----------------------
# Using 2 bins per channel as in the above figure
H8hist <- colordistance::getImageHist("Heliconius/Heliconius_08.jpeg", bins=c(2, 2, 2), lower=lower, upper=upper)

## ---- fig.align="center", results=F, fig.width=8, fig.height=5-----------
histList <- colordistance::getHistList("Heliconius/", lower=lower, upper=upper, bins=rep(2, 3), plotting=F, pausing=F)

## ---- fig.align="center", fig.width=7, fig.height=4, echo=F, results=F----
par(mfrow=c(2,4))
histList <- suppressMessages(colordistance::getHistList("Heliconius/", lower=lower, upper=upper, bins=rep(2, 3), plotting=T, pausing=F))

## ------------------------------------------------------------------------
names(histList)
histList$Heliconius_01

## ------------------------------------------------------------------------
CDM <- colordistance::getColorDistanceMatrix(histList, method="emd", plotting=F)
print(CDM)

## ---- fig.align="center", fig.width=7, fig.height=5, fig.cap="Blue cells indicate **higher** similarity, while yellow cells indicate **lower** similarity."----
colordistance::heatmapColorDistance(CDM)

## ---- fig.align="center", out.width="300px", echo=F----------------------
knitr::include_graphics("Heliconius_divided.jpg")

## ------------------------------------------------------------------------
write.csv(CDM, file = "./Heliconius_color_distance_matrix.csv")

## ------------------------------------------------------------------------
colordistance::exportTree(CDM, file = "./Heliconius_color_tree.newick", returnTree = F)

## ---- fig.align="center", fig.width=8, fig.height=5----------------------
# Define upper and lower bounds for background pixels
upper <- rep(1, 3)
lower <- rep(0.8, 3)

# Get histograms for each image and plot the results
par(mfrow=c(2,4))
histList <- colordistance::getHistList("Heliconius/", lower=lower, upper=upper, bins=2)

# Inspect distance matrix using heatmap
par(mfrow=c(1,1))
CDM <- colordistance::getColorDistanceMatrix(histList, method="emd", plotting = F)

# Export distance matrix
write.csv(CDM, file = "./Heliconius_color_distance_matrix.csv")


## ---- fig.align="center", fig.width=7, fig.height=5, results=F-----------
# Default: histogram binning, EMD color distance metric, 3 bins per channel (27 total)
# Note that we get slightly different clustering each time
default <- colordistance::imageClusterPipeline("Heliconius/", upper=upper, lower=lower)

# Using k-means instead of histogram
kmeansBinning <- colordistance::imageClusterPipeline("Heliconius/", clusterMethod="kmeans", upper=upper, lower=lower)

# Using chisq instead of emd
chisq <- colordistance::imageClusterPipeline("Heliconius/", distanceMethod = "chisq", upper=upper, lower=lower)

# Using HSV instead of RGB
hsvPix <- colordistance::imageClusterPipeline("Heliconius/", hsv=T, upper=upper, lower=lower)

