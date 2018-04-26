path <- system.file("extdata", "Heliconius/Heliconius_B/Heliconius_07.jpeg", package="colordistance")

loadImage2 <- function(path, lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24), colorSpace="Lab", ref.white="D65") {

  # Read in the file as either JPG or PNG (or, if neither, stop execution and return error message)
  if (!is.character(path)) {
    stop("Provided filepath is not a string (must be of character type)")
  }

  # Get absolute filepath in case relative one was provided
  path <- normalizePath(path)

  # Get filetype so we know how to read it in; make lowercase so checking later is easier
  filetype <- tolower(tail(strsplit(path, split="[.]")[[1]], 1))

  if (filetype %in% "png") {
    img <- png::readPNG(path)
    if (dim(img)[3]==4) {
      img <- img[,,1:3] # remove alpha channel if present
    }
  } else if (filetype %in% c("jpg", "jpeg")) {
    img <- jpeg::readJPEG(path)
  } else {
    stop("Images must be either JPEG (.jpg or .jpeg) or PNG (.png) format")
  }

  # Once the file is read in, eliminate pixels that fall between lower and upper bounds (background)
  if (is.numeric(upper) & is.numeric(lower)) {
    idx <- which((lower[1]<=img[, , 1] & img[, , 1]<=upper[1]) & (lower[2]<=img[, , 2] & img[, , 2]<=upper[2]) & (lower[3]<=img[, , 3] & img[, , 3]<=upper[3]))
  } else {idx <- NULL}

  # Reshape image matrix into 2-dimensional pixel matrix (3 columns, each row =
  # 1 pixel with RGB values)
  pix <- img
  dim(pix) <- c(dim(img)[1]*dim(img)[2], 3)
  colnames(pix) <- c("r", "g", "b")
  if (length(idx)!=0) {
    pix <- pix[-idx, ] # remove background pixels
  }

  # Initialize empty list with at minimum the image path, original 3D RGB array,
  # filtered RGB pixels, and elements for specified additional color spaces
  endList <- vector("list", length=(3+length(colorSpace)))
  endList[1:3] <- list(path, img, pix)

  # Name elements in the list according to filtered.colorspace.2d pattern
  names(endList) <- c("path", "original.rgb", "filtered.rgb.2d", sapply(colorSpace, function(i) paste("filtered", i, "2d", sep=".")))

  for (i in 1:length(colorSpace)) {
    if (colorSpace[i] %in% names(colorspaces)) {
      convertedPixels <- convertColor(pix, from="sRGB", to = colorSpace[i],
                                      from.ref.white = ref.white)
    } else if (tolower(colorSpace[i])=="hsv") {
      convertedPixels <- t(rgb2hsv(t(pix), maxColorValue = 1))
    } else {
      warning(paste(i, "not a valid colorspace; skipping"))
    }
    endList[[3+i]] <- convertedPixels
  }

  # Return a list with the path to the image, the original RGB image (3d array),
  # the reshaped matrix with background pixels removed (for clustering
  # analysis), and the reshaped RGB matrix converted to any number of specified
  # color spaces

  return(endList)

}

test <- loadImage2(path, lower=rep(0.8, 3), upper=rep(1, 3),
                   colorSpace=c("Lab", "hsv"), ref.white="D55")

images <- unlist(sapply(dir(system.file("extdata", "Heliconius/", package="colordistance"),
                  full.names = TRUE), getImagePaths))

# TEST 1: get everything in RGB, then convert clusters to CIELab

# get clusters in RGB
rgb_histograms <- getHistList(images, bins=2, lower = rep(0.8, 3), upper = rep(1, 3))

# for every cluster, convert RGB triplet to Lab triplet, with choice of reference white
rgb_histograms$Heliconius_01
lab_histograms <- rgb_histograms
ref.white <- "E"
for (i in 1:length(rgb_histograms)) {
  lab_histograms[[i]][, 1:3] <- convertColor(rgb_histograms[[i]][, 1:3], from="sRGB", to="Lab", from.ref.white=ref.white)
  colnames(lab_histograms[[i]]) <- c("L", "a", "b", "Pct")
}

# get distance matrix for CIELab colors
getColorDistanceMatrix(lab_histograms, plotting=T)
# ey, it still works! the scores are of course not internally bounded anymore, which isn't so nice...more like a PCA score...hm

# TEST 2: convert pixels to CIELab before clustering, then cluster???
ref.white <- "E"
rgb_pixels <- vector("list", length(images))
names(rgb_pixels) <- sapply(images, basename)
lab_pixels <- rgb_pixels
for (i in 1:length(images)) {
  rgb_pixels[[i]] <- loadImage(images[i], lower=rep(0.8, 3), upper=rep(1, 3), hsv=FALSE)
}


# for visualization:
# plot pixels in RGB space


# convert to CIELab
lab_pixels[[1]] <- convertColor(rgb_pixels[[1]]$filtered.rgb.2d, from="sRGB", to="Lab", from.ref.white=ref.white)

# plot CIELab?

# find approximate upper/lower bounds for pixels in RGB space
# broos
rgb_reference_path <- "/Users/hannah/Dropbox/Westneat_Lab/colordistance/Examples/Benchmark/RGB16Million_BruceLindbloom.png"
rgb_reference <- loadImage(rgb_reference_path, lower=NULL, hsv=FALSE)
dim(rgb_reference$filtered.rgb.2d) # 256^3 rows
plotPixels(rgb_reference)
plotPixels(rgb_reference, rev=T)
rgb_2d <- rgb_reference$filtered.rgb.2d

require(rbenchmark)
pixel_count <- seq(10000, 1000000, 50000)
conversion_time <- NULL
for (i in pixel_count) {
  message(paste("Converting", i, "pixels from RGB to CIEL*a*b colorspace"))
  subset_rgb_reference <- rgb_2d[sample(nrow(rgb_2d), i), ]
  conversion_time <- c(conversion_time, benchmark(Lab_pixels <- convertColor(subset_rgb_reference, from="sRGB", to="Lab", from.ref.white="D65"), replications=3)$elapsed/3)
}
pixel_sizes <- seq(10000, 510000, 10000)

rgb_lab_conversion2 <- data.frame(conversion_time=conversion_time, pixel_count=pixel_count)
par(mfrow=c(1,1), mar=rep(4,4))
plot(x=rgb_lab_conversion$pixel_count,
     y=rgb_lab_conversion$conversion_time,
     xlab="Number of pixels converted",
     ylab="Seconds for conversion",
     pch=19)
points(x=rgb_lab_conversion2$pixel_count,
       y=rgb_lab_conversion2$conversion_time,
       pch=18, col="blue")
abline(lm(data=rgb_lab_conversion, conversion_time~pixel_count))
summary(lm(data=rgb_lab_conversion, conversion_time~pixel_count))

ref.whites <- c("A", "B", "C", "E", "D50", "D55", "D65")
lab_reference_whites <- vector("list", length(ref.whites))
names(lab_reference_whites) <- ref.whites

iter <- 10
n <- 50000

limit_list <- vector("list", length=100)

k <- 1
for (k in 1:100) {
    # i'm a control freak
    message(paste("Iteration:", k))

    # sample random subset of pixels
    subset_rgb_reference <- rgb_reference$filtered.rgb.2d[sample(nrow(rgb_reference$filtered.rgb.2d), n), ]

    # progress bar so i can see if i break the computer
    pb <- txtProgressBar(min = 0, max = 100, style=3)
    for (i in 1:length(ref.whites)) {
      # convert all pixels in subset to CIEL*a*b color space using each reference white
      lab_reference_whites[[i]] <- convertColor(subset_rgb_reference, from="sRGB", to="Lab", from.ref.white=ref.whites[i])
      setTxtProgressBar(pb, round(i/length(ref.whites)*100))
    }
    close(pb)

    # get minimum/maximum values for each channel
    minmax <- lapply(lab_reference_whites, function(i) apply(i, 2, range))

    # store everything
      limits <- matrix(unlist(lapply(minmax, function(i) i[1:2, 2:3])), ncol=4, byrow=TRUE, dimnames = list(ref.whites, c("min.a", "max.a", "min.b", "max.b")))

    limit_list[[k]] <- limits
    k <- k + 1
  }


limit_list[[2]]

w <- 1 # row
par(mfrow=c(1,1))

A_channel <- matrix(data=NA, dimnames = list(c(1:200), ref.whites), nrow=200, ncol=length(ref.whites))
B_channel <- A_channel

for (w in 1:length(ref.whites)) {
  A_channel[,w] <- c(unlist(lapply(limit_list, function(j) j[w, 1])),
                     unlist(lapply(limit_list, function(j) j[w, 2])))
  B_channel[,w] <- c(unlist(lapply(limit_list, function(j) j[w, 3])),
                     unlist(lapply(limit_list, function(j) j[w, 4])))
}

par(mfrow=c(2,1))
a_df <- reshape2::melt(A_channel)
p <- ggplot(a_df, aes(x=Var2, y=value, color=Var2))
p <- p + ggplot2::geom_jitter(width=0.1, height=1)
p <- p + labs(x="White reference", y="Axis range", color="", title="RGB to L*a*b ranges (a channel)"); p

a_df <- reshape2::melt(B_channel)
q <- ggplot(a_df, aes(x=Var2, y=value, color=Var2))
q <- q + ggplot2::geom_jitter(width=0.1, height=1)
q <- q + labs(x="White reference", y="Axis range", color="", title="RGB to L*a*b ranges (b channel)"); q

multiplot(p, q)

p

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
dim(temp.Lab.ranges)

unlist(limit_list)[1:28]

limit_list[[1]]

# round 1:
# a = -90.45622 (A) : 97.48846 (D65)
# b = -193.82861 (A) :

# binning: divide up each channel into an evenly spaced number of bins?

# then cluster (same code as before)

# then get distance matrix (use colordistance functions)
