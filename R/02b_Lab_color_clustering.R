getLabHist <- function(image, bins = 3, sample.size = 100000, 
                       ref.white, from = "sRGB", bin.avg = TRUE, 
                       as.vec = FALSE, plotting = TRUE,
                       lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24), 
                       title = "path", a.bounds = c(-128, 127),
                       b.bounds = c(-128, 127), ...) {

  message("Accuracy of CIE Lab color space depends on specification of an appropriate white reference. See 'Color spaces' vignette for more information.")
  
  # If filepath was provided, check to make sure it exists or throw an error
  if (is.character(image)) {
    if (file.exists(image)) {
      image <- loadImage(image, lower=lower, upper=upper, hsv=FALSE, 
                         CIELab = TRUE, sample.size = sample.size, ref.white = ref.white)
    }
  } else if (!is.list(image)) {
    stop("'image' must be either a path to the image or a list object returned by loadImage")
  }
  
  # If Lab coordinates were not calculated, calculate them
  if ("filtered.lab.2d" %in% names(image)) {
    pix <- image$filtered.lab.2d
  } else {
    pix <- convertColorSpace(image$filtered.rgb.2d, sample.size = sample.size, 
                             to.ref.white = ref.white, from = from, to = "Lab")
  }
  
  # Define universal boundaries
  boundaries <- list(L = c(0, 100), a = a.bounds, b = b.bounds)
  
  # Create vector of bins
  if (length(bins)==1 | length(bins)==3) {
    if (length(bins)==1) {
      message(paste("Using ", bins, "^3 = ", paste(bins^3), " total bins", sep = ""))
      bins <- rep(bins, 3)
    } else {
      message(paste("Using ", bins[1], "*", bins[2], "*", bins[3], " = ", bins[1]*bins[2]*bins[3], " bins", sep = ""))
    }
  
  # Generate breakpoints/boundaries in which to bin the pixels
  breaks <- list(L = numeric(), a = numeric(), b = numeric())
  for (i in 1:length(breaks)) {
    breaks[[i]] <- seq(boundaries[[i]][1], boundaries[[i]][2], length = bins[i] + 1)
  }

  } else {
    
    stop("Bins must be a numeric vector of length 1 or length 3")
    
  }
  
  # Bin all the channels
  binnedImage <- data.frame(
    L = cut(pix[, 1], breaks = breaks$L, include.lowest = T, labels = F),
    a = cut(pix[, 2], breaks = breaks$a, include.lowest = T, labels = F),
    b = cut(pix[, 3], breaks = breaks$b, include.lowest = T, labels = F)
  )
  
  # Unless defaultClusters dataframe is provided, just make a dataframe where
  # each default cluster color is the center of the bin
  means <- lapply(breaks, function(i) sapply(c(1:(length(i) - 1)), function(j) mean(c(i[j], i[j + 1]))))
  defaultClusters <- expand.grid(means)
  colnames(defaultClusters) <- c("L", "a", "b")
  
  # Set clusters as defaultClusters - values only overwritten if there are
  # pixels in that bin, otherwise center is cluster value and Pct is 0
  clusters <- as.data.frame(defaultClusters)
  
  # If bin.avg is flagged and as.vec is off, return a matrix with average color
  # of all pixels assigned to each bin and the percentage of pixels in that bin
  # (if none, retain bin center as cluster color)
  # If bin.avg is flagged by as.vec is TRUE, don't waste time calculating the
  # average bin color, since it won't be returned anyways
  if (bin.avg & !as.vec) {
    
    # Get list of all filled bins + their sizes
    d <- mgcv::uniquecombs(binnedImage, T)
    ind <- attr(d, "index")
    
    clusters$Pct <- rep(0, dim(clusters)[1])
    
    # Possible bins for comparison with assigned indices
    possibleBins <- expand.grid(c(1:bins[1]), c(1:bins[2]), c(1:bins[3]))
    
    # For every identified cluster, get the average L, a, and b values
    # If only one pixel was assigned there (unlikely, but, you know), just plop
    # that pixel in as the average color
    # Otherwise leave the default cluster in place
    for (j in 1:dim(d)[1]) {
      
      pixTemp <- pix[which(ind == j), ]
      
      if (nrow(pixTemp) > 0) {
        
        clusters[which(apply(possibleBins, 1, 
                             function(x) all(x==d[j, ]))), 1:3] <- apply(pixTemp, 2, mean)
        
        clusters[which(apply(possibleBins, 1, 
                             function(x) all(x==d[j, ]))), 4] <- nrow(pixTemp) / dim(pix)[1]
        
      }
    }
    
  } else {
    
    # Count up pixels per bin without keeping track of which pixel was assigned to which bin
    Pct <- as.vector(xtabs(~ ., binnedImage))
    clusters$Pct <- Pct/max(sum(Pct))
    
  }
  
  # Plot if flagged
  if (plotting) {
    pixelBins <- clusters$Pct
    colExp <- apply(clusters, 1, 
                    function(i) grDevices::rgb(suppressMessages(convertColorSpace(i[1:3],
                                                            from = "Lab", to = from, sample.size = 1,
                                                            from.ref.white = ref.white))))
    
    
    if (title=="path") {
      title <- strsplit(tail(strsplit(image$path, "/")[[1]], 1), "[.]")[[1]][1]
    }
    barplot(as.vector(pixelBins), col=colExp, main=title, ...)
  }
  
  if (as.vec) {
    clusters <- clusters$Pct
  }
  
  return(clusters)
  
}

getLabHistList <- function(images, bins = 3, sample.size = 100000, 
                           ref.white, from = "sRGB", bin.avg = TRUE, 
                           as.vec = FALSE, plotting = FALSE, pausing = TRUE,
                           lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24),
                           title = "path", a.bounds = c(-128, 127),
                           b.bounds = c(-128, 127), ...) {
  
  message("Accuracy of CIE Lab color space depends on specification of an appropriate white reference. See 'Color spaces' vignette for more information.")
  
  # If argument isn't a string/vector of strings, throw an error
  if (!is.character(images)) {
    stop("'images' argument must be a string (folder containing the images), a vector of strings (paths to individual images), or a combination of both")
  }
  
  imPaths <- c()
  
  # Extract image paths from any folders
  if (length(which(dir.exists(images))) >= 1) {
    imPaths <- unlist(sapply(images[dir.exists(images)], colordistance::getImagePaths), use.names=F)
  }
  
  # For any paths that aren't folders, append to imPaths if they are existing image paths
  # ok this is confusing so to unpack: images[!dir.exists(images)] are all paths that are not directories; then from there we take only ones for which file.exists=TRUE, so we're taking any paths that are not folders but which do exist
  imPaths <- c(imPaths, images[!dir.exists(images)][file.exists(images[!dir.exists(images)])])
  
  # Grab only valid image types (jpegs and pngs)
  imPaths <- imPaths[grep(x=imPaths, pattern="[.][jpg.|jpeg.|png.]", ignore.case=T)]
  
  # Before we embark on this lengthy journey, make sure bins argument is valid
  # Convert bins to a vector of length 3 or throw an error if bins argument is not valid
  if (length(bins)==1 | length(bins)==3) {
    if (length(bins)==1) {
      message(paste("Using ", bins, "^3 = ", paste(bins^3), " total bins", sep=""))
      bins <- rep(bins, 3)
    } else {
      message(paste("Using ", bins[1], "*", bins[2], "*", bins[3], " = ", bins[1]*bins[2]*bins[3], " bins", sep = ""))
    }
  } else {
    stop("Bins must be a numeric vector of length 1 or length 3")
  }
  
  if (length(imPaths)==0) {
    stop("No images found")
  }
  # Empty list for histogram output
  endList <- vector("list", length(imPaths))
  
  # If pausing is on (and plotting is also on because otherwise this is pointless), fill in one element at a time, plot it, then wait for user input
  
  # Display progress bar
  pb <- txtProgressBar(min=0, max=length(imPaths), style=3)
  
  for (i in 1:length(endList)) {
    endList[[i]] <- suppressMessages(getLabHist(imPaths[i], bins = bins, bin.avg = bin.avg, 
                                                lower = lower, upper = upper,
                                                plotting = plotting, title = title, 
                                                ref.white = ref.white, from = from,
                                                a.bounds = a.bounds, b.bounds = b.bounds))
    
    setTxtProgressBar(pb, i)
    
    if (pausing & plotting & i < length(endList)) {
      pause()
    }
    
  }
  
  names(endList) <- basename(imPaths)
  
  return(endList)
  
}