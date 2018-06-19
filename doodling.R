## TO DO:
# Document new functions/changes to existing functions
# Check whether current distance matrix function works with Lab clusters
# check whether plotly function works with Lab space
# Edit pipeline to include lab options
# trundle through the exhaustive process of Travis CI



# Basic pipeline:
# Load images as RGB coordinates
# Optional: convert color spaces
# Bin/cluster colors in relevant color space
# Quantify cluster distances

all.rgb <- loadImage("Examples/Benchmark/RGB16Million_BruceLindbloom.png", lower = NULL, upper = NULL, CIELab = FALSE)

refWhites <- c("A", "B", "C", "E", "D50", "D55", "D65")

getLabHist <- function(image, ref.white, bins=3, lower=c(0, 0.55, 0), upper=c(0.24, 1, 0.24), title="path", bounds="default", sampleSize = 100000) {
  
  # If filepath was provided, check to make sure it exists or throw an error
  if (is.character(image)) {
    if (file.exists(image)) {
      image <- loadImage(image, lower = lower, upper = upper, 
                         hsv = FALSE, CIELab = TRUE, sample.size = sample.size, 
                         ref.white = ref.white)
    }
  } else if (!is.list(image)) {
    stop("'image' must be either a path to the image or a list object returned by loadImage")
  }
  
  
}


refranges <- vector("list", length(refWhites))
names(refranges) <- refWhites
for (i in 1:length(refWhites)) {
  ranges <- data.frame(L.lower = rep(NA, 10),
                       L.upper = rep(NA, 10),
                       a.lower = rep(NA, 10),
                       a.upper = rep(NA, 10),
                       b.lower = rep(NA, 10),
                       b.upper = rep(NA, 10))
  j <- 1
  while (j < 11) {
    lab <- suppressMessages(convertColorSpace(all.rgb$filtered.rgb.2d, to.ref.white = refWhites[i]))
    ranges[j,] <- as.vector(apply(lab, 2, range))
    j <- j + 1
  }
  refranges[[i]] <- ranges
  message(paste("Reference white: ", refWhites[i], "\n"))
  print(apply(lab, 2, range))
  message("\n")
}

refranges <- reshape2::melt(refranges)
g <- ggplot(data = refranges, aes(x = variable, y = value)) +
  geom_jitter(aes(color = L1)); g

# L: 0-100 (all)
# a: -103-79 (A), -93-98 (B-E)
# b: -194-49 (A), -127-97 (B-E)

plotPixels(all.rgb)

apply(all.rgb$filtered.lab.2d, 2, range)

lab <- all.rgb$filtered.lab.2d[sample(nrow(all.rgb$filtered.lab.2d), 100000), ]
colorVector <- rgb(convertColor(lab, from="Lab", to="sRGB", from.ref.white="D65"))




scatterplot3d::scatterplot3d(lab$L,
                             lab$a.x,
                             lab$b,
                            color=scales::alpha(colorVector, 0.1),
                            xlim=c(0, 100),
                            ylim=c(-100, 100),
                            zlim=c(-100, 100),
                            pch=19,
                            xlab="Luminance",
                            ylab="a (green-red)",
                            zlab="b (blue-yellow)")
dim(all.rgb$filtered.lab.2d)


# Needed:
# DONE: Color space conversion function (convertColorSpaces)
# Binning function modifications:
  # figure out boundaries/binning in XYZ, CIE Lab, Luv color spaces
  # figure out display of irregular CIE lab objects (scatterplot3d?)
  # binning of irregular color spaces & notifications/warnings:
    # if using RGB, warn of non-linearity/perception
    # if using Lab, warn about white references

# Distance measurements:
  # Require color space to be specified & assume all clusters are in same space
  # EMD, etc, should all work the same because clusters should share the same boundaries? test this

# Color space vignette:
  # Why would you use different color spaces
  # Pros/cons of using more complex ones (better results, but potential to get screwed up)
  # White references: sunglasses metaphor
  # Recommendations: use Lab if you need perceptually uniform spaces; RGB is fine for quantifying more general differences
