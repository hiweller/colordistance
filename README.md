# colordistance 0.0.0.9000

An R package with functions for quantifying the differences between colorful objects.

**Input**: Set(s) of JPEG or PNG images of colorful objects, optionally with backgrounds masked out.

**Output**: Color clusters, visualizations for color binning and image similarity, and distance matrices quantifying color similarity between images.

**Requirements**: R >= 3.3.2

**Documentation**: <https://hiweller.github.io/colordistance>

**Author**: [Hannah Weller](https://scholar.google.com/citations?user=rjI5wpEAAAAJ&hl=en)

**Contact**: hannahiweller@gmail.com

## Installation

`colordistance` is still in development, and you can track it at <https://github.com/hiweller/colordistance>. 

To install the current (largely untested) version of `colordistance` in R:

1. Install the [`devtools`](https://github.com/hadley/devtools) package (`install.packages("devtools")`).

2. Install `colordistance` *without* vignettes (long-form documentation) to save time and space or *with* vignettes for offline access to help documents.

    ```R
    # Without vignettes
    devtools::install_github("hiweller/colordistance")
  
    # With vignettes
    devtools::install_github("hiweller/colordistance", build_vignettes=TRUE)
    ```
<<<<<<< HEAD
 3. If you chose to install vignettes, you can access help documents by running `help(package="colordistance")` or `vignette("colordistance-introduction")`.
=======
 3. You can access help documents by running `help(package="colordistance")` and clicking on the html files or, if you set `build_vignettes=TRUE` during install, run `vignette("colordistance-introduction")`.
>>>>>>> 71b06064b1fac7d77335932db8ecc57927babf31

## Documentation

All of the `colordistance` vignettes that (optionally) come with the package are also available online at <https://hiweller.github.io/colordistance/>. I recommend reading at least the introduction before getting started.

* [Introduction to `colordistance`](https://hiweller.github.io/colordistance/colordistance-introduction.html)

* [Explanation of pixel binning methods](https://hiweller.github.io/colordistance/binning-methods.html)

* [Explanation of color distance metrics](https://hiweller.github.io/colordistance/color-metrics.html)

## Quickstart

To get started with `colordistance`, you'll need:

1. A set of images of objects you want to compare, ideally as consistent with each other as possible in terms of lighting and angle, and with anything you want to ignore [masked out with a uniform background color](https://graphicdesign.stackexchange.com/questions/5446/making-the-background-of-an-image-transparent-in-gimp).

2. R version 3.3.2 or later.

3. Estimates for the upper and lower RGB bounds for your background color. R reads in pixels channels with a 0-1 intensity range instead of the typical 0-255 (so pure red would be [1, 0, 0], green would be [0, 1, 0], blue would be [0, 0, 1], and so on). Background masking is rarely perfect, so you'll need to specify an upper and lower threshold for the background cutoff - around 0.2 usually does it. So if your background is white, your lower threshold would be [0.8, 0.8, 0.8] and your upper would be [1, 1, 1]. The default background color for `colordistance` is bright green, [0, 1, 0].

To run an analysis with all the default settings (bright green background masking, RGB color histograms with 3 bins per channel, and earth mover's distance for color distance metric -- see documentation), just run:

```R
colordistance::imageClusterPipeline("path/to/images/folder")
```

You'll get a blue and yellow heatmap with a cluster dendrogram and labels taken from the image names. Yellow cells correspond to *dissimilar* images; blue cells correspond to more *similar* images. If those scores don't look right, try changing the number of bins (`bins` argument), the distance metric (`distanceMethod` argument), and making sure you're masking out the right background color.

## Questions?

Email me!
