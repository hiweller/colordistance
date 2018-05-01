# colordistance 1.0.0

An R package with functions for quantifying the differences between colorful objects. Loads and displays images, selectively masks specified background
  colors, bins pixels by color using either data-dependent or automatically
  generated color bins, quantitatively measures color similarity among images
  using one of several distance metrics for comparing pixel color clusters, and
  clusters images by object color similarity. Originally written for use with
  organism coloration (reef fish color diversity, butterfly mimicry, etc), but
  easily applicable for any image set.

April 19, 2018: Functions for combining data across a set of images (`combineClusters` and `combineList`) added. Useful for pooling multiple images of the same individual, species, etc before analysis.

**Input**: Set(s) of JPEG or PNG images of colorful objects, optionally with backgrounds masked out.

**Output**: Color clusters, visualizations for color binning and image similarity, and distance matrices quantifying color similarity between images.

**Requirements**: R >= 3.3.2

**Documentation**: <https://hiweller.github.io/colordistance>

**Author**: [Hannah Weller](https://scholar.google.com/citations?user=rjI5wpEAAAAJ&hl=en)

**Contact**: hannahiweller@gmail.com

## Installation

The development version of `colordistance` can be found at <https://github.com/hiweller/colordistance>.

To install the development version of `colordistance` in R:

1. Install the [`devtools`](https://github.com/hadley/devtools) package (`install.packages("devtools")`).

2. Install `colordistance` *without* vignettes (long-form documentation) to save time and space or *with* vignettes for offline access to help documents.

    ```R
    # Without vignettes
    devtools::install_github("hiweller/colordistance")

    # With vignettes
    devtools::install_github("hiweller/colordistance", build_vignettes=TRUE)
    ```
 3. You can access help documents by running `help(package="colordistance")` and clicking on the html files or, if you set `build_vignettes=TRUE` during install, run `vignette("colordistance-introduction")`.

To install the stable release version on CRAN (https://CRAN.R-project.org/package=colordistance), just run `install.packages("colordistance")`.

## Documentation

All of the `colordistance` vignettes that (optionally) come with the package are also available online at <https://hiweller.github.io/colordistance/>. I recommend reading at least the introduction before getting started.

* [Introduction to `colordistance`](https://hiweller.github.io/colordistance/colordistance-introduction.html)

* [Explanation of pixel binning methods](https://hiweller.github.io/colordistance/binning-methods.html)

* [Explanation of color distance metrics](https://hiweller.github.io/colordistance/color-metrics.html)

## Quickstart

To get started with `colordistance`, you'll need:

1. A set of images of objects you want to compare, ideally as consistent with each other as possible in terms of lighting and angle, and with anything you want to ignore [masked out with a uniform background color](https://graphicdesign.stackexchange.com/questions/5446/making-the-background-of-an-image-transparent-in-gimp). Need something to get started? Try [these butterflyfish photos](https://github.com/hiweller/butterflyfish)! `colordistance` also comes with an example set of *Heliconius* butterfly pictures from [Meyer, 2006](http://journals.plos.org/plosbiology/article?id=10.1371/journal.pbio.0040341), which you can access via `system.file("extdata", "Heliconius", package="colordistance")` in R.

2. R version 3.3.2 or later.

3. Estimates for the upper and lower RGB bounds for your background color. R reads in pixels channels with a 0-1 intensity range instead of the typical 0-255 (so pure red would be [1, 0, 0], green would be [0, 1, 0], blue would be [0, 0, 1], and so on). Background masking is rarely perfect, so you'll need to specify an upper and lower threshold for the background cutoff - around 0.2 usually does it. So if your background is white, your lower threshold would be [0.8, 0.8, 0.8] and your upper would be [1, 1, 1]. The default background color for `colordistance` is bright green, [0, 1, 0].

To run an analysis with all the default settings (bright green background masking, RGB color histograms with 3 bins per channel, and earth mover's distance for color distance metric -- see documentation), just run:

```R
colordistance::imageClusterPipeline("path/to/images/folder")
```

You'll get a blue and yellow heatmap with a cluster dendrogram and labels taken from the image names. Yellow cells correspond to *dissimilar* images; blue cells correspond to more *similar* images. If those scores don't look right, try changing the number of bins (`bins` argument), the distance metric (`distanceMethod` argument), and making sure you're masking out the right background color.

## Questions?

Email me: hannahiweller@gmail.com
