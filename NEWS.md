# colordistance 1.1.1

* Transparency (alpha channel) can now be used to mask image backgrounds. By default, the presence of transparent pixels in a PNG overrides other background parameters, and the transparent pixels are ignored as background. This behavior can be disabled by setting `alpha.channel = FALSE` in any function that takes an image path as an argument. 
* Minor bug fixes with plotting 3D interactive plots.

# colordistance 1.1.0

Added `scatter3dclusters` function to plot clusters in color space, scaled according to their size and colored according to their color.

# colordistance 1.0.1
Minor fixes. Patched a bug in convertColorSpace.


# colordistance 1.0.0
Second release version. Changes include:

* Functions for using CIELAB color space option for analysis

* 'Color Spaces' and 'CIELab Analyses' vignettes

* Ability to average cluster palettes across photographs or by directory hierarchies ('combineClusters' and 'combineList')

* Minor fixes to plotting functions

# colordistance 0.8.0
First release version. All versions are functional, potentially pending image segmentation additions.
