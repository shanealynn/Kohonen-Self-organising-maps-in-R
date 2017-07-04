# Palette defined by kohonen package
#
# Shane Lynn 14-01-2014

coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}