## Note: Rmd files load most packages explicitly.
## This header ensures only its direct dependencies are available.
if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
  stop("Package 'RColorBrewer' is required by header.R. Please install it.")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required by header.R. Please install it.")
}
if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required by header.R. Please install it.")
}

# PNG and PDF helpers -------------------------------------------------------

# Open a high-DPI PNG device using ragg
mypng <- function(file, width, height) {
  return(ragg::agg_png(file, width = width, height = height, res = 144)) 
}

# Open a PDF device (convert px-like width/height to inches)
mypdf <- function(file, width, height) {
  return(pdf(file, width = width/100, height = height/100))
}

# Save a plot to PNG with consistent sizing
save_png <- function(grf, filename, width, height) {
  mypng(filename, width = width, height = height) 
  plot(grf)
  dev.off() 
}

# Save a plot to PDF with consistent sizing
save_pdf <- function(grf, filename, width, height) {
  mypdf(filename, width = width, height = height)
  plot(grf)
  dev.off() 
}

# Default palette and font theme used across examples
colors <- RColorBrewer::brewer.pal(9, 'Set1')[c(1:5,7:9)]
font <- ggplot2::theme(text = ggplot2::element_text(size=16))

# Apply a consistent visual style and axis labels
prepare_grf <- function(grf, xlabel) {
  grf <- grf + ggplot2::theme_bw(base_size = 10)
  grf <- grf + ggplot2::theme(plot.title = ggplot2::element_blank())
  grf <- grf + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  grf <- grf + ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  grf <- grf + ggplot2::ylab(xlabel)
  grf <- grf + ggplot2::xlab("time")
  grf <- grf + font
  return(grf)
}
