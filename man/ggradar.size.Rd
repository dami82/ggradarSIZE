\name{ggradar.size}
\alias{ggradar.size}
\title{Draw Color Radar Charts}
\description{
Draw Colored Radar Charts using numeric data frames. This package was derived from the ggradar package, written by Ricardo Bion. Some tiny modifications were included to the original package in order to color the radar lines according to a weigth (or size) feature (required) that is supposed to be a numeric continuous variable.
}
\usage{
ggradar.size(plot.data, group_col = 1, weight_col = ncol(plot.data), grid.mid = 0.18, grid.max = 0.36, grid.min = 0, values.radar = c("0\%", "18\%", "36\%"), legend.pos = "bottom", axis.label.size = 5, group.point.size = 2.5, group.line.width = 2, axis.label.offset = 1.15, grid.label.size = 4.5, font.radar = "ArialMT", gridline.min.colour = "grey", gridline.mid.colour = "#007A87", gridline.max.colour = "grey", background.circle.colour = "#D7D6D1", axis.line.colour = "grey40", linepoint.alpha = 0.56, background.circle.transparency = 0.2, plot.extent.x.sf = 1, plot.extent.y.sf = 1.2, gridline.min.linetype = "longdash", gridline.mid.linetype = "longdash", gridline.max.linetype = "longdash", grid.line.width = 0.5)
}
\arguments{
  \item{plot.data}{
numeric data frame
}
  \item{group_col}{
number pointing to the index of the column corresponding to the grouping variable
}
  \item{weight_col}{
number pointing to the index of the column corresponding to the weight/size variable
}
  \item{grid.mid}{
size of the grid, mid point
}
  \item{grid.max}{
size of the grid, max point
}
  \item{grid.min}{
size of the grid, min point
}
  \item{values.radar}{
labels for the min, mid and max positions of the chart
}
  \item{legend.pos}{
legend position, c("bottom", "top", "left", "right")
}
  \item{axis.label.size}{
axis label size
}
  \item{group.point.size}{
chart point size
}
  \item{group.line.width}{
chart line size
}
  \item{axis.label.offset}{
axis label offset
}
  \item{grid.label.size}{
grid label size
}
  \item{font.radar}{
font used for the chart
}
  \item{gridline.min.colour}{
color for the min chart axis
}
  \item{gridline.mid.colour}{
color for the mid chart axis
}
  \item{gridline.max.colour}{
color for the max chart axis
}
  \item{background.circle.colour}{
color for the background circle
}
  \item{axis.line.colour}{
color for the radar chart axes
}
  \item{linepoint.alpha}{
radar line alpha
}
  \item{background.circle.transparency}{
background transparency
}
  \item{plot.extent.x.sf}{
plot extent x ax
}
  \item{plot.extent.y.sf}{
plot extent y ax
}
  \item{gridline.min.linetype}{
line style for the chart circle lines - min
}
  \item{gridline.mid.linetype}{
line style for the chart circle lines - mid
}
  \item{gridline.max.linetype}{
line style for the chart circle lines - max
}
  \item{grid.line.width}{
grid line width
}
}
\references{
http://www.biotechworld.it/bioinf/2016/11/10/radar-charts-with-ggradar-and-ggradarsize/
}
\author{
Damiano Fantini, modifying Ricardo Bion's code
}
\note{
check out the ggradar package
}
\examples{
df <- data.frame(rbind(c(0,0.13,0.22,0.52,0.12,0.32,100), 
                       c(1,0.33,0.12,0.32,0.32,0.32,2000),
                       c(2,0.53,0.02,0.12,0.42,0.32,5000)))
colnames(df) <- c("group", "f.1", "f.2", "f.3", "f.4", "f.5", "weight")
ggradar.size(plot.data = df, group_col = 1, weight_col = ncol(df), grid.max = 0.6, grid.mid = 0.3)
}
