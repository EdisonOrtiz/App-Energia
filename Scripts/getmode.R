getmode <- function(v) {
  dens <- density(v)
  max(dens$x[dens$y==max(dens$y)])
}