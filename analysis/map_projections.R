library(geodata)
library(viridis)
library(sf)

w <- world(path = tempdir())

plot(w)
text(w, labels = "NAME_0", cex = 0.5)

wsf <- st_as_sf(w)

p <- worldclim_global("prec", 10, path = tempdir(), version = "2.1")

plot(
  p[[1]],
  fun = function() lines(w, col = "grey72", lwd = 0.2)
  )

wsf_robin <- st_transform(wsf, crs = "+proj=robin")

plot(wsf_robin)

p_robin <- project(p, "+proj=robin")

plot(p_robin[[1]])
