# Labraries ----

library(sf) # Stats on spatial data
library(ggplot2)
library(stars) # Rastorize gpkg data
library(rayshader)
library(MetBrewer) # Color palettes
library(colorspace) # working with colors
library(magick) # Images edition
library(glue)

# Data preparation ----

# Kontur data source for Kazakhstan: https://data.humdata.org/dataset/kontur-population-kazakhstan
data <- st_read("kontur_population_KZ_20220630.gpkg")

# define aspect ratio (square borders)
bb <- st_bbox(data)

bottom_left <- 
  st_point(c(bb[['xmin']], bb[['ymin']])) %>% 
  st_sfc(crs = st_crs(data))

bottom_right <- 
  st_point(c(bb[['xmax']], bb[['ymin']])) %>% 
  st_sfc(crs = st_crs(data))

top_left <- 
  st_point(c(bb[['xmin']], bb[['ymax']])) %>% 
  st_sfc(crs = st_crs(data))

width <- st_distance(bottom_left, bottom_right)
height <- st_distance(bottom_left, top_left)

# Handle conditions of W or H being the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- height/width
} else {
  h_ratio <- 1
  w_ratio <- width/height
}

# rasterize so we can convert to matrix
size <- 5000 # arbitrary
size_x <- floor(w_ratio * size)
size_y <- floor(h_ratio * size)

rast <- st_rasterize(data, nx = size_x, ny = size_y)
mat <- matrix(rast$population, nrow = size_x, ncol = size_y)

# Plots -----

# create color palette
c1 <- met.brewer('Tam')
swatchplot(c1)

## 3d chart ----

# bias (optional) which color should be dominant
# the higher the number the more right color would be present
texture <- (grDevices::colorRampPalette(c1, bias = 2))(256) 
swatchplot(texture)

# close prev chart
rgl::rgl.close()

mat %>% 
  height_shade(texture = texture) %>% 
  plot_3d(
    heightmap = mat,
    zscale = 100 / (size / 1000), # the more the less exaggerated
    solid = FALSE, # no base dots
    shadowdepth = 0, # shadow control
  )

render_camera(
  # angle, 0 is default view from the datasource
  theta = -10, 
  # azimut, 90 is 2d object
  phi = 45,
  zoom= 0.8
)

outfile <- 'kazakhstan_plot.png'

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
  
  render_highquality(
    filename = outfile,
    interactive = FALSE,
    lightdirection = 310,
    lightaltitude = c(20, 80),
    lightcolor = c(c1[3], 'white'),
    lightintensity = c(400, 200),
    samples = 450,
    width = 6000,
    height = 6000
  )
  
  end_time <- Sys.time()
  diff <- end_time - start_time
  cat(crayon::cyan(diff), "\n")
}


# add title and annotations
# This is a part of the tutorial, but eventually I just made a title using MS Paint as it was less time consuming :)
img <- image_read(outfile)

img %>% 
  image_crop(
    gravity = 'center',
    geometry = '6000x3500'
  ) %>% 
  image_annotate(
    text = 'Kazakhstan Population Density',
    gravity = 'northwest',
    location = '+200+200',
    size = 180,
    color = c1[8]
  ) %>% 
  image_write('titled_plot.png')


## 2D chart ----
data |>
  ggplot() +
  geom_sf(col=c1[8]) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_blank(),
    plot.background = element_rect(fill = '#F8F4EA', color = NA),
  )

output_2dplot <- 'kazakhstan_plot_2d'
ggsave(
  glue('{output_2dplot}.pdf'), 
  width = 8, 
  height = 6, 
  device = cairo_pdf
)

pdftools::pdf_convert(
  glue('{output_2dplot}.pdf'), 
  filenames = glue('{output_2dplot}.png'),
  format = 'png', 
  dpi = 450
)
