## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
rgl::setupKnitr()

## ----setup, include=FALSE-----------------------------------------------------
library(brickr)

## ----g1-----------------------------------------------------------------------
demo_img = tempfile() 
download.file("http://ryantimpe.com/files/mf_unicorn.PNG", demo_img, mode="wb")

mosaic <- png::readPNG(demo_img)  %>% 
  image_to_mosaic()

mosaic %>% build_mosaic()

## ----g2-----------------------------------------------------------------------
png::readPNG(demo_img) %>% 
  image_to_mosaic(img_size = 32) %>% 
  build_mosaic()

## ----c_palettes, fig.width=6, fig.height=4------------------------------------
p1 <- png::readPNG(demo_img) %>% 
  image_to_mosaic(32, color_palette = c('universal', 'generic')) %>% 
  build_mosaic(title = "universal & generic")

p2 <- png::readPNG(demo_img) %>% 
  image_to_mosaic(32, color_palette = c('universal')) %>% 
  build_mosaic(title = "universal")

gridExtra::grid.arrange(p1, p2, layout_matrix = matrix(c(1,2), ncol=2))


## ----c_bw---------------------------------------------------------------------
png::readPNG(demo_img) %>% 
  image_to_mosaic(32, color_palette = 'bw', contrast = 1.1)%>% 
  build_mosaic()

## ----c_custom-----------------------------------------------------------------
#Remove blue and azure colors from lego_colors
lego_colors_wo_blue <- lego_colors %>% 
  dplyr::filter(!grepl("blue|azur", tolower(Color)))

png::readPNG(demo_img) %>% 
  image_to_mosaic(32, color_table = lego_colors_wo_blue)%>% 
  build_mosaic(title = "Mosaic without blue or azur")

## ----c_methods, fig.height=5, fig.width=5-------------------------------------
c("cie94", "cie2000", "euclidean", "cmc") %>% 
  purrr::map(~png::readPNG(demo_img) %>% 
  image_to_mosaic(24, method =.x) %>% 
  build_mosaic(title = .x )) -> mosaics_by_method

gridExtra::grid.arrange(grobs = mosaics_by_method, layout_matrix =rbind(c(1,2),c(3,4)))


## ----c_threed, rgl=TRUE, dev='png'--------------------------------------------
png::readPNG(demo_img) %>% 
  image_to_mosaic(32) %>% 
  bricks_from_mosaic(highest_el = "dark") %>% 
  build_bricks(outline_bricks = TRUE, rgl_lit = FALSE)

#From dput(round(rgl::par3d("userMatrix"),1)) after manual rotation
custom_rotation <- structure(c(0.9, 0.3, -0.3, 0, -0.3, 0.9, -0.3, 
                               0, 0.2, 0.4, 0.9, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))

rgl::par3d(userMatrix = rgl::rotate3d(custom_rotation, 0, 0, pi/4 ,1))

