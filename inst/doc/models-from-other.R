## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
rgl::setupKnitr()

## ----setup, include = FALSE---------------------------------------------------
library(brickr)

## ----bricks_6, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
demo_img = tempfile() 
download.file("http://ryantimpe.com/files/mf_unicorn.PNG", demo_img, mode="wb")

mosaic <- png::readPNG(demo_img) %>% 
  image_to_mosaic()

mosaic %>% build_mosaic()

## ----bricks_6a, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
mosaic %>% 
  bricks_from_mosaic(highest_el = "dark") %>% 
  build_bricks()

#From dput(round(rgl::par3d("userMatrix"),1)) after manual rotation
custom_rotation <- structure(c(0.9, 0.3, -0.3, 0, -0.3, 0.9, -0.3, 
                               0, 0.2, 0.4, 0.9, 0, 0, 0, 0, 1), .Dim = c(4L, 4L))

rgl::par3d(userMatrix = rgl::rotate3d(custom_rotation, 0, 0, pi/4 ,1))

