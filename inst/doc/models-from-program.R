## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
rgl::setupKnitr()

## ----setup, include = FALSE---------------------------------------------------
library(brickr)

## ----bricks_6, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
use_colors <- c("Bright blue", "Bright yellow", "Bright red")

cube <- expand.grid(
  x = 1:8,
  y = 1:8,
  z = 1:8
) 

cube$Color <- sample(use_colors, nrow(cube), replace = TRUE, prob = c(5, 3, 1))

cube %>% 
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_7, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
radius <- 4
sphere_coords <- expand.grid(
  x = 1:round((radius*2.5)),
  y = 1:round((radius*2.5)),
  z = 1:round((radius/(6/5)*2.5)) #A brick is 6/5 taller than it is wide/deep
) %>%
  dplyr::mutate(
    #Distance of each coordinate from center
    dist = (((x-mean(x))^2 + (y-mean(y))^2 + (z-mean(z))^2)^(1/2)),
    Color = dplyr::case_when(
      #Yellow stripes on the surface with a 2to4 thickness
      dplyr::between(dist, (radius-1), radius) & (x+y+z) %% 6 %in% 0:1 ~ "Bright yellow",
      #Otherwise, sphere is blue
      dist <= radius ~ "Bright blue"
  ))

sphere_coords %>% 
  bricks_from_coords() %>% 
  build_bricks(rgl_lit = FALSE, outline_bricks = TRUE)

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_8, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
brick_house <- function(x_coord = 0, y_coord = 0, width=6, length=5, height=7){
  roof_colors <- c("Dark orange", "Dark brown", "Medium nougat", "Medium stone grey")
  roof_col <- sample(roof_colors, 1)
  
  house_colors <- c("Bright blue", "Bright red", "Dark red", "Dark azur", "Nougat", "Bright reddish violet")
  house_col <- sample(house_colors, 1)
  
  house_coords <- expand.grid(
    x = 1:width, y = 1:length, z = (1:height)+1
  ) %>% 
    dplyr::mutate(
      roof = (z > round((1/2)*height)),
      Color = dplyr::case_when(
        #Roof
        roof & (abs(y - floor(length/2) -1) <= (height-z)) ~ roof_col,
        roof ~ NA_character_,
        #Door and windows
        x == round(width/2) & y==1 & z <= 3 ~ NA_character_,
        dplyr::between(x, 2, width-1) & x %% 2 == 0  & y > 1 & z == 3 ~ NA_character_,
        dplyr::between(y, 2, length-1) & y %% 2 == 0 & z == 3 ~ NA_character_,
        x %in% c(1, width) | y %in% c(1, length) ~ house_col),
      x = x+x_coord, 
      y = y+y_coord
    )
  return(house_coords)
}

#Build one house
brick_house() %>% 
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_9, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
brick_street <- function(width = 100, length = 40){
  expand.grid(x=1:width, y=1:length, z=1) %>% 
    dplyr::mutate(
      Color = dplyr::case_when(
        y == round(length/2) & x %% 4 %in% 1:4 ~ "Bright yellow",
        dplyr::between(y, length/2 -5, length/2 +5) ~ "Dark stone grey",
        TRUE ~ "Dark green"
      ))
}

#Build a village, houses on 2 sides of a street
n_houses = 14
sz = c(100, 40)

list(x_coord = c(sample(seq(10, sz[1]-10, by = 12), n_houses/2),
                 sample(seq(10, sz[1]-10, by = 12), n_houses/2)),
     y_coord = c(rep(sz[2]/2-15, n_houses/2), rep(sz[2]/2+10, n_houses/2)),
     width = sample(4:10, n_houses, replace = TRUE),
     length = sample(5:8, n_houses, replace = TRUE),
     height = sample(7:9, n_houses, replace = TRUE)
     ) %>% 
  purrr::pmap_df(brick_house) %>% 
  dplyr::bind_rows(brick_street(sz[1], sz[2])) %>% 
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, pi/4, 0 ,1))

