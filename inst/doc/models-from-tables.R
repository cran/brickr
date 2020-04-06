## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
rgl::setupKnitr()

## ----setup, include = FALSE---------------------------------------------------
library(brickr)

## ----bricks_1, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=3, fig.height=3----
#This is a 2 (columns) x 4 (rows) brick
(brick <- data.frame(
  Level="A",
  X1 = rep(3,4), #The number 3 is the brickrID for 'bright red'
  X2 = rep(3,4)
))

brick %>% 
  bricks_from_table() %>% 
  build_bricks() 

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_2, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=3, fig.height=3----
1:10 %>% 
  purrr::map_df(~dplyr::mutate(brick,
                        Level = LETTERS[.x], 
                        X1 = .x,
                        X2 = .x)) %>% 
  bricks_from_table() %>% 
  build_bricks(rgl_lit=FALSE, outline_bricks = TRUE)

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_5, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
tree_or_mushroom <- tibble::tribble(
  ~Level, ~X1, ~X2, ~X3, ~X4, ~X5, ~X6,
  "A", 1, 1, 1, 1, 1, 1, 
  "A", 1, 1, 1, 1, 1, 1, 
  "A", 1, 1, 1, 1, 1, 1, 
  "A", 1, 1, 1, 1, 1, 1, 
  "B", 0, 0, 0, 0, 0, 0, 
  "B", 0, 0, 2, 2, 0, 0,
  "B", 0, 0, 2, 2, 0, 0, 
  "B", 0, 0, 0, 0, 0, 0, 
  "C", 0, 0, 0, 0, 0, 0, 
  "C", 0, 0, 2, 2, 0, 0,
  "C", 0, 0, 2, 2, 0, 0, 
  "C", 0, 0, 0, 0, 0, 0, 
  "D", 0, 3, 3, 3, 3, 0, 
  "D", 0, 3, 3, 3, 3, 0,
  "D", 0, 3, 3, 3, 3, 0, 
  "D", 0, 3, 3, 3, 3, 0,
  "E", 0, 0, 3, 3, 0, 0, 
  "E", 0, 3, 3, 3, 3, 0,
  "E", 0, 3, 3, 3, 3, 0, 
  "E", 0, 0, 3, 3, 0, 0,
  "F", 0, 0, 0, 0, 0, 0, 
  "F", 0, 0, 3, 3, 0, 0,
  "F", 0, 0, 3, 3, 0, 0, 
  "F", 0, 0, 0, 0, 0, 0,
  "G", 0, 0, 0, 0, 0, 0, 
  "G", 0, 0, 3, 0, 0, 0,
  "G", 0, 0, 0, 3, 0, 0, 
  "G", 0, 0, 0, 0, 0, 0
)

brick_colors <- tibble::tribble(
  ~`.value`, ~Color,
  1, "Bright green",
  2, "Dark orange",
  3, "Dark green"
)
  
tree_or_mushroom %>% 
  bricks_from_table(brick_colors) %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

