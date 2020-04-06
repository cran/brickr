## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
collapse = TRUE,
comment = "#>"
)
rgl::setupKnitr()

## ----setup, include = FALSE---------------------------------------------------
library(brickr)

## ----piece_table, results='asis', echo=FALSE, warning=FALSE, message=FALSE----

tibble::tribble(
  ~Piece, ~piece_type, ~Area, ~Height, ~Orientation,
  "Brick (classic)", "'b'", "any", "1", "--",
  "Plate (flat brick)", "'p'", "any", "1/3", "--",
  "Round 1x1 (cylinder)" , "'c1'", "1", "1", "--",
  "Round 1x1 (cone)", "'c2'", "1", "1", "--",
  "Cheese slope", "'w1'", "1", "2/3", "north",
  "Cheese slope", "'w2'", "1", "2/3", "east",
  "Cheese slope", "'w3'", "1", "2/3", "south",
  "Cheese slope", "'w4'", "1", "2/3", "west"
) %>% 
  knitr::kable()


## ----bricks_2a, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----

expand.grid(
  x = 1:4 * 2,
  y = 1:2 * 2,
  z = 1)  %>% 
  dplyr::mutate(Color = head(lego_colors$Color, 8),
                piece_type = c("b", "p", "c1", "c2", "w1", "w2", "w3", "w4")) %>% 
  bricks_from_coords() %>% 
  build_bricks(rgl_lit = FALSE, outline_bricks = TRUE)

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi, 0, 0 ,1))

## ----bricks_1, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----

expand.grid(
  x = 1:4,
  y = 1:2,
  z = 1,
  Color = "Bright red", stringsAsFactors = FALSE
)  %>% 
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_2, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----

expand.grid(
  x = 1:4,
  y = 1:2,
  z = 1,
  Color = "Bright red", stringsAsFactors = FALSE
)  %>% 
  dplyr::mutate(piece_type = "p") %>% 
  bricks_from_coords() %>% 
  build_bricks()

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

## ----bricks_6, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----
tree_or_mushroom_pieces <- tree_or_mushroom %>% 
  dplyr::mutate_at(dplyr::vars(dplyr::starts_with("X")), 
                   ~ifelse(.==3, "c2", "b")) #If the color is the dark green, make a cone, else brick

tree_or_mushroom %>% 
  bricks_from_table(brick_colors,
                    piece_matrix = tree_or_mushroom_pieces) %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_10, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----

expand.grid(
  x = 1:3,
  y = 1:3,
  z = 1:3,
  color = "Bright blue"
)  %>% 
  dplyr::mutate(color = ifelse(z==2, "Bright red", as.character(color))) %>%
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_11, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----

expand.grid(
  x = 1:3,
  y = 1:3,
  z = 1:3,
  color = "Bright blue"
)  %>% 
  dplyr::mutate(color = ifelse(z==2, "Bright red", as.character(color)),
                piece_type = "p") %>%
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_12, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----

expand.grid(
  x = 1:3,
  y = 1:3,
  z = 1:3,
  color = "Bright blue"
)  %>% 
  dplyr::mutate(color = ifelse(z==2, "Bright red", as.character(color)),
                piece_type = "p") %>%
  dplyr::mutate(mid_level = (z-1) %% 3,
                z = (z-1) %/% 3 +1) %>% 
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

## ----bricks_13, rgl=TRUE, dev='png', echo=TRUE, warning=FALSE, message=FALSE, fig.width=4, fig.height=4----

expand.grid(
  x = 1:3,
  y = 1:3,
  z = 1:3,
  color = "Bright blue"
)  %>% 
  dplyr::mutate(color = ifelse(z==2, "Bright red", as.character(color)),
                piece_type = ifelse(z==2, "p", "b")) %>%
  dplyr::mutate(
    mid_level = dplyr::case_when(
      z %in% 1:2 ~ 0, 
      z == 3 ~ 1
    ), 
    z = dplyr::case_when(
      z %in% 2:3 ~ 2,
      TRUE ~ 1
    )) %>% 
  bricks_from_coords() %>% 
  build_bricks()

rgl::par3d(userMatrix = rgl::rotate3d(rgl::par3d("userMatrix"), 1.1*pi/4, 0, 0 ,1))

