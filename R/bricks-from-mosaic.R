#' Convert a 2D LEGO mosaic into a 'brickr' 3D object
#' 
#' Stacks LEGO plates to create a 3D version of the 2D brick mosaics. 
#' Height of bricks determined by brightness of color. 
#'
#' @param mosaic_list List output from image_to_bricks(). Contains an element \code{Img_lego}.
#' @param mosaic_height Number of layers in the 3D image.
#' @param highest_el Brick height is determined by brightness of color. Use \code{highest_el = 'dark'} for darkest bricks to have \code{mosaic_height}.
#' @return A list with elements \code{Img_lego} to pass to \code{\link{build_bricks}}.
#' @family 3D Models
#' @export 
#' @examples 
#' # Import a jpeg or png
#'  demo_file <- system.file("extdata", "demo_img.jpg", 
#'                           package = "brickr", mustWork = TRUE)
#'  demo_image <- jpeg::readJPEG(demo_file)
#'  
#'  #Begin with a 24x24 mosaic object
#'  \donttest{
#'  mosaic <- demo_image %>% 
#'     image_to_mosaic(24)
#'     }
#'  
#'  #Pass the mosaic object to bricks_from_mosaic() to convert to 3D specifications
#'  \donttest{
#'  mosaic %>% 
#'    bricks_from_mosaic() %>% 
#'    build_bricks()
#'    
#'    rgl::clear3d()
#'    }
#'    
#'  #In this image, the background is a light color.
#'  # Change the 'highest_el' to make dark colors highest
#'  # Change mosaic height to change the number of layers
#'  \donttest{
#'  mosaic %>% 
#'    bricks_from_mosaic(mosaic_height = 3, highest_el = "dark") %>% 
#'    build_bricks()
#'  
#'   rgl::clear3d()
#'   }
bricks_from_mosaic <- function(mosaic_list, mosaic_height = 6, highest_el = "light"){
  
  #Get previous data
  in_list <- mosaic_list
  
  BrickIDs <- in_list$ID_bricks
  img_lego <- in_list$Img_lego
  
  img_sorted_by_lum <- mosaic_list$Img_lego %>% 
    dplyr::left_join(brickr::lego_colors %>% dplyr::select(Lego_name = Color, lum), by = "Lego_name") %>% 
    dplyr::mutate(Level = as.numeric(as.factor(cut(lum, mosaic_height)))) %>% 
    dplyr::do(
      if(highest_el == "dark"){
        dplyr::mutate(., Level = max(Level) - Level + 1)
      } else {.}
    )
  
  #For each Level, create a full based mosaic Level
  img_all_levels <- 1:mosaic_height %>% 
    purrr::map_df(function(lvl){
      dat <- img_sorted_by_lum %>% 
        #Only get colors at or above the current level
        dplyr::filter(Level >= lvl) %>% 
        #Replace any higher levels with colors in this level
        dplyr::mutate(Lego_name = ifelse(Level > lvl, NA_character_, Lego_name),
                      Lego_color = ifelse(is.na(Lego_name), NA_character_, Lego_color),
                      Level = lvl)
      
      most_common_color <- dat %>% 
        dplyr::filter(!is.na(Lego_name)) %>% 
        dplyr::count(Lego_name, Lego_color, sort = TRUE)
      
      dat %>% 
        dplyr::mutate(Lego_name = ifelse(is.na(Lego_name), as.character(most_common_color[1, "Lego_name"]), Lego_name),
                      Lego_color = ifelse(is.na(Lego_color), as.character(most_common_color[1, "Lego_color"]), Lego_color)) %>% 
        dplyr::select(-color) %>% 
        dplyr::rename(color = Lego_name, z = Level) %>% 
        dplyr::mutate(mid_level = (z-1) %% 3,
                      z = (z-1) %/% 3 +1)
    })
  
  return(img_all_levels %>% bricks_from_coords() )
  
}

