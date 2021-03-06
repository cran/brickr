#' Generate required bricks as a data frame
#' 
#' Create a dataframe of brick colors and sizes used in a brick mosaic or model.
#'
#' @param brick_obj brickr mosaic or 3D model object.
#' @return Data frame of piece counts by LEGO color name and size. 
#' @family Resources
#' @export 
#' @examples 
#' # Import a jpeg or png
#'  demo_file <- system.file("extdata", "demo_img.jpg", 
#'                           package = "brickr", mustWork = TRUE)
#'  demo_image <- jpeg::readJPEG(demo_file)
#' #Create a mosaic oject 
#'  \donttest{
#'  mosaic <- demo_image %>% 
#'    image_to_mosaic(img_size = 24)
#'  }
#'  
#'  #Rather than drawing the mosaic, use build_pieces_table() to produce piece table
#'  \donttest{
#'   mosaic %>% 
#'    build_pieces_table()
#'  }


build_pieces_table <- function(brick_obj){
  pcs <- brick_obj$pieces
  
  pcs %>% 
    dplyr::select(-Lego_color) %>% 
    tidyr::spread(Brick_size, n, fill = 0) %>% 
    dplyr::rename(`LEGO Brick Color` = Lego_name)
}

#' Display bricks required to build model or mosaic
#' 
#' Create a chart of brick colors and sizes used in a brick mosaic or model.
#'
#' @param brick_obj brickr mosaic or 3D model object.
#' @return Plot object of required bricks by color and size. 
#' @family Resources
#' @export 
#' @examples
#' # Import a jpeg or png
#'  demo_file <- system.file("extdata", "demo_img.jpg", 
#'                           package = "brickr", mustWork = TRUE)
#'  demo_image <- jpeg::readJPEG(demo_file)
#' #Create a mosaic oject 
#'  \donttest{
#'  mosaic <- demo_image %>% 
#'    image_to_mosaic(img_size = 24)
#'  }
#'  
#'  #Rather than drawing the mosaic, use build_pieces_table() to draw piece chart
#'  \donttest{
#'   mosaic %>% 
#'    build_pieces()
#'  }

build_pieces <- function(brick_obj){
  in_list <- brick_obj
  pcs <- in_list$pieces
  
  pcs_coords <- dplyr::tibble(
    Brick_size = c("1 x 1", "2 x 1", "3 x 1", "4 x 1", "2 x 2", "4 x 2"),
    xmin = c(0, 0, 0, 0, 6, 6),
    xmax = c(1, 2, 3, 4, 8, 8),
    ymin = c(0, 2, 4, 6, 0, 3),
    ymax = c(1, 3, 5, 7, 2, 7)
  ) 

  #This function creates nodes in each brick for stud placement
  pcs_coords <- pcs_coords %>% 
    dplyr::mutate(studs = purrr::pmap(list(xmin, xmax, ymin, ymax), function(a, b, c, d){
      expand.grid(x=seq(a+0.5, b-0.5, by=1), 
                  y=seq(c+0.5, d-0.5, by=1))
    }))
  
  pcs2 <- pcs %>% 
    dplyr::arrange(Lego_color) %>% 
    dplyr::mutate(Lego_name = factor(Lego_name, 
                                     levels = c("Black", 
                                                unique(Lego_name)[!(unique(Lego_name) %in% c("Black", "White"))],
                                                "White"))) %>% 
    dplyr::left_join(pcs_coords, by = "Brick_size")
  
  coord_xlim <- c(-0.5, 10)
  facet_cols <- 5
  
  pcs2 %>% 
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin=xmin, xmax=xmax, ymin=-ymin, ymax=-ymax,
                                    fill = Lego_color), color = "#333333")+
    ggplot2::scale_fill_identity() +
    ggplot2::geom_point(data = pcs2 %>% tidyr::unnest(studs),
                        ggplot2::aes(x=x, y=-y), 
                        color = "#cccccc", alpha = 0.25, 
                        shape = 1, size = 2) +
    ggplot2::geom_text(
      ggplot2::aes(x = xmax + 0.25, y = -(ymin+ymax)/2, label = paste0("x", n)), 
      hjust = 0, vjust = 0.5, size = 3.5) +
    ggplot2::coord_fixed(xlim = coord_xlim) +
    ggplot2::labs(title = "Suggested LEGO Bricks") +
    ggplot2::facet_wrap(~Lego_name, ncol=facet_cols) +
    ggplot2::theme_minimal() +
    ggplot2::theme( panel.background = ggplot2::element_rect(fill = "#7EC0EE"),
                    strip.background = ggplot2::element_rect(fill = "#F7F18D"),
                    strip.text = ggplot2::element_text(color = "#333333", face = "bold"),
                    axis.line = ggplot2::element_blank(),
                    axis.title.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_blank(),
                    axis.title.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    panel.grid = ggplot2::element_blank())
}