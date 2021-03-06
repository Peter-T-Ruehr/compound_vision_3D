#' Search Diameter Definition
#'
#' Define local plane search diameter interactively.
#'
#' @param df A tibble containing triangle center coordinates in columns `x, y, z`.
#'
#' @return A numerical search diameter.
#'
#' @export
#' @examples
#' # xxx: add example
#'
search_diam_interactive <- function(df){
  # dplyr NULLs
  x <- y <- z <- NULL
  
  # select two neighboring lens peaks
  run = TRUE
  while(run == TRUE){
    
    df <- df %>% 
      ungroup() %>% 
      select(x,y,z)
    
    # plot and save plot object for 3D selection
    print("Rotate view so that two facet peaks are clearly identifyable.")
    ids <- plot3d(df %>% 
                    select(x, y, z),
                  aspect = "iso", col = "blue")
    
    # calculate appropriate sphere size
    selection_sphere_size = 0.05 * (max(df$x) - min(df$x))
    
    continue <- readline(prompt="Finished rotation? (\"y\" = yes; \"n\" = no\")")
    if(continue == "Y" | continue == "y") run = T
    
    # start 3D selection
    print(paste0("Select two neighboring facet peaks. Then press the 'Esc' key."))
    
    curr.selection <- selectpoints3d(ids["data"], value = FALSE,
                                     multiple = function(ids) {
                                       spheres3d(df[ids[, "index"], , drop = FALSE], color = "red",
                                                 alpha = .9, radius = selection_sphere_size)
                                       TRUE
                                     })
    run = FALSE
  }
  
  # extract selection coordinates
  curr_selection_coords <- df[curr.selection[(nrow(curr.selection)-1):(nrow(curr.selection)), 2], ]
  
  # calculate distance between points ~= facet diameter
  dist_between_points <- as.numeric(dist(curr_selection_coords))
  print(paste0("Facet diameter: ~", round(dist_between_points,2)))
  
  # calculate search diameter ~ should be ~3 x facet size
  search_diam <- dist_between_points*3
  print(paste0("Search diameter: ", round(search_diam, 2)))
  
  # plot vertex coordinates and search dimater sphere to check its size
  plot3d(df, aspect = "iso", col = "blue")
  spheres3d(df %>% select(x, y, z) %>% 
              slice(2*round(n()/4)),
            radius = search_diam/2, alpha = 0.9, aspect3d = "iso", col="red")
  
  print("done!")
  return(search_diam)
}