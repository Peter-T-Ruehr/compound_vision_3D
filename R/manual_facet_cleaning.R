#' Remove Facets Interactively
#'
#' Remove facets that have been identified by the `find_facet_peaks_fine()` 
#' function interactively.
#'
#' @param clusters A tibble containing facet coordinates in columns `x, y, z`.
#' @param local_heights A tibble containing all vertex coordinates extracted 
#' from STLs in columns `x, y, z`.
#' @param selection.indices.rem A numerical list of facet indices to remove. 
#' In the first run of `remove_facets_manually()`, these should be set to 
#' `NULL`.The list will then be amended with every iteration of 
#' `remove_facets_manually()`. Make sure to not reset this list, because than 
#' all info on selected facets to be removed will be gone.
#' @param plot_diam_facets A numeric value for plotting facets. This should be 
#' roughly the size of one facet. Default: `50`.
#' @param plot_diam_points A numeric value for plotting points. This should be 
#' roughly 1/10th of the size of one facet. Default: `plot_diam_facets/10`.
#' @return `selection.indices.rem` See above.
#'
#' @export
#' @examples
#' # xxx: add example
#'
# remove facets manually
remove_facets_manually <- function(clusters,
                                   local_heights,
                                   selection.indices.rem,
                                   plot_diam_facets = 50,
                                   plot_diam_points = plot_diam_facets/10){
  
  plot_size = plot_diam_facets/4
  ids <- plot3d(clusters %>% 
                  select(x,y,z), 
                col="red", 
                size=plot_size, 
                aspect = "iso")
  
  points3d(local_heights[,2:4],
           col = local_heights$local_height_cols,
           size=plot_diam_points,
           alpha = 0.8)
  if(length(selection.indices.rem) > 0){ points3d(clusters[selection.indices.rem, 1:3], col="orange", size=plot_size*1.5, alpha = 0.5) }
  # text3d(clusters[,1:3], texts = 1:nrow(clusters), pos = 3, col = "blue", cex=1.5)
  
  run = TRUE
  while(run == TRUE){
    print("Select facets (red points) to delete.")
    print("If you want to change the view or are finished with the selection process, press the `Esc` key.")
    curr.selection <- selectpoints3d(ids["data"], value = FALSE,
                                     multiple = function(ids) {
                                       points3d(clusters[ids[, "index"], , drop = FALSE], color = "orange", 
                                                alpha = 0.6, size = plot_size*1.5)
                                       TRUE
                                     })
    curr.selection <- as_tibble(curr.selection) %>% 
      pull(index)
    selection.indices.rem <-unique(c(selection.indices.rem, curr.selection))
    
    continue <- readline(prompt="Continue selection? (\"y\" = yes; \"n\" = no\")")
    if(continue == "N" | continue == "n") run = FALSE
  }
  
  plot3d(clusters %>% 
           select(x,y,z), 
         col="red", 
         size=plot_size, 
         aspect = "iso")
  points3d(local_heights[,2:4],
           col = local_heights$local_height_cols,
           size=plot_diam_points)
  points3d(clusters[selection.indices.rem,1:3], 
           col="orange", 
           size=plot_size*1.5)
  
  return(selection.indices.rem)
}



#' Add Facets Interactively
#'
#' Add facets that that are missing interactively.
#'
#' @param clusters A tibble containing facet coordinates in columns `x, y, z`.
#' @param local_heights A tibble containing all vertex coordinates extracted 
#' from STLs in columns `x, y, z`.
#' @param selection.indices.add A numerical list of facet indices belonging to 
#' to `local_heights` to add to `clusters`.
#' In the first run of `add_facets_manually()`, these should be set to 
#' `NULL`.The list will then be amended with every iteration of 
#' `add_facets_manually()`. Make sure to not reset this list, because than 
#' all info on selected facets to be removed will be gone.
#' @param plot_diam_facets A numeric value for plotting facets. This should be 
#' roughly the size of one facet. Default: `50`.
#' @param plot_diam_points A numeric value for plotting points. This should be 
#' roughly 1/10th of the size of one facet. Default: `plot_diam_facets/10`.
#' @return `selection.indices.add` See above.
#'
#' @export
#' @examples
#' # xxx: add example
#'
# add facets manually
add_facets_manually <- function(clusters,
                                local_heights,
                                selection.indices.add,
                                plot_diam_facets = 50,
                                plot_diam_points = plot_diam_facets/10){
  
  plot_size = plot_diam_facets/4
  
  local_heights_tmp <- local_heights %>% 
    select(x,y,z)
  
  ids <- plot3d(local_heights_tmp,
                col = local_heights$local_height_cols,
                size=plot_diam_points, 
                aspect = "iso",
                alpha = 0.8)
  
  points3d(facets_rem %>% 
             select(x,y,z), 
           col="red", 
           size=plot_size)
  
  if(length(selection.indices.add) > 0){
    points3d(local_heights[selection.indices.add,] %>% 
               select(x,y,z), 
             col="orange", 
             size=plot_size)
  }
  
  run = TRUE
  while(run == TRUE){
    print("Select facets (red points) to delete.")
    print("If you want to change the view or are finished with the selection process, press the `Esc` key.")
    curr.selection <- selectpoints3d(ids["data"], value = FALSE,
                                     multiple = function(ids) {
                                       points3d(local_heights_tmp[ids[, "index"], , drop = FALSE], color = "blue", 
                                                alpha = 0.3, size = plot_size*1.5)
                                       TRUE
                                     })
    curr.selection <- as_tibble(curr.selection) %>% 
      pull(index)
    selection.indices.add <- unique(c(selection.indices.add, curr.selection))
    
    continue <- readline(prompt="Continue selection? (\"y\" = yes; \"n\" = no\")")
    if(continue == "N" | continue == "n") run = FALSE
  }
  
  
  plot3d(clusters %>% 
           select(x,y,z), 
         col="red", 
         size=plot_size, 
         aspect = "iso")
  points3d(local_heights[,2:4],
           col = local_heights$local_height_cols,
           size=plot_diam_points)
  points3d(clusters[selection.indices.add,1:3], 
           col="blue", 
           size=plot_size*1.5)
  
  return(selection.indices.add)
}