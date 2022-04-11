#' Import STL as Tibble
#'
#' Imports triangle centers and triangle normals of STL file as tibble.
#'
#' @param file_name File name of STL to import.
#'
#' @return A tibble containing triangle centers and triangle normals of STL file.
#'
#' @export
#' @importFrom magrittr '%>%'
#' @importFrom dplyr filter pull select mutate arrange slice group_by ungroup left_join summarize distinct
#' first lead lag case_when bind_cols tibble as_tibble desc progress_estimated bind_rows all_of rename n 
#' mutate_all
#' @importFrom rgl plot3d spheres3d selectpoints3d
#' @importFrom tidyr separate
#' @importFrom stats dist
#' @examples
#' # xxx: add example
#'
STL_2_tibble <- function(file_name){
  # dplyr NULLs
  ID <- x <- y <- z <- value <- value.1 <- value.2 <- row_number <- norm.x <- 
    norm.y <- norm.z <- NULL
  
  # load STL file as lines
  print(paste0("Importing ", file_name, "..." ))
  file_in <- file(file_name, open = "r")
  lines <- readLines(file_in)
  # delete first and last lines
  lines <- lines[-c(1, length(lines))]
  close(file_in)
  
  # convert character vector lines to tibble
  print("Converting to tibble...")
  lines_tbl <- as_tibble(lines)
  
  # get coordinates of triangle vertices from lines_tbl
  print("Extracting vertex coordinates of triangles...")
  vertex_coords_triangles <- lines_tbl %>% 
    filter(grepl("vertex", value)) %>% 
    separate(value, into = c("value", "x", "y", "z"), sep = " ") %>% 
    select(-value) %>% 
    mutate_all(as.numeric) 
  
  # create vector of IDs (3 x same number per vertex coordinate)
  IDs <- c()
  for(i in 1:(nrow(vertex_coords_triangles)/3)){
    IDs[(length(IDs)+1):(length(IDs)+3)] <- rep(i, 3)
  }
  
  # get vertex coordinates of triangle centers
  print("Extracting coordinates of triangle centers...")
  vertex_coords_triangle_centers <- vertex_coords_triangles %>% 
    mutate("ID" = IDs) %>% 
    group_by(ID) %>% 
    mutate(x = mean(x), 
           y = mean(y),
           z = mean(z)) %>% 
    distinct(ID, .keep_all = T) %>%
    select(ID, x, y, z) %>% 
    arrange(ID) 
  
  # get normals of triangles
  print("Extracting triangle normals...")
  normals <-  lines_tbl %>% 
    filter(grepl("facet normal", value)) %>% 
    separate(value, into = c("value.1", "value.2", "norm.x", "norm.y", "norm.z"), sep = " ") %>% 
    select(-c(value.1, value.2)) %>% 
    mutate_all(as.numeric) %>% 
    mutate(ID = row_number()) %>% 
    select(ID, norm.x, norm.y, norm.z)
  
  # join triangle center coordinates and their normals
  tri_centers_normals <- left_join(vertex_coords_triangle_centers, normals, by = "ID")
  
  # # plot triangle center coordinates
  # plot3d(tri_centers_normals$x, tri_centers_normals$y, tri_centers_normals$z, aspect = "iso", col = "blue")
  
  print("done!")
  return(tri_centers_normals)
}
