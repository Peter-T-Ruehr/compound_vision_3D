#' Align 3D Point Cloud to Global Axis
#'
#' Rotates 3D point cloud according to one defined vector so that this vector
#' is aligned to one of the global coordinate system axes.
#'
#' @param df A tibble containing coordinates in columns `x, y, z`.
#' @param line_points A 2x3 tibble containing coordinates of line to align the 
#' point cloud to. Must contain one row per points and columns `x, y, z`.
#' @param axis A character string defining the global axis to align to. Must be 
#' `x`, `y`, or `z`.
#' @return Returns a tibble with the aligned coordinates in columns `x, y, z`.
#'
#' @export
#' @examples
#' # xxx: add example
#'
align_to_global_axis = function(df,
                                line_points,
                                axis){
  # https://stackoverflow.com/a/35617817
  
  # define axis to rotate
  x =  line_points %>% 
    slice(2) %>% 
    unlist(., use.names=FALSE) - 
    line_points %>% 
    slice(1) %>% 
    unlist(., use.names=FALSE)
  # x
  
  # define target (global) axis
  if(axis == "x"){
    y <- c(1,0,0)
  } else if(axis == "y"){
    y <- c(0,1,0)
  } else if(axis == "z"){
    y <- c(0,0,1)
  }
  # y
  
  u=x/sqrt(sum(x^2))
  
  v=y-sum(u*y)*u
  v=v/sqrt(sum(v^2))
  
  cost=sum(x*y)/sqrt(sum(x^2))/sqrt(sum(y^2))
  
  sint=sqrt(1-cost^2);
  
  R <- diag(length(x)) - u %*% t(u) - v %*% t(v) + 
    cbind(u,v) %*% matrix(c(cost,-sint,sint,cost), 2) %*% t(cbind(u,v))
  
  df_rot <- as_tibble(as.matrix(df)%*% R) %>% 
    round(., 8)
  
  colnames(df_rot) <- c("x", "y", "z")
  
  return(df_rot)
}