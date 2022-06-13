# Martin Holdrege

# started 6/23/2022

# Purpose functions used for cross validiation
# including those needed for splitting the spatial data up
# into testing/training groups


#' create raster of quandrants
#'
#' @param r spatRast object
#' @param x x coordinate of the center 
#' @param y y coordinate of the center
#'
#' @return raster with 4 quandrants defined as the quandrants
#' that from around the point x,y
#' @export
#'
#' @examples
#' r <- rast(system.file("external/test.grd", package="raster"))
#' plot(create_quadrants(r, x = 180000, y = 331500))
create_quadrants <- function(r, x, y) {
  n <- length(values(r))
  xy <- xyFromCell(r, 1:n)
  xy <- as.data.frame(xy)
  # upper right
  is_quad1 <- xy$x >= x & xy$y >= y
  # lower right
  is_quad2 <- xy$x >= x & xy$y < y
  
  # lower left
  is_quad3 <- xy$x < x & xy$y < y
  # upper left
  is_quad4 <- xy$x < x & xy$y >= y
  
  if(any(is_quad1 & is_quad2 & is_quad3 & is_quad4)){
    stop('quadrants not uniquely partioned')
  }
  r_out <- r
  r_out[is_quad1] <- 1
  r_out[is_quad2] <- 2
  r_out[is_quad3] <- 3
  r_out[is_quad4] <- 4
  
  r_out
}

#' count the number of cells per quadrant
#'
#' @param xy vector, x & y coordinates of the center of the quadrants (i.e. center of the '+')
#' @param r spatRast object
#' @param just_mse return just the mse term (useful for optimization)
#'
#' @return a list. first item is a numeric vector giving the number of non NA cells in
#' each quadrant. 2nd item is squared difference in number of cells
#' between the smallest and largest quadrant (previously using mse, but this
#' seems better) (this is
#' what you want to minimize if trying to make equal sized quadrants)
#' @examples
#' r <- rast(system.file("external/test.grd", package="raster"))
#' cells_per_quadrant(c(180000, y = 331500), r)
#' # what happens if x and y are no where near the center
#' cells_per_quadrant(c(x = 1, y = 1), r)
cells_per_quadrant <- function(xy, r, just_mse = FALSE) {
  stopifnot(length(xy) == 2)
  x <- xy[1]
  y <- xy[2]
  r2 <- r
  r2[!is.na(r2)] <- 1
  
  r_quads <- create_quadrants(r = r2, x = x, y = y)
  
  quad_vec <- as.numeric(values((r2*r_quads)))
  quad_table <- table(quad_vec)
  out <- as.vector(quad_table)
  names(out) <- attr(quad_table, 'dimnames')$quad_vec
  
  
  quad_levs <- as.character(1:4)
  # filling in 'missing' quadrants
  # if don't do this the lowest mse would be 
  # having just one 'quandrant' with the center off the
  # side of the raster
  out <- out[quad_levs]
  names(out) <- quad_levs
  out[is.na(out)] <- 0
  
  #s <- (mean(out) - out)^2
  mse <- (max(out) - min(out))^2
  #mse <- mean(s)

  stopifnot(length(out) == 4)
  
  if(just_mse) {
    return(mse)
  }
  list(cells_per_quad = out, mse = mse)
}


#' Divide the quadrants in the 'best' way possible
#' 
#' @description find the middle point (center of the quadrants)
#' such that the quadrants are as equal in size as possible 
#' 
#' @param r spatRast object
#' @param x starting guess of x coordinate
#' @param y starting guess of y coordinate
#' @examples 
#' r <- rast(system.file("external/test.grd", package="raster"))
#' out <- best_quadrant_center(r, x = 180000, y = 331500)
#' out
#' r2 <- r
#' r2[!is.na(r2)] <- 1
#' cells_per_quadrant(out, r)
#' plot(create_quadrants(r, x = out['x'], y = out['y'])*r2)
best_quadrant_center <- function(r, x, y) {
  
  result <- optim(par = c(x, y),
        fn = cells_per_quadrant,
        r = r,
        just_mse = TRUE)
  
  # check that it did converge
  stopifnot(result$convergence == 0)
  
  out <- result$par
  names(out) <- c('x', 'y')
  out
}

