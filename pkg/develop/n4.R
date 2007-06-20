######################################################################
# Return all the neighbours of a pixel (border problem!)
#
# Params:
#  x - x coordinate of the pixel
#  y - y coordinate of the pixel
#
# Returns an up to 4x2  matrix containing the coordinates
# of the neighbours
######################################################################
n4 <- function(x,y) {
  #Create neighbours (x-coords,y-coords)
  grid <- cbind(c(x,x,x-1,x+1),c(y-1,y+1,y,y))

  #Throw away points which are outside the border
  grid <- grid[ (grid[,1] >= 1) & (grid[,1] <= dimx) &
               (grid[,2] >= 1) & (grid[,2] <= dimy),] 
       
  return(grid)
}

#Go from an index to pixel coordinate, i goes from 1 to dimx*dimy
i2pix <- function(i) {
  i <- i-1
  y <- floor(i/dimx)
  x <- i-y*dimx
  return(cbind(x+1,y+1))
}


#Convert pixel coordinate to index
pix2i <- function(pix) {
  return( (pix[,1]-1) + (pix[,2]-1)*dimx + 1)
}
