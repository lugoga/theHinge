
## This function convert matrix into a data frame that is suibtable for tidyverse ecosystem.
##
matrix_tb <- function(x="supply the vector containing the the x value of the array",
                      y = " supply the vector containing the y value of the array",
                      data = "supply the the matrix from the array"){

  if(!is.matrix(data)){
    stop("you supplied unsupported file, only matrix format file is acceptable")
  }else{


  require(magrittr)

  dimension <- data.frame(x, data) %>% dim()

  output <- data.frame(x, data) %>%
    tidyr::gather(key = "lati", value = "value",2:dimension[2]) %>%
    dplyr::mutate(y = rep(y, each = dimension[1])) %>%
    dplyr::select(x,y, value) %>%
    tibble::as_tibble()

  return(output)
  }
}


## function for binning irregular spatial data
interpolating_semba = function(x = "longitude spacing", 
                     y = " latitude spacing", 
                     z = "the value to be interpolated"){
   require(tidyverse)
   require(oce)
   
output = interpBarnes(x = x, y = y, z = z)

return(output)
}


## griding irregular spatial data
gridding_semba = function(xmin = "Minimum bound of longitude", 
                          xmax = "Maximum bound of longitude", 
                          ymin = "Minimum bound of latitude", 
                          ymax = "maximu limit of latitud", 
                          xg = "Single value of vector represent longitude", 
                          yg = "single vector of number of latitude", 
                          uz = " a third dimension vector for gridding", 
                          vz = "",
                          eacc.bins.ne = "Simple feature in point geometry"){
  
  output = eacc.sf %>%
    st_crop(xmin = xmin, 
            ymin = ymin, 
            xmax = xmax, 
            ymax = ymax) %>%
    st_make_grid(n = c(xg, yg)) %>%
    st_sf()%>%
    mutate(id = 1:n(),
           contained = lapply(st_contains(geometry, eacc.bins.ne), identity),
           observations = sapply(contained, length),
           observations = replace(observations, observations == 0, NA),
           vz = sapply(contained, function(x) {mean(eacc.bins.ne[x, ]$v, na.rm = TRUE)}),
           uz = sapply(contained, function(x) {mean(eacc.bins.ne[x, ]$u, na.rm = TRUE)})) %>%
    select(-contained)
  
  coordinates = output %>% st_centroid() %>% 
    st_coordinates() %>% 
    as_tibble() %>% 
    rename(lon = 1, lat = 2)
  
  st_geometry(output) = NULL
  
  output = coordinates %>% bind_cols(output)
  
 
  return(output) 
  ## example
  ##mie = gridding_semba(xmin = 38.5, xmax = 42, ymin = -6, ymax = -3, xg = 20, yg = 20, z = eacc.sf$u, eacc.bins.ne = eacc.sf)
}

##  Capitalizing first letter

FirstCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

## The inverse hyperbolic sine function. 
## It transform data to close to logged but allows for zeros, which log() function returns infinity

inverse_hyperbolic = function(x = "a numeric, a data frame or matrix"){
    output = log(x + sqrt(x^2 + 1))
  return(output)
  
}

## function to interpolate irregular data to regular grids. It aslo scale up or downscale points data. 
## this function is faster and more accurately than the interpBarnes from oce package
## it also return the output in tibble format that is compartible with tidyverse ecosystem

interpolate2 <- function(x = "A vector containing a element represent x value",
                         y = "A vector of y-value",
                         z = "A vector containign element of z-value", 
                         n = "determine the spacing, if you specify 10 you get 100 i.e 10*10 = 100"){
  
  ## you need these package installed in your machine
  require(metR)
  require(tidyverse)
  
  ## define the spacing of the grids.
  x_out = seq(from = min(x), to = max(x), length.out = n)
  y_out = seq(from = min(y), to = max(y), length.out = n)
  
  ## the body, work out the interpolation
  output = metR::Interpolate(formula = z~x+y, 
                             x.out = x_out, 
                             y.out = y_out) %>% 
    as_tibble()
  
  ## return the output
  return(output)
  
}

## function that create transect from vector of longitude and latitude
transect = function(lon = "a vector with minimum and maximum value of longitude ",
                    lat = "a vector with minimum and maximum value of latitude ",
                    n = "the number of points representing quadrants along the transect"){
  require(metR)
  require(tidyverse)
  
  output = metR::as.path(x = lon,y = lat,n) %>% 
    as_tibble() %>% 
    select(longitude = x, latitude = y)
  
  return(output)
}

## label for leaflet popups
library("htmltools")

addLabel <- function(data) {
  
  data$label <- paste0(
    '<b>',  '</b><br>
    <table style="width:120px;">
    <tr><td>Village:</td><td align="right">', data$village, '</td></tr>
    <tr><td>Longitude:</td><td align="right">', data$lon, '</td></tr>
    <tr><td>Latitude:</td><td align="right">', data$lat, '</td></tr>
    <tr><td>Stolothrissa:</td><td align="right">', data$stolo, '</td></tr>
     <tr><td>Limnothrissa:</td><td align="right">', data$limno, '</td></tr>
    </table>'
  )
  data$label <- lapply(data$label, HTML)

  return(data)
}