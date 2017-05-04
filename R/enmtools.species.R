#' Defining a class for enmtools.species.
#'
#' Each species gets:
#' @param range A raster or SpatialPolygon with the actual range they occur in
#' @param presence.points A data frame with sampled localities
#' @param background.points A data frame with absence/pseudoabsence/background localities
#' @param species.name A character vector with the species name
#' @param models A list of models that are made for the species, which will
#'    be stuffed in there as we go along
#'
#' @export enmtools.species
#' @export summary.enmtools.species
#' @export print.enmtools.species
#' @export plot.enmtools.species


enmtools.species <- function(range = NA, presence.points = NA, background.points = NA,
                             species.name = NA, models=NA){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.

  if(!isTRUE(is.na(range))){
    if(!any(c("raster", "RasterLayer", "SpatialPolygons") %in% class(range))){
      print("Argument range requires an object of class raster or SpatialPolygons")
    }
  }

  if(!isTRUE(is.na(presence.points))){
    if(!any(c("data.frame") %in% class(presence.points))){
      print("Argument presence.points requires an object of class data.frame")
    }

  # 2017-04-26_NJM:
  # Check 'points' for lon/lat, make sure 
  # 1st column = x = longitude
  # 2nd column = y = latitude
  tmpnames = colnames(presence.points)
  
  # First column with "lon" in it is x, longitude
  xTF = grepl(pattern="lon", x=tmpnames)
  if (any(xTF) == TRUE)
  	{
    xcolnum = (1:length(tmpnames))[xTF][1]
    } else {
    xcolnum = 1
    }
  # First column with "lat" in it is y, latitude
  yTF = grepl(pattern="lat", x=tmpnames)
  if (any(yTF) == TRUE)
  	{
    ycolnum = (1:length(tmpnames))[yTF][1]
    } else {
    ycolnum = 2
    }
  xyvals = presence.points[,c(xcolnum, ycolnum)]
	presence.points = xyvals
	names(presence.points) = c("longitude", "latitude")
  }

  if(!isTRUE(is.na(background.points))){
    if(!any("data.frame" %in% class(background.points))){
      print("Argument background.points requires an object of class data.frame")
    }

  # 2017-04-26_NJM:
  # Check 'points' for lon/lat, make sure 
  # 1st column = x = longitude
  # 2nd column = y = latitude
  tmpnames = colnames(background.points)
  
  # First column with "lon" in it is x, longitude
  xTF = grepl(pattern="lon", x=tmpnames)
  if (any(xTF) == TRUE)
  	{
    xcolnum = (1:length(tmpnames))[xTF][1]
    } else {
    xcolnum = 1
    }
  # First column with "lat" in it is y, latitude
  yTF = grepl(pattern="lat", x=tmpnames)
  if (any(yTF) == TRUE)
  	{
    ycolnum = (1:length(tmpnames))[yTF][1]
    } else {
    ycolnum = 2
    }
  xyvals = background.points[,c(xcolnum, ycolnum)]
	background.points = xyvals
	names(background.points) = c("longitude", "latitude")
  }

  if(!isTRUE(is.na(species.name))){
    if(!any("character" %in% class(species.name))){
      print("Argument species.name requires an object of class character")
    }
  }






  output <- list(
    range = range,
    presence.points = presence.points,
    background.points = background.points,
    models = models,
    species.name = species.name)

  class(output) <- c("list", "enmtools.species")

  return(output)
}


summary.enmtools.species <- function(this.species){
  stopifnot(inherits(this.species, "enmtools.species"))

  if(class(this.species$range) == "RasterLayer"){
    cat("\n\nRange raster: \n")
    print(this.species$range)
  } else {
    cat("\n\nRange raster not defined.")
  }

  if(class(this.species$presence.points) %in% c("data.frame", "matrix")){
    cat("\n\nPresence points (first ten only): ")
    print(kable(head(this.species$presence.points, 10)))
  } else{
    cat("\n\nPresence points not defined.")
  }

  if(class(this.species$background.points)  %in% c("data.frame", "matrix")){
    cat("\n\nBackground points (first ten only): ")
    print(kable(head(this.species$background.points, 10)))
  } else{
    cat("\n\nBackground points not defined.")
  }

  if(!is.na(this.species$models)){
    for(i in 1:length(this.species$models)){
      print(summary(this.species$models[[i]]))
    }
  }

  if(class(this.species$species.name) == "character"){
    cat(paste("\n\nSpecies name: ", this.species$species.name))
  } else {
    cat("\n\nSpecies name not defined.")
  }

  cat("\n\n")

}


#' @param maxpixels Plotting all the pixels of large rasters can
#'        very slow, and is unnecessary. The \code{maxpixels} option is
#'        passed to \code{raster::as.raster}, which quickly subsamples
#'        the image to below the \code{maxpixels} number of pixels.
#'        The function \code{plot} (technically \code{raster::plot.raster}) is used
#'        to quickly plot the subset raster.
#'        Setting \code{maxpixels} to Inf would replicate the original
#'        behavior.
#' @param legend Show the colorbar. Default \code{FALSE} is particularly
#'               useful when plotting many images
#' @param box    Show the box around the image. Default \code{TRUE}.
#' @param axes   Show the axis ticks and tick labels. Default \code{FALSE} is particularly
#'               useful when plotting many images
#' 
plot.enmtools.species <- function(this.species, maxpixels=10000, legend=FALSE, box=TRUE, axes=FALSE, nickmar=TRUE){
  stopifnot(inherits(this.species, "enmtools.species"))

  if(class(this.species$range) == "RasterLayer"){
		plot(this.species$range, maxpixels=maxpixels, legend=legend, box=box, axes=axes)
		}

  if(class(this.species$background.points)  %in% c("data.frame", "matrix")){
    points(this.species$background.points[,1:2], pch=".", col="gray")
  }

  if(class(this.species$presence.points) %in% c("data.frame", "matrix")){
    points(this.species$presence.points[,1:2], pch=".", col="black")
  }

  if(class(this.species$species.name) == "character"){
    title(this.species$species.name)
  }
}

print.enmtools.species <- function(this.species){

  summary(this.species)

}

