
#' isNAsr: Checking if a species$range item is NA
#'
#' Running \code{is.na} on a species range object, which is a raster::RasterLayer,
#' throws the following error:
#'
#' \code{> isNAsr(this.species)
#' Error in file(fn, "rb") : cannot open the connection
#' In addition: Warning message:
#' In file(fn, "rb") :
#'   cannot open file 
#' '/private/var/folders/4m/cq3wxdkd3rng9wsfgwhc71wh0000gp/T/RtmpkF55An/raster/r_tmp_2017-05-
#' 01_142654_53632_74654.gri': No such file or directory }
#' 
#' Apparently, running \code{is.na()} causes R to check for some temporary file 
#' location, which may shift.
#' 
#' Anyway, one solution is to just do:
#'
#' is.na(this.species["range"])
#'
#' ...which works fine:
#' 
#' \code{is.na(this.species["range"])
#' range 
#' FALSE}
#' 
#' ...and also works if this.species really is NA:
#' 
#' \code{tmp=NA
#' > is.na(tmp["range"])
#' [1] TRUE  }
#' 
#' However, it is handy to put this in a function, in case we need to change/expand it 
#' later to deal with other variants.
#' 
#' @param this.species An object of class \code{enmtools.species}; also of class \code{list}.
#' @examples
#' # Example speed test below (don't run)
#' test=1
#' 
#' \dontrun{
#' isNAsr(this.species)
#' }
#' 
#' @export isNAsr
#' 
isNAsr <- function(this.species)
	{
	TF = is.na(this.species["range"])
	TF = unname(TF)  # This is needed, so that the result is *identical* to TRUE or FALSE
	return(TF)
	}


#' Checking compliance for an object of class enmtools.species.
#'
#' Checks for existence and proper class of:
#' @param range A raster or SpatialPolygon with the actual range they occur in
#' @param presence.points A data frame with sampled localities
#' @param background.points A data frame with absence/pseudoabsence/background localities
#' @param species.name A character vector with the species name
#' @param models A list of models that are made for the species, which will be stuffed in there as we go along
#' to pass the check.  This is used by internal enmtools functions to make sure the necessary data is present
#' before processing anything.
#'
#' @export check.species


check.species <- function(this.species){

  # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
  # know how to do is.na on raster data, so it was barfing and error when a raster
  # was passed in.
  
  # This bit replaces NULL values with NA values
  expect <- c("presence.points", "background.points", 
              "models", "species.name", "range")
  nulls <- names(which(sapply(expect, function(x) is.null(this.species[[x]]))))
  
  # Have to do this in a loop because sapply won't assign NAs for some reason
  for(i in nulls){
    this.species[[i]] <- NA
  }

  if(!isTRUE(isNAsr(this.species))){
    if(!inherits(this.species$range, c("raster", "RasterLayer", "RasterBrick", "RasterStack"))){
      stop("Argument range requires an object of class raster or RasterLayer")
    }
  }

  if(!isTRUE(is.na(this.species$presence.points))){
    if(!inherits(this.species$presence.points, "data.frame")){
      stop("Argument presence.points requires an object of class data.frame")
    }

    # Presence points exist, and are a data frame
    this.species$presence.points <- format.latlon(this.species$presence.points)
  }

  if(!isTRUE(is.na(this.species$background.points))){
    if(!inherits(this.species$background.points, "data.frame")){
      stop("Argument background.points requires an object of class data.frame")
    }

    # Background points exist, and are a data frame
    this.species$background.points <- format.latlon(this.species$background.points)
  }

  if(!isTRUE(is.na(this.species$background.points)) & !isTRUE(is.na(this.species$presence.points))){
    if(any(!colnames(this.species$presence.points) %in% colnames(this.species$background.points))){
      stop("Column names for presence and background points do not match")
    }
  }

  if(!isTRUE(is.na(this.species$species.name))){
    if(!inherits(this.species$species.name, "character")){
      stop("Argument species.name requires an object of class character")
    }
  }

  # Return the formatted species object
  return(this.species)
}


format.latlon <- function(latlon){

  # Basically this bit just tries to auto-identify the lat and lon columns, then returns a
  # reformatted data frame with col names "Longitude" and "Latitude"

  # Try to figure out which columns contain "lon" or "x"
  loncols <- c(which(grepl("^lon", colnames(latlon), ignore.case = TRUE)), match("x", tolower(colnames(latlon))))
  if(any(!is.na(loncols))){
    loncols <- loncols[which(!is.na(loncols))]
  }

  # Ditto for "lat" and "y"
  latcols <- c(which(grepl("^lat", colnames(latlon), ignore.case = TRUE)), match("y", tolower(colnames(latlon))))
  if(any(!is.na(latcols))){
    latcols <- latcols[which(!is.na(latcols))]
  }


  # Check whether we've got one column for each, and make sure they're not the same column
  if(is.na(latcols) | is.na(loncols)){
    stop("Unable to auotmatically determine Longitude and Latitude columns.  Please rename to Longitude and Latitude.")
  }
  if(length(latcols == 1) & length(loncols == 1) & latcols != loncols){
    output <- data.frame(cbind(latlon[,loncols], latlon[,latcols]))
    colnames(output) <- c("Longitude", "Latitude")
  } else {
    stop("Unable to auotmatically determine Longitude and Latitude columns.  Please rename to Longitude and Latitude.")
  }
  return(output)
}
