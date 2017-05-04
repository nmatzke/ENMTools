#' Takes a set of points, a buffer radius, and a mask and returns
#' randomly sampled points from within that buffer radius.
#'
#' Code modified from Elith and Hijmans SDM with R tutorial
#'
#' NOTE: This function can get VERY slow for large rasters, and it's even worse 
#'       if you have to repeat it for each of (say) 40 species.  The major
#'       speedups are:
#'       (1) set up a script where, when runslow is TRUE, the background
#'       rasters are generated and saved to rasters_savedir, and these are just
#'       re-loaded when runslow=FALSE
#' 
#'       (2) Make sure to set nonNAs_to_1=FALSE, and then input an actual landmask into mask, 
#'       where "1" represents land and NA represent non-land (reverse for marine species).
#' 
#'       Code to do #2  is below:
#' 
#' \code{
#' # Load altitude raster from downloaded bioclim
#' alti = raster("rasters/alt_oz.tif")
#' alti <- raster::setMinMax(alti)
#' 
#' # Convert to a NA/1 landmask
#' wd = getwd()
#' rasters_savedir = paste0(wd, "saved_rasters/")
#' if (file.exists(rasters_savedir) == FALSE)
#' 	{
#' 	dir.create(rasters_savedir)
#' 	}
#' filename = paste0(rasters_savedir, "landmask.grd")
#' 
#' # Use calc() to (quite efficiently!) reclassify the raster (better/faster than reclassify)
#' tmpfun <- function(x)
#' 	{
#' 	x[x <= 0] <- NA
#' 	x[x > 0] <- 1
#' 	return(x)
#' 	}
#' landmask = raster::calc(x=env[[3]], fun=tmpfun, filename=filename, format="raster", overwrite=TRUE)
#' landmask <- raster::setMinMax(landmask)
#' 
#' # Plots and statistics
#' plot(landmask, maxpixels=1000)
#' landmask_vals = getValues(landmask)
#' length(landmask_vals)
#' numNAs = sum(is.na(landmask_vals))  			# 97134108
#' numLAND = length(landmask_vals) - numNAs	# 19505892
#' 
#' 
#' # Subsample to lower resolution, retaining NAs (handy for display)
#' filename = paste0(rasters_savedir, "landmask_lowres.grd")
#' landmask_lowres = raster::aggregate(x=landmask, fact=10, fun=modal, na.rm=FALSE, filename=filename, format="raster", overwrite=TRUE)
#' landmask_lowres <- raster::setMinMax(landmask_lowres) 
#' 
#' # Plots and statistics
#' plot(landmask_lowres, maxpixels=1000)
#' landmask_lowres_vals = getValues(landmask_lowres)
#' length(landmask_lowres_vals)
#' numNAs = sum(is.na(landmask_lowres_vals))  			# 958916
#' numLAND = length(landmask_lowres_vals) - numNAs	# 207484
#' }
#'
#' @param points A two column data frame with X and Y coordinates
#' @param radius Radius for circular buffers to draw around points, in meters.
#' @param mask A raster to use as a mask
#' @param nonNAs_to_1 Default is \code{FALSE}. This assumes that your "mask" input
#'        is a landmask (1 for land, NA for non-land) (or, the reverse for marine
#'        species).  If TRUE, then a (slow!) operation will convert all non-NA
#'        values to 1. If you have no NA values in your raster, this will have 
#'        no effect!
#' @param filename The (complete! absolute!) path and filename to save this species 
#'        raster to.  If set to the blank default, \code{""}, the raster may be hard to
#'        load/save in other functions later.
#' @param rasterformat The input for the format option of \code{writeRaster}. 
#'        See \code{?raster::writeRaster}.
#' @param cropfirst Would you like to crop the raster to the extent of the points+buffers? 
#'        Typically a big speedup for large rasters, but still not super-fast. 
#'        Default=\code{FALSE}, because you might well need the full-extent raster for
#'        overlay operations later.
#' @examples
#' # Example speed test below (don't run)
#' test=1
#' 
#' \dontrun{
#' library(devtools)	# install_github
#' #install_github("danlwarren/ENMTools")
#' library(ENMTools)	# hierarchical species distribution modeling
#' # enmtools.aoc
#' 
#' library(BioGeoBEARS)
#' sourceall("/GitHub/ENMTools/R/")
#' 
#' wd = "/GitHub/ENMTools/inst/extdata/"
#' setwd(wd)
#' 
#' # Download only first time you run, then change to FALSE
#' download_data_files = FALSE
#' # Species occurrences from a CSV file (from ENMTools GitHub page)
#' remote_dir = "https://raw.githubusercontent.com/danlwarren/ENMTools/master/test/testdata/"
#' # Species occurrences from a LOCAL file (on your computer)
#' local_dir = "tmp"
#' 
#' 
#' 
#' # Create an empty species object
#' ahli = enmtools.species()
#' ahli
#' 
#' # Load a species object with occurrence data
#' species_occs_fn = "ahli.csv"
#' remote_fn = paste0(remote_dir, species_occs_fn)
#' local_fn = paste0(local_dir, "/", species_occs_fn)
#' #local_fn = "hi/this/is/my/folder/and_file_name.csv" # for a file you have on disk
#' if (file.exists(local_dir) == FALSE)
#' 	{
#' 	dir.create(local_dir)
#' 	}
#' 
#' # Downloading the file. You may need to change the 'method' argument
#' # on some operating systems
#' if (download_data_files == TRUE)
#' 	{
#' 	download.file(url=remote_fn, destfile=local_fn, method="curl")
#' 	}
#' 
#' # Read in the file
#' occs = read.csv(file=local_fn)
#' head(occs)
#' 
#' # Make an ENMTools species object
#' ahli = enmtools.species(species.name="ahli", presence.points=occs[,3:4])
#' check.species(ahli)
#' 
#' 
#' #######################################################
#' # Download and load environmental data (ASCII rasters)
#' # (from ENMTools GitHub page)
#' #######################################################
#' env_fns = c("pc1.asc", "pc2.asc", "pc3.asc", "pc4.asc")
#' cat("\nDownloading ", length(env_fns), "...", sep="")
#' for (i in 1:length(env_fns))
#' 	{
#' 	env_fn = env_fns[i]
#' 	cat("\nFile #", i, "/", length(env_fns), ": ", env_fn, "...\n", sep="")
#' 	remote_fn = paste0(remote_dir, env_fn)
#' 	local_fn = paste0(local_dir, "/", env_fn)
#' 	if (download_data_files == TRUE)
#' 		{
#' 		download.file(url=remote_fn, destfile=local_fn, method="curl", quiet=FALSE)
#' 		}
#' 	#cat("done.")
#' 	} # END for (i in 1:length(env_fns))
#' 
#' local_env_fns <- list.files(path=local_dir, pattern="pc", full.names=TRUE)
#' local_env_fns
#' env <- raster::stack(local_env_fns)
#' names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
#' env <- raster::setMinMax(env)
#' 
#' 
#' #######################################################
#' # Experiment 1: Speed of assigning background points
#' #######################################################
#' 
#' # Assign background points
#' # Original version: 
#' # 1. create a circular buffer around the occurrences
#' # 2. create a shapefile from the buffer
#' # 3. Cut the env raster down (mask it) to the polygon in the buffer shapefile
#' # 4. Randomly extract pixels from the masked raster
#' # The settings below produce the original behavior:
#' ahli$background.points_old = background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=FALSE, use_spsample=FALSE, type="random")
#' 
#' # How long does this take (with a small raster?)
#' orig_time1 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=FALSE, use_spsample=FALSE, type="random"))
#' orig_time1
#' #    user  system elapsed 
#' #   0.925   0.046   0.963 
#' 
#' # Even worse if we use all 4 layers
#' orig_time2 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=FALSE, use_spsample=FALSE, type="random"))
#' orig_time2
#' #    user  system elapsed 
#' #   3.040   0.112   3.128 
#' 
#' # I found that with a larger raster, e.g. 6 tiles of bioclim data (Australasia), this 
#' # operation could take several minutes, and would have to be repeated for each species
#' # The first attempt, "cropfirst=TRUE", uses a crop() function before step 3. This can help
#' # speed somewhat, but is not needed below.
#' 
#' # I revised the function to rely more on vector methods
#' # New version: 
#' # 1. create a circular buffer around the occurrences
#' # 2. create a shapefile from the buffer
#' # 3. Use sp::spsample to sample random long/lat coordinates inside the shapefile
#' # 4. Extract the pixels at these points 
#' 
#' 
#' # Let's use the new defaults.
#' ahli$background.points = background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=FALSE)
#' # Seems faster! How much faster?
#' 
#' new_time1 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random"), extract_values=FALSE)
#' new_time1
#' # user  system elapsed 
#' #   0.052   0.001   0.052 
#' 
#' new_time2 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=TRUE, use_spsample=TRUE, type="random"), extract_values=FALSE)
#' new_time2
#' #    user  system elapsed 
#' #   0.052   0.002   0.052 
#' 
#' # It gets slower if you do extract the values (although still much faster
#' # for large rasters)
#' new_time3 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=TRUE))
#' new_time3
#' #    user  system elapsed 
#' #   0.350   0.005   0.349 
#' 
#' # Even slower for a multi-layer raster
#' new_time4 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=TRUE))
#' new_time4
#' #    user  system elapsed 
#' #   1.256   0.019   1.259 
#' 
#' #######################################################
#' # Experiment 2: Speed of making background raster
#' #######################################################
#' 
#' ahli$range = background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=FALSE)
#' 
#' # The only thing that might be helpful here is cropping first
#' # (but, this might cause problems downstream)
#' raster_time1 = system.time(background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=FALSE))
#' raster_time1
#' #    user  system elapsed 
#' #   1.641   0.033   1.661 
#' 
#' raster_time2 = system.time(background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=TRUE))
#' raster_time2
#' #    user  system elapsed 
#' #   1.421   0.018   1.430 
#' 
#' # The raster overlap stuff, though, could perhaps be done in other ways...
#' }
#' 
#' @export background.raster.buffer


background.raster.buffer <- function(points, radius, mask, nonNAs_to_1=FALSE, filename="", rasterformat="raster", cropfirst=FALSE){

  # 2017-04-26_NJM:
  # Check 'points' for lon/lat, make sure 
  # 1st column = x = longitude
  # 2nd column = y = latitude
  tmpnames = colnames(points)
  
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
  xyvals = points[,c(xcolnum, ycolnum)]
  
  x <- dismo::circles(xyvals, d=radius, lonlat=TRUE)
  pol <-  rgeos::gUnaryUnion(x@polygons)
  
  
  # Cut the mask to 1 layer
  if(length(names(mask)) > 1){
    mask <- mask[[1]]
  }

	# Cropping the raster before mask operation can be faster...but dangerous if you need the same extents
	if (cropfirst == TRUE)
		{
		cropped = raster::crop(x=mask, y=extent(pol))
		} else {
		cropped = mask
		}
  
  # Produce the raster of the buffer; pixels are either NA or 1
  if (nonNAs_to_1 == TRUE)
  	{
  	cropped[!is.na(cropped)] <- 1	# set non-NA values to 1 FIRST. THEN buffer.
  	}
  	
  buffer.raster <- raster::mask(x=cropped, mask=pol, filename=filename, inverse=FALSE, updatevalue=NA, format=rasterformat, overwrite=TRUE)
  #buffer.raster[!is.na(buffer.raster)] <- 1

  return(buffer.raster)
}
