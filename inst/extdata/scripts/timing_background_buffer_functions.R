library(devtools)	# install_github
#install_github("danlwarren/ENMTools")
library(ENMTools)	# hierarchical species distribution modeling
# enmtools.aoc

library(BioGeoBEARS)
sourceall("/GitHub/ENMTools/R/")

wd = "/GitHub/ENMTools/inst/extdata/"
setwd(wd)

# Download only first time you run, then change to FALSE
download_data_files = FALSE
# Species occurrences from a CSV file (from ENMTools GitHub page)
remote_dir = "https://raw.githubusercontent.com/danlwarren/ENMTools/master/test/testdata/"
# Species occurrences from a LOCAL file (on your computer)
local_dir = "tmp"



# Create an empty species object
ahli = enmtools.species()
ahli

# Load a species object with occurrence data
species_occs_fn = "ahli.csv"
remote_fn = paste0(remote_dir, species_occs_fn)
local_fn = paste0(local_dir, "/", species_occs_fn)
#local_fn = "hi/this/is/my/folder/and_file_name.csv" # for a file you have on disk
if (file.exists(local_dir) == FALSE)
	{
	dir.create(local_dir)
	}

# Downloading the file. You may need to change the 'method' argument
# on some operating systems
if (download_data_files == TRUE)
	{
	download.file(url=remote_fn, destfile=local_fn, method="curl")
	}

# Read in the file
occs = read.csv(file=local_fn)
head(occs)

# Make an ENMTools species object
ahli = enmtools.species(species.name="ahli", presence.points=occs[,3:4])
check.species(ahli)


#######################################################
# Download and load environmental data (ASCII rasters)
# (from ENMTools GitHub page)
#######################################################
env_fns = c("pc1.asc", "pc2.asc", "pc3.asc", "pc4.asc")
cat("\nDownloading ", length(env_fns), "...", sep="")
for (i in 1:length(env_fns))
	{
	env_fn = env_fns[i]
	cat("\nFile #", i, "/", length(env_fns), ": ", env_fn, "...\n", sep="")
	remote_fn = paste0(remote_dir, env_fn)
	local_fn = paste0(local_dir, "/", env_fn)
	if (download_data_files == TRUE)
		{
		download.file(url=remote_fn, destfile=local_fn, method="curl", quiet=FALSE)
		}
	#cat("done.")
	} # END for (i in 1:length(env_fns))

local_env_fns <- list.files(path=local_dir, pattern="pc", full.names=TRUE)
local_env_fns
env <- raster::stack(local_env_fns)
names(env) <- c("layer.1", "layer.2", "layer.3", "layer.4")
env <- raster::setMinMax(env)


#######################################################
# Experiment 1: Speed of assigning background points
#######################################################

# Assign background points
# Original version: 
# 1. create a circular buffer around the occurrences
# 2. create a shapefile from the buffer
# 3. Cut the env raster down (mask it) to the polygon in the buffer shapefile
# 4. Randomly extract pixels from the masked raster
# The settings below produce the original behavior:
ahli$background.points_old = background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=FALSE, use_spsample=FALSE, type="random")

# How long does this take (with a small raster?)
orig_time1 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=FALSE, use_spsample=FALSE, type="random"))
orig_time1
#    user  system elapsed 
#   0.925   0.046   0.963 

# Even worse if we use all 4 layers
orig_time2 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=FALSE, use_spsample=FALSE, type="random"))
orig_time2
#    user  system elapsed 
#   3.040   0.112   3.128 

# I found that with a larger raster, e.g. 6 tiles of bioclim data (Australasia), this 
# operation could take several minutes, and would have to be repeated for each species
# The first attempt, "cropfirst=TRUE", uses a crop() function before step 3. This can help
# speed somewhat, but is not needed below.

# I revised the function to rely more on vector methods
# New version: 
# 1. create a circular buffer around the occurrences
# 2. create a shapefile from the buffer
# 3. Use sp::spsample to sample random long/lat coordinates inside the shapefile
# 4. Extract the pixels at these points 


# Let's use the new defaults.
ahli$background.points = background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=FALSE)
# Seems faster! How much faster?

new_time1 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random"), extract_values=FALSE)
new_time1
# user  system elapsed 
#   0.052   0.001   0.052 

new_time2 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=TRUE, use_spsample=TRUE, type="random"), extract_values=FALSE)
new_time2
#    user  system elapsed 
#   0.052   0.002   0.052 

# It gets slower if you do extract the values (although still much faster
# for large rasters)
new_time3 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env[[1]], cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=TRUE))
new_time3
#    user  system elapsed 
#   0.350   0.005   0.349 

# Even slower for a multi-layer raster
new_time4 = system.time(background.points.buffer(points=ahli$presence.points, radius=20000, n=1000, mask=env, cropfirst=TRUE, use_spsample=TRUE, type="random", extract_values=TRUE))
new_time4
#    user  system elapsed 
#   1.256   0.019   1.259 

#######################################################
# Experiment 2: Speed of making background raster
#######################################################

ahli$range = background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=FALSE)

# The only thing that might be helpful here is cropping first
# (but, this might cause problems downstream)
raster_time1 = system.time(background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=FALSE))
raster_time1
#    user  system elapsed 
#   1.641   0.033   1.661 

raster_time2 = system.time(background.raster.buffer(points=ahli$presence.points, radius=50000, mask=env, cropfirst=TRUE))
raster_time2
#    user  system elapsed 
#   1.421   0.018   1.430 


# The raster overlap stuff, though, could perhaps be done in other ways...



