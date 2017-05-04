#' Defining a class for enmtools.clade.  Each clade gets:
#' @param species A list of enmtools.species objects
#' @param tree A tree showing the relationships between the species
#'
#' @export enmtools.clade
#' @export summary.enmtools.clade
#' @export print.enmtools.clade
#' @export plot.enmtools.clade


enmtools.clade <- function(species = NA, tree = NA, root.species = NA){

   # Checking classes of input args.  The isTRUE stuff is needed because R doesn't
   # know how to do is.na on raster data, so it was barfing an error when a raster
   # was passed in.

   if(!isTRUE(is.na(species))){

      # Checking to see if species is a list
      if(!"list" %in% class(species)){
         print("Argument species requires a list of enmtools.species objects")
      }

      # This if-statement is asking whether any of the list elements don't have
      # enmtools.species in their class definitions
      if(any(unlist(lapply(species, function(x) !"enmtools.species" %in% class(x))))){
         print("The following objects in the species list do not appear to be enmtools.species objects:")
         print(names(which(unlist(lapply(species, function(x) !"enmtools.species" %in% class(x))))))
      }

   }

   if(!isTRUE(is.na(tree))){
      # Checking to see if species is a list
      if(!"phylo" %in% class(tree)){
         print("Argument tree requires a phylo object")
      }
   }

   output <- list(species = species,
                  tree = tree)

   class(output) <- c("list", "enmtools.clade")

   return(output)
}

summary.enmtools.clade <- function(this.clade){

  this.clade <- check.clade(this.clade)

  cat(paste("\n\nAn enmtools.clade object with", length(this.clade$species), "species\n"))

  cat("\nSpecies names:\n")
  cat(paste0(lapply(this.clade$species, function(x) x$species.name), collapse=", "))

  cat("\n\nTree: ")
  print(this.clade$tree)

  cat("\nData Summary:")
  print(kable(this.clade$summary))

  cat("\n")
}



#' @param maxpixels Plotting all the pixels of large rasters can
#'        very slow, and is unnecessary. The \code{maxpixels} option is
#'        passed to \code{raster::as.raster}, which quickly subsamples
#'        the image to below the \code{maxpixels} number of pixels.
#'        The function \code{plot} (technically \code{raster::plot.raster}) is used
#'        to quickly plot the subset raster.
#'        Setting \code{maxpixels} to Inf would replicate the original
#'        behavior.
#' @param polygons Plot this polygon layer on top of the species records,
#'                 if desired. Default \code{NULL} plots nothing.
#' 
plot.enmtools.clade <- function(this.clade, maxpixels=10000, polygons=NULL){

  # Figure out how many plots you need.  We'll do one for each species (up to 15)
  # and one for the tree.
  n.plots.per.page <- min(16, length(this.clade$species))
  
  if (identical(NA, this.clade$tree) == FALSE)
  	{
    n.plot <- 1 + length(this.clade$species)
		# We'll use this to keep track of how many plots we've made
		plotted <- 1
		} else {
    n.plot <- 0 + length(this.clade$species)
		# We'll use this to keep track of how many plots we've made
		plotted <- 0
		}
    
  # Figure out how many rows and columns we need, declare a new plot
  n.rows <- ceiling(sqrt(n.plots.per.page))
  n.cols <- ceiling(n.plots.per.page/n.rows)

  #plot.new()
  par(mfrow = c(n.rows, n.cols))
	
	# Plot the tree
	if (identical(NA, this.clade$tree) == FALSE)
  	{
  	attempt = try(plot(this.clade$tree))
  	if ("try-error" %in% class(attempt))
  		{
  		attempt2 = try(plot(this.clade$tree, show.tip.label=FALSE))
  			if ("try-error" %in% class(attempt2))
  				{
  				plot(1:10,1:10, pch=".", col="white")
  				txt = paste0("Tree has ", length(this.clade$tree$tip.label), " tips,\ntoo big for plot.")
  				graphics::text(x=5,y=5,labels=txt)
  				} else {
  				axisPhylo()
  				}
  		} else {
  		axisPhylo()
  		}
  	title("Phylogeny")
  	}
	
	# Plot the ranges/background/occurrences
  for(i in (1+plotted):n.plot){
    plot(this.clade$species[[i-plotted]], maxpixels=maxpixels)
    if (is.null(polygons) == FALSE)
    	{
    	plot(polygons, add=TRUE)
    	}
  }

  #par(mfrow = c(1,1))
}

print.enmtools.clade <- function(this.clade){
  summary(this.clade)
}
