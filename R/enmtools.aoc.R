#' Takes an overlap matrix and a tree and runs permutation tests to
#' determine the statistical significance of the relationship between
#' overlap and time
#'
#' @param clade An enmtools.clade object containing species data and a phylogeny
#' @param nreps A number of reps to do
#' @param overlap.source The source of the overlaps to calculate.  Choices are "bc", "dm", "gam", "glm", "mx", "range", and "point"
#' @param model The model to be used for GLM and GAM comparisons
#' @param overlap.matrix A matrix of overlaps to use, for option overlap.source = "matrix"
#' @param metric The overlap metric to use. For ENM sources, this can be any combination of "D", "I", "cor", "env.D", "env.I", and "env.cor".
#' for range and point overlap this argument is ignored.
#'
#' @export enmtools.aoc
#' @export summary.enmtools.aoc
#' @export print.enmtools.aoc
#' @export plot.enmtools.aoc
#' @export enmtools.aoc.precheck

enmtools.aoc <- function(clade, nreps, overlap.source, env = NULL,  model = NULL, overlap.matrix = NULL, metric = "D"){

  description <- "Age-Overlap Correlation from Monte Carlo Test"

  clade <- check.clade(clade)

  # Make sure the data's okay
  enmtools.aoc.precheck(clade, nreps, overlap.source, env,  model, overlap.matrix, metric)

  # Generate empirical overlaps

  # Bioclim models
  if(overlap.source == "bc"){
    empirical.models <- lapply(clade$species, function(x) enmtools.bc(x, env = env))
  }

  # Domain models
  if(overlap.source == "dm"){
    empirical.models <- lapply(clade$species, function(x) enmtools.dm(x, env = env))
  }

  # GAM models
  if(overlap.source == "gam"){
    empirical.models <- lapply(clade$species, function(x) enmtools.gam(x, env = env, f = model))
  }

  # GLM models
  if(overlap.source == "glm"){
    empirical.models <- lapply(clade$species, function(x) enmtools.glm(x, env = env, f = model))
  }

  # Maxent models
  if(overlap.source == "mx"){
    empirical.models <- lapply(clade$species, function(x) enmtools.maxent(x, env = env))
  }

  if(is.na(match(overlap.source, c("range", "points")))){
    if(grepl(pattern = "^env", metric)){
      overlap <- sapply(empirical.models, function(x) sapply(empirical.models, function(y) env.overlap(x,y, env=env)[metric]))
    } else {
      overlap <- sapply(empirical.models, function(x) sapply(empirical.models, function(y) raster.overlap(x,y)[metric]))
    }
  }



  # Range rasters
  if(overlap.source == "range"){

    # Do pairwise for all species
    overlap <- sapply(clade$species, function(x) sapply(clade$species, function(y) ENMTools::range.overlap(x,y)))
  }

  # Presence points
  if(overlap.source == "points"){
    overlap <- sapply(clade$species, function(x) sapply(clade$species, function(y) ENMTools::point.overlap(x,y)))
  }

  tree <- clade$tree

  # sapply is renaming rows, gotta change tnem back
  rownames(overlap) <- colnames(overlap)

  # Scale empirical overlaps for phylogeny
  empirical.df <- node.overlap(overlap, tree)

  # Build an empirical lm
  empirical.model <- lm(overlap ~ age, data = empirical.df)

  # Define a function for each rep so we can try to apply it
  do.rep <- function(inds) {
    tree$tip.label <- tree$tip.label[inds]
    rep.df <- node.overlap(overlap, tree)
    return(list(rep.df = rep.df,
                rep.lm = lm(overlap ~ age, data = rep.df)))
  }

  reps <- list()

  for(i in 1:nreps){
    this.rep <- sample(nrow(overlap))
    reps[[paste0("rep.", i)]] <- do.rep(sample(length(tree$tip.label)))$rep.lm
  }

  reps.aoc <- rbind(empirical.model$coefficients,
                    do.call(rbind, lapply(reps, function(x) x$coefficients)))

  rownames(reps.aoc) <- c("empirical", paste("rep", 1:nreps))

  p.values <- apply(reps.aoc, 2, function(x) 1 - mean(x > x[1]))
  p.values <- sapply(p.values, function(x) min(x, 1-x)*2)

  intercept.plot <- qplot(reps.aoc[2:nrow(reps.aoc),"(Intercept)"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.aoc[1,"(Intercept)"], linetype = "longdash") +
    guides(fill = FALSE, alpha = FALSE) + xlab("Intercept") + ggtitle(description)

  slope.plot <- qplot(reps.aoc[2:nrow(reps.aoc),"age"], geom = "density", fill = "density", alpha = 0.5) +
    geom_vline(xintercept = reps.aoc[1,"age"], linetype = "longdash") +
    guides(fill = FALSE, alpha = FALSE) + xlab("Slope") + ggtitle(description)

  regressions.plot <- qplot(age, overlap, data = empirical.df) + theme_bw()
  for(i in 2:min(100, nrow(reps.aoc))){
    regressions.plot <- regressions.plot + geom_abline(slope=reps.aoc[i,2],
                                                       intercept=reps.aoc[i,1],
                                                       color="grey86")
  }
  regressions.plot <- regressions.plot + geom_abline(slope = reps.aoc[1,2], intercept = reps.aoc[1,1]) +
    geom_point()

  output <- list(coefficients = reps.aoc,
                 p.values = p.values,
                 intercept.plot = intercept.plot,
                 slope.plot = slope.plot,
                 regressions.plot = regressions.plot,
                 tree = tree,
                 empirical.overlap = overlap,
                 empirical.df = empirical.df,
                 empirical.model = empirical.model,
                 reps = reps)
  class(output) <- "enmtools.aoc"

  return(output)
}

summary.enmtools.aoc <- function(this.aoc){

  cat("\n\nAge-Overlap Correlation test\n\n")
  cat(paste(length(this.aoc$reps), "replicates", "\n\n"))

  cat("p values:\n")
  print(this.aoc$p.values)

  plot(this.aoc)

}

print.enmtools.aoc <- function(this.aoc){
  summary(this.aoc)
}

plot.enmtools.aoc <- function(this.aoc){

  grid.arrange(this.aoc$regressions.plot, this.aoc$intercept.plot,
               this.aoc$slope.plot, ncol = 2)

}

enmtools.aoc.precheck <- function(clade, nreps, overlap.source, env,  model, overlap.matrix, metric){

  if(!inherits(clade$tree, "phylo")){
    stop("Tree is not a phylo object!")
  }

  if(is.null(clade$tree$edge.length)){
    stop("Tree does not have branch lengths!")
  }

  # Check to make sure env data is good
  if(!is.na(match(overlap.source, c("bc", "dm", "mx", "glm", "gam")))){
    if(!inherits(env, c("raster", "RasterLayer", "RasterStack", "RasterBrick"))){
      stop("No environmental rasters were supplied!")
    }
  }

  if(overlap.source == "range"){
    if(any(is.na(lapply(clade$species, function(x) x$range)))){

      stop(paste("Overlap source set to range, but some species are missing range rasters: ",
                 paste(names(clade$species)[which(is.na(lapply(clade$species, function(x) x$range)))], collapse = ", ")))
    }
  }

}
