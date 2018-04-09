#' Set values of Initial Richness
#'
#' Esta funcion establece los valores iniciales de riqueza de especies
#'
#' @param r A \code{raster} object
#'
#' @param draster A \code{raster} object with distance to nearest oak TODO (COMPLETAR)
#'
#' @param r_range A \code{data frame} with three columns: \code{value} of land use
#' (\code{integer}: 0 = "Other", 1 = "Pine plantation", 2 = "Natural Forests",
#' 3 = "Crop"); \code{lowRich} and \code{upRich} (lower an upper value of the
#' range of Richness: See Gomez-Aparicio et al 2009)
#'
#' @param treedensity density of the pine plantation (\code{integer})
#'
#' @param pastUse the past land use of the pine plantation (\code{character}).
#' One of "Oak", "Shrubland", "Pasture" or "Crop"
#'
#' @param rescale If "TRUE" the results are rescaled
#'
#' @return A \code{raster} object with values of initial Richness for each
#' pixel.
#'
#' @references
#'
#' Gomez-Aparicio L, Zavala MA, Bonet FJ, Zamora R (2009) Are pine plantations
#' valid tools for restoring Mediterranean forests? An assessment along abiotic
#' and biotic gradients. Ecological Applications, 19: 2124 - 2141.
#'
#'

initRichness <- function(r, draster, r_range, treedensity, pastUse, rescale=TRUE){

  # --- N cells
  ncell_pp <- ncell(r[r == 1])
  ncell_nf <- ncell(r[r == 2])
  ncell_crop <- ncell(r[r == 3])

  # --- Potential Richness values
  ## Ranges
  range_pp <- r_range[which(r_range$value == 1), ]
  range_nf <- r_range[which(r_range$value == 2), ]
  range_crop <- r_range[which(r_range$value == 3), ]

  ## Potential vectors
  potR_pp <- runif(ncell_pp*3, range_pp$lowRich, range_pp$upRich)
  potR_nf <- runif(ncell_nf*3, range_nf$lowRich, range_nf$upRich)
  potR_crop <- runif(ncell_crop*3, range_crop$lowRich, range_crop$upRich)

  # --- Reclassify
  r[r == 0] <- NA
  r[r == 1] <- -100
  r[r == 2] <- -200
  r[r == 3] <- -300

  # --- Pine plantation
  ## ~ TreeDensity
  ### Fraction of Potential Richness (tree Density Eq. 3 Gomez Aparicio et al. 2009)
  ftreeden <- exp(-0.5*((treedensity - 0.22)/1504.1)^2)

  ## ~ Distance to Seed Source
  ### Compute diversity raster (See Gonzalez-Moreno et al. 2011)
  sh <- calc(draster, fun=function(x){1.7605 - 0.0932*(sqrt(sqrt(x)))})

  ### Create a stack with the shanon diversity raster and landuse raster, and then compute values for pine plantations
  s <- calc(stack(r, sh), fun=function(x)  ifelse(x[1] == -100 , (x[1]/-100)*x[2],  NA))

  ### Scale the distance effect from 0 to 1
  sh_scaled <- (s - cellStats(s, "min"))/(cellStats(s, "max") - cellStats(s, "min"))

  ## ~ PastUSE
  ### Past Land Use
  fplu <- ifelse(pastUse == 'Oak', .9999,
                 ifelse(pastUse == 'Shrubland', .4982,
                        ifelse(pastUse == 'Crop', .0279, .0001)))

  ## Combine factor to correct pine plantations
  f_pine <- (sh_scaled*0.35) + (.45*ftreeden + .2*fplu)

  r[r == -100]  <- sample(potR_pp, ncell_pp, replace = TRUE)
  r <- calc(stack(r, f_pine), fun = function(x) ifelse(x[1] < -100, x[1], x[1]*x[2]))

  # --- Crops
  r[r == -300]  <- sample(potR_crop, ncell_crop, replace = TRUE)

  # --- Natural forest
  r[r == -200]  <- sample(potR_nf, ncell_nf, replace = TRUE)

  # Rescale results
  if (rescale)
    r <- (r - cellStats(r, "min"))/(cellStats(r, "max") - cellStats(r, "min"))

  return(r)

}
