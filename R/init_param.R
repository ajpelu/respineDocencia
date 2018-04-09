### -------------------------------
# Initial configuration

# Height for plotOutput
h_plots <- 800

# Colours land Use class 
# note <- to change pine plantation see input$density_pp
col_landUses <- c('#999999', #lightgoldenrod1', # Crops
                  '#f1a340', #green', # Natural forests
                  'white') # Matorrales 

## Create empty landscape
set.seed(123)
ancho <- 63 * 2
alto <- 53 * 2

m <- matrix(nrow=alto, ncol=ancho, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, ancho, alto), nrow=2)
r[] <- 0

## Some parameters
line_pol <- 2 ### Line width polygon
pp_value <- 1 ### Value for Pine plantation
nf_value <- 2 ### Value for Natural forest

# Richness range
ri_range <- as.data.frame(
  cbind(value = c(0,1,2,3),
        lowRich = c(0, 12.82, mean(13.72, 15.62), 1),
        upRich = c(0, 13.34, mean(16.11, 19.66), 2)))

# Input year/m2
piBird = (3.7)/50
piMammal = (0.2)/50

# Themes for raster richness
richness_theme <- rasterTheme(region = brewer.pal(9, "YlGn"),
                              axis.line = list(col = "transparent"),
                              layout.heights = list(xlab.key.padding= 12))


propagule_theme <- rasterTheme(region = brewer.pal(9, "RdBu"),
                               axis.line = list(col = "transparent"),
                               layout.heights = list(xlab.key.padding= 12))


