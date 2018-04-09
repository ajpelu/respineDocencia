library('shiny')
library('shinycssloaders')
library('raster')
library('landscapeR')
library('rasterVis')
library('RColorBrewer')
library('sp')
library('rgeos')


### -------------------------------
# Load functions
source('createLandscape.R', local=TRUE)
source('initRichness.R', local = TRUE)
source('dist2nf.R', local = TRUE)
source('disper.R', local=TRUE)
source('disper_time.R', local=TRUE)


### -------------------------------
# Initial configuration

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

# Height for plotOutput
h_plots <- 1000

### -------------------------------
# SERVER
shinyServer(
  function(input, output, session){

    ### ----------------------------------------------
    # Density
    den_pp <- reactive ({
      list(
        den = switch(input$density_pp, 'baja' = 100, 'media' = 1250, 'alta' = 3000),
        col = switch(input$density_pp,'baja' = '#a1d99b', 'media' = '#238b45','alta' = '#00441b'))
      })

    # Create landscape
    landscapeInit <- reactive({
      createLandscape(r, size_pp = input$size_pp, size_nf = input$size_nf, n_nf = input$n_nf)
      })

    # Past Use
    pastUse <- reactive({
      switch(input$pp_pastUse, 'Bosque natural' = 'Oak', 'Matorral' = 'Shrubland',
             'Pastizal' = 'Pasture','Cultivo' = 'Crop')
      })

    ### ----------------------------------------------
    # Dispersion table
    ## slider conditioned to small_bird slider (see ui.R)
    output$mb <- renderUI({
      sliderInput(inputId = "mb",
                  label = "Aves mediano tamaño",
                  min = 0, max = 100 - input$sb, value = 0)
      })

    # disp <- reactive({
    #   list(persb = input$sb,
    #        permb = input$mb,
    #        perma = (100-(input$sb + input$mb)))
    # })


    perma <- reactive({
      100-(input$sb + input$mb)
    })

    # output$disptable <- renderTable({
    #   tabla <- cbind(SmallBirds = disp()$persb,
    #                  MediumBirds = disp()$permb,
    #                  Mammals = disp()$perma)
    #   tabla},
    #   hover = TRUE, spacing = 'xs', align = 'c', digits = 0)

    output$disptable <- renderTable({
      tabla <- cbind(disperser = c('SmallBirds', 'MediumBirds', 'Mammals'),
                     percentage = c(input$sb, input$mb, perma()))
      tabla},
      hover = TRUE, spacing = 'xs', align = 'c', digits = 0)


    ### ----------------------------------------------
    ## Distance raster
    dist_raster <- reactive({
      dist2nf(landscapeInit(), nf_value = nf_value)
      })

    ## Compute initial Richnness
    rasterRich <- reactive({
      initRichness(r = landscapeInit(), draster = dist_raster(),
                   r_range = ri_range, treedensity = den_pp()$den,
                   pastUse = pastUse(), rescale = FALSE)
    })

    ## Get bouondary of pp
    limit_pp <- reactive({
      rasterToPolygons(landscapeInit(), fun=function(x){x==pp_value}, dissolve = TRUE)
    })

    ## extension of Landscape Init
    ext <- reactive({
      list(
        xmin = extent(landscapeInit())@xmin,
        xmax = extent(landscapeInit())@xmax,
        ymin = extent(landscapeInit())@ymin,
        ymax = extent(landscapeInit())@ymax)
    })


    ### ----------------------------------------------
    ## Compute dispersion rasters
    rasterDisp <- reactive({
      disper(x = landscapeInit(), xr = rasterRich(), nf_value = nf_value, pp_value = pp_value)
      })

    ## Compute Richness pine plantations
    rich_pp <- reactive({
     calc(stack(landscapeInit(), rasterRich()), fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))
    })



    propagule_sb <- reactive({
      rasterDisp()[['msb']] * as.numeric(input$sb)
    })

    propagule_mb <- reactive({
      rasterDisp()[['mmb']] * as.numeric(input$mb)
    })

    propagule_ma <- reactive({
      rasterDisp()[['mma']] * as.numeric(perma())

    })


    propagule_bird_aux <- reactive({
      calc(stack(propagule_sb(), propagule_mb()),sum)
    })

    propagule_bird <- reactive({
      propagule_bird_aux() * piBird
    })

    propagule_mammal <- reactive({
      propagule_ma() * piMammal
    })

    propagule <- reactive({
      calc(stack(propagule_bird(), propagule_mammal()), sum)
      })

    # ## Input propagule
    # propagule <- reactive({
    #   # Compute propagule input by cell
    #
    #   persb <- as.numeric(disp()$persb)
    #   permb <- as.numeric(disp()$permb)
    #   perma <- as.numeric(disp()$perma)
    #
    #   msb <-
    #   mmb <- rasterDisp()[['mmb']]
    #   mma <- rasterDisp()[['mma']]
    #
    #
    #
    #   propagule <- piBird * ((msb * persb) + (mmb * permb)) + ((mma * perma) * piMammal)
    #   list(propagule)
    #
    #   # propagule <- piBird * ((rasterDisp()[['msb']] * disp()$persb) + (rasterDisp()[['mmb']] * disp()$permb)) + (rasterDisp()[['mma']] * disp()$perma) * piMammal
    # })


    ## Richness nf
    rich_nf <- reactive({
      rich_nf <- calc(stack(landscapeInit(), rasterRich()), fun=function(x) ifelse(x[1] == nf_value, (x[1]/nf_value)*x[2], NA))
      })


    ## Richness End
    rich_end <- reactive({
      propagulo_time <- rich_pp() + propagule()*input$timeRange

      rich_time <- calc(stack(landscapeInit(),
                              rasterRich(),
                              propagulo_time),
                        fun = function(x) ifelse(x[1] == pp_value, x[1]*x[3], x[2]))
      rich_time[rich_time== 0] <- NA

      list(
        rich_pp_end = propagulo_time,
        rich_time = rich_time)

    })


    ### ----------------------------------------------
    # Endpoints

    observeEvent(input$doPaisaje, {
      output$plotMaps <- renderUI({
        withSpinner(
          plotOutput("initial_map", height = h_plots),
        type=5, size=.8)
      })

      output$initial_map <- renderPlot({
        colores <- c('lightgoldenrod1', # Crops
                         'green', # Natural forests
                         'white', # Other
                         den_pp()$col) # Pine plantation
            key_landuses <- list(text = list(lab = c("Cultivos", "Bosques Naturales","Matorrales", "Pinares")),
                                 rectangles=list(col = colores), space='bottom', columns=4)

            levelplot(landscapeInit(), att='landuse', scales=list(draw=FALSE),
                      col.regions = colores, colorkey=FALSE, key = key_landuses,
                      par.settings = list(axis.line = list(col = "transparent"),
                                          layout.heights = list(xlab.key.padding= 12))) +
              spplot(limit_pp(), fill = "transparent", col = "black",
                     xlim = c(ext()$xmin, ext()$xmax), ylim = c(ext()$ymin, ext()$ymax),
                     colorkey = FALSE, lwd=line_pol)
      })

    })

    observeEvent(input$doRiquezaInit, {
      output$plotMaps <- renderUI({
        withSpinner(
        plotOutput("richness_map", height = h_plots),
        type=5, size=.8)})

      output$richness_map <- renderPlot({

          mapa_riqueza <- rasterRich()
          mapa_riqueza[mapa_riqueza == 0] <- NA

          levelplot(mapa_riqueza, par.settings = richness_theme, margin = FALSE,
                    scales=list(draw=FALSE), pretty=TRUE,
                    colorkey = list(space = "bottom")) +
            spplot(limit_pp(), fill = "transparent", col = "black",
                   xlim = c(ext()$xmin, ext()$xmax), ylim = c(ext()$ymin, ext()$ymax),
                   colorkey = FALSE, lwd=line_pol)
      })

    })

    observeEvent(input$doPropagulo, {
      output$plotMaps <- renderUI({
        withSpinner(
          plotOutput("richness_disper", height = h_plots),
          type=5, size=.8)})

      output$richness_disper <- renderPlot({
        levelplot(propagule(),
                  margin=FALSE,  par.settings = propagule_theme,
                  scales=list(draw=FALSE), colorkey = list(space = "bottom"))
      })
    })

    observeEvent(input$doRiquezaEnd, {
      output$plotMaps <- renderUI({
        withSpinner(
          plotOutput("richness_disperTime", height = h_plots),
          type=5, size=.8)})

      output$richness_disperTime <- renderPlot({
        rend <- rich_end()$rich_time
        levelplot(stack(rend),
                  par.settings = richness_theme, margin = FALSE, pretty=TRUE,
                  scales=list(draw=FALSE), colorkey = list(space = "bottom")) +
          spplot(limit_pp(), fill = "transparent", col = "black",
                 xlim = c(ext()$xmin, ext()$xmax), ylim = c(ext()$ymin, ext()$ymax),
                 colorkey = FALSE, lwd=line_pol)

      })

    })


    ### ----------------------------------------------
    ## Richness Info Boxes
    output$rich_ppInitBox <- renderValueBox({
      valueBox(
               value = round(cellStats(rich_pp(), mean),2),
               subtitle = paste0(
                 round(cellStats(rich_pp(), min),2), " - ",
                 round(cellStats(rich_pp(), max),2)),
               icon = icon('tree-conifer', lib='glyphicon'), color = 'green')
    })

    output$rich_nfBox <- renderValueBox({
      valueBox(value = round(cellStats(rich_nf(), mean),2),
               subtitle = paste0(
                 round(cellStats(rich_nf(), min),2), " - ",
                 round(cellStats(rich_nf(), max),2)),
               icon = icon('tree-deciduous', lib='glyphicon'), color = 'yellow')
    })

    output$rich_ppEndBox <- renderValueBox({
      valueBox(value = round(cellStats(rich_end()$rich_pp_end, mean),2),
               subtitle = paste0(
                 round(cellStats(rich_end()$rich_pp_end, min),2), " - ",
                 round(cellStats(rich_end()$rich_pp_end, max),2)),
               icon = icon('tree-conifer', lib='glyphicon'), color = 'olive')
    })

}

)


