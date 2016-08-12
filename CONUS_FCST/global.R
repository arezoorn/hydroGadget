library(shiny)
library(leaflet)
library(rwrfhydro)
library(png)

load("data/masterUsgsMetadata.Rdata")

# Plot Specs
#globalPlotDir <- '/d7/adugger/WRF_Hydro/InspGadget/CONUS_FCST/plots/'
#globalPlotDir <- '/glade/p/ral/RHAP/arezoo/InspGadget/CONUS_FCST/plots/' #A
globalPlotDir <- '/glade/p/ral/RHAP/adugger/CONUS_IOC/real_time_plots/'

prefList <- list(short="/FCST/short/",
                 medium="/FCST/medium/",
                 long="/FCST/long/",
                 retro="/RETRO/5yr/",
                 ol="/ANALYSIS/",
                 mapshort="/MAP_FCST/short/",
                 mapmedium="/MAP_FCST/medium/",
                 maplong="/MAP_FCST/long/",
                 mapol="/MAP_ANALYSIS/")

suff1List <- list(short="_short_range_plot",
                 medium="_medium_range_plot",
                 long="_long_range_plot",
                 retro="_5yr_retro",
                 ol="_openloop",
                 mapshort="_short_range_plot",
                 mapmedium="_medium_range_plot",
                 maplong="_long_range_plot",
                 mapol="_open_loop_plot")

suff2List <- list(fcsttime=".byFcstTime",
                 leadtime=".byLeadTime",
                 none="")

suff3List <- list(standard=".Linear.png",
                 log=".Log.png")

suff4List <- list(intervening=".intervening", #A
                  contributing=".contributing")  #A

suff5List <- list(accum=".acc", #A
                  noAccum=".noacc")  #A

# Graphics
gageIcons <- iconList(red = makeIcon("marker_darkred.png", iconWidth = 24, iconHeight =32),
                      blue = makeIcon("marker_darkblue.png", iconWidth = 24, iconHeight =32))

palfun <- colorFactor(palette=c("navy", "darkmagenta"), domain=c(TRUE, FALSE))


# Setup gauge points
masterMeta <- masterMeta[order(masterMeta$dec_long_va),]
#gagesDf <- plyr::mutate(masterMeta, group = factor(masterMeta$inGagesIIRef, labels = c("red", "blue")))
masterMeta$sizefact <- cut(masterMeta$qmean, breaks=c(0, 5, 20, 50, 100, 20000), labels=c(4,6,8,10,12))
masterMeta$sizefact <- as.character(masterMeta$sizefact)
masterMeta$sizefact <- ifelse(is.na(masterMeta$sizefact), "4", masterMeta$sizefact)
masterMeta$sizefact <- factor(masterMeta$sizefact, levels=c(4,6,8,10,12))
masterMeta$sizefact <- as.integer(as.character(masterMeta$sizefact))

# Function to get plot path
plotType <- function(x, plottype, axistype, plotdir, fcstplottyp) {
  axistype_full <- paste0(axistype, "_", fcstplottyp)
  if ( (plottype == "retro") | (plottype == "ol") ) fcstplottyp <- "none"
  if (plottype == "long") {
        axistype <- "standard"
        fcstplottyp <- "fcsttime"
  }
  switch(axistype_full,
         "log_fcsttime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "standard_fcsttime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "log_leadtime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "standard_leadtime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]))
}

# Function to get plot path for MAP
MapPlotType <- function(x, plottype, axistype, plotdir, fcstplottyp, areatyp, acctyp ){
#  axistype_full <- paste0(axistype, "_", fcstplottyp)
  if (plottype == "mapol") fcstplottyp <- "none"
#  if (plottype == "long") {
#        axistype <- "standard"
#        fcstplottyp <- "fcsttime"
#  }
   paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff4List[[areatyp]], suff5List[[acctyp]], suff3List[[axistype]])
}


#****************************************************************************************************
#                           UI
#****************************************************************************************************

ui <-  fluidPage(
    theme = "bootstrap.css",
    windowTitle="HydroInspectorGadget",
   
#    titlePanel(tags$h1("HydroInspector(Gadget!): National Water Model",  
#        style = "font-family: 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif;
#        color: #fff; text-align: left;
#        background-color: #4372AA;
#        padding: 20px"), windowTitle="HydroInspectorGadget"),

      tags$head(
        tags$style(HTML("
          .navbar .navbar-nav {float: right}
          .navbar .navbar-header {float: left}
        "))),

  navbarPage(tags$b("HydroInspector(Gadget!): National Water Model",
        style = "font-family: 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif;
        color: #fff; text-align: left;
        font-size: 40px; line-height: 40px"),
        windowTitle="HydroInspectorGadget", 
    #theme = "bootstrap.css",
    #tabsetPanel(position="top",
# MASTER TAB MAP
     tabPanel("MAP",
    
    fluidRow(
       column(6,
# MAP FRAME
          wellPanel(leafletOutput("map", width= "100%", height=800)),
          fluidRow(
             column(6,
# LEGEND
                tableOutput("legendTable"),
                br()),
             column(3,
# GAGE ID
                textInput('selGage', 'Zoom to Gage ID:', value="", width='50%'),
                br()),
             column(3, 
# COMID
                textInput('selComid', 'Zoom to COMID:', value="", width='50%'),
                br())
          ),
          #p(strong("Map Legend:")),
          #tags$ul(tags$li(strong(tags$span(style="color:navy","USGS Gauge"))),
          #tags$li(strong(tags$span(style="color:darkmagenta","GAGES-II Reference Gauge")))),
          fluidRow(
             column(3,
# LOG ON/OFF
                radioButtons("radioLog", label = "Plot Y Scale:",
                        choices = list("Standard" = "standard", "Log" = "log"), 
                        selected = "standard", inline=TRUE)),
             column(3,
# FCST PLOT TYPE
                radioButtons("radioPlotTyp", label="Forecast Plot Type:",
                        choices = list("By Forecast Time" = "fcsttime", "By Lead Time" = "leadtime"),
                        selected = "fcsttime", inline=TRUE)),

             column(3,
# MAP Area Type
                radioButtons("radioMapArea", label="MAP Area Type:",
                        choices = list("Contributing" = "contributing", "Intervening" = "intervening"),
                        selected = "contributing", inline=TRUE)),
             column(3,
# Accumulation ON/OFF for MAP
                radioButtons("radioAccTyp", label="MAP Display:",
                        choices = list("Accumulated" = "accum", "Not Accumulated" = "noAccum"),
                        selected = "noAccum", inline=TRUE))
             ),
# PLOT SOURCE DIRECTORY
          textInput("plotDir", "Plot Directory:", 
                    value=globalPlotDir,
                    width="80%")
          #p(verbatimTextOutput("fcstShortPath"))
          ),
# PLOTS
       column(6,
# TOP FRAME
             tabsetPanel(
               tabPanel("SHORT-RANGE",
                  fluidRow(
                    column(12,
                      #wellPanel(a(imageOutput("fcstShort", width="100%", height="50%"), target="_blank", href=verbatimTextOutput("fcstShortPath")))
                      wellPanel(imageOutput("fcstShort", width="100%", height="50%")),
                      p(verbatimTextOutput("fcstShortPath"))
                    )
                  )
               ),
               tabPanel("MED-RANGE",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("fcstMed", width="100%", height="50%")),
                      p(verbatimTextOutput("fcstMedPath"))
                    )
                  )
               ),
               tabPanel("LONG-RANGE",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("fcstLong", width="100%", height="50%")),
                      p(verbatimTextOutput("fcstLongPath"))                   
                    )
                  )               
               ),
               tabPanel("OPEN LOOP",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("analOL_top", width="100%", height="50%")),
                      p(verbatimTextOutput("analOLPath_top"))
                    )
                  )
               )
             )  
           )
    ),
  fluidRow(
     column(6,
           tabsetPanel(selected="RETROSPECTIVE",
               tabPanel("RETROSPECTIVE",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("analRetro5yr", width="100%", height="50%")),
                      p(verbatimTextOutput("analRetro5yrPath"))
                    )
                  )
               )
             )
          ),
     column(6,
           tabsetPanel(selected="OPEN LOOP",
               tabPanel("MAP-SHORT-RANGE",
                  fluidRow(
                    column(12,
                      #wellPanel(a(imageOutput("fcstShort", width="100%", height="50%"), target="_blank", href=verbatimTextOutput("fcstShortPath")))
                      wellPanel(imageOutput("MAPfcstShort", width="100%", height="50%")),
                      p(verbatimTextOutput("MAPfcstShortPath"))
                    )
                  )
               ),
               tabPanel("MAP-MED-RANGE",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("MAPfcstMed", width="100%", height="50%")),
                      p(verbatimTextOutput("MAPfcstMedPath"))
                    )
                  )
               ),
               tabPanel("MAP-LONG-RANGE",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("MAPfcstLong", width="100%", height="50%")),
                      p(verbatimTextOutput("MAPfcstLongPath"))
                    )
                  )
               ),
              tabPanel("MAP-OPEN LOOP",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("MAPanalOL", width="100%", height="50%")),
                      p(verbatimTextOutput("MAPanalOLPath"))
                    )
                  )
               ),
              tabPanel("OPEN LOOP",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("analOL", width="100%", height="50%")),
                      p(verbatimTextOutput("analOLPath"))
                    )
                  )
               )
             )
           )
    ) # THis is the end of the second fluidRow I added to see how it works
), # This is the end of the first tabPanel (MAP)

# MASTER TAB STATS
    tabPanel("STATISTICS",
       tabsetPanel(
          tabPanel("SHORT-RANGE",
             tabsetPanel(
                tabPanel("BY CYCLE",
                   fluidRow(column(12, h3("Correlation:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatShortCycleCorRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatShortCycleCorSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatShortCycleBiasRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatShortCycleBiasSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("RMSE:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatShortCycleRmseRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatShortCycleRmseSplit", width="100%", height="100%"))
                       )
                   ),
                   fluidRow(column(12, h3("Peak Flow:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatShortCycleQmaxRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatShortCycleQmaxSplit", width="100%", height="100%"))
                       )
                   ),
                   fluidRow(column(12, h3("Peak Flow Timing:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatShortCycleQmaxtimeRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatShortCycleQmaxtimeSplit", width="100%", height="100%"))
                       )
                   )
                 ),
                 tabPanel("BY LEAD TIME",
                   fluidRow(column(12, h3("Correlation:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatShortLeadCorRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatShortLeadCorSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatShortLeadCorRefRFC", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatShortLeadBiasRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatShortLeadBiasSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatShortLeadBiasRefRFC", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("RMSE:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatShortLeadRmseRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatShortLeadRmseSplit", width="100%", height="100%"))
                       )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatShortLeadRmseRefRFC", width="100%", height="100%"))
                      )
                   )
                 ),
                tabPanel("MAPS",
                   fluidRow(column(12, h3("Correlation:"))),
                   fluidRow(
                      column(7,
                         wellPanel(imageOutput("mapStatShortCycleCorRef", width="100%", height="100%"))
                      ),
                      column(5,
                         wellPanel(imageOutput("histStatShortCycleCorRef", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(7,
                         wellPanel(imageOutput("mapStatShortCycleBiasRef", width="100%", height="100%"))
                      ),
                      column(5,
                         wellPanel(imageOutput("histStatShortCycleBiasRef", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("RMSE:"))),
                   fluidRow(
                       column(7,
                         wellPanel(imageOutput("mapStatShortCycleRmseRef", width="100%", height="100%"))
                       ),
                       column(5,
                         wellPanel(imageOutput("histStatShortCycleRmseRef", width="100%", height="100%"))
                       )
                   ),
                   fluidRow(column(12, h3("Peak Flow:"))),
                   fluidRow(
                       column(7,
                         wellPanel(imageOutput("mapStatShortCycleQmaxRef", width="100%", height="100%"))
                       ),
                       column(5,
                         wellPanel(imageOutput("histStatShortCycleQmaxRef", width="100%", height="100%"))
                       )
                   ),
                   fluidRow(column(12, h3("Peak Flow Timing:"))),
                   fluidRow(
                       column(7,
                         wellPanel(imageOutput("mapStatShortCycleQmaxtimeRef", width="100%", height="100%"))
                       ),
                       column(5,
                         wellPanel(imageOutput("histStatShortCycleQmaxtimeRef", width="100%", height="100%"))
                       )
                   )
                 )
               )
           ),
           tabPanel("MEDIUM-RANGE STATS",
             tabsetPanel(
                tabPanel("BY CYCLE",
                   fluidRow(column(12, h3("Correlation:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatMedCycleCorRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatMedCycleCorSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatMedCycleBiasRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatMedCycleBiasSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("RMSE:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatMedCycleRmseRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatMedCycleRmseSplit", width="100%", height="100%"))
                       )
                   )
                 ),
                 tabPanel("BY LEAD TIME",
                   fluidRow(column(12, h3("Correlation:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatMedLeadCorRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatMedLeadCorSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatMedLeadCorRefRFC", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatMedLeadBiasRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatMedLeadBiasSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatMedLeadBiasRefRFC", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("RMSE:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatMedLeadRmseRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatMedLeadRmseSplit", width="100%", height="100%"))
                       )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatMedLeadRmseRefRFC", width="100%", height="100%"))
                      )
                   )
                 ),
                tabPanel("MAPS",
                   fluidRow(column(12, h3("Correlation:"))),
                   fluidRow(
                      column(7,
                         wellPanel(imageOutput("mapStatMedCycleCorRef", width="100%", height="100%"))
                      ),
                      column(5,
                         wellPanel(imageOutput("histStatMedCycleCorRef", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(7,
                         wellPanel(imageOutput("mapStatMedCycleBiasRef", width="100%", height="100%"))
                      ),
                      column(5,
                         wellPanel(imageOutput("histStatMedCycleBiasRef", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("RMSE:"))),
                   fluidRow(
                       column(7,
                         wellPanel(imageOutput("mapStatMedCycleRmseRef", width="100%", height="100%"))
                       ),
                       column(5,
                         wellPanel(imageOutput("histStatMedCycleRmseRef", width="100%", height="100%"))
                       )
                   )
                 )

               )
           ),
           tabPanel("LONG-RANGE STATS",
             tabsetPanel(
                tabPanel("BY CYCLE",
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatLongCycleBiasRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatLongCycleBiasSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Total Volume Error:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatLongCycleVolerrRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatLongCycleVolerrSplit", width="100%", height="100%"))
                       )
                   )
                 ),
                 tabPanel("BY LEAD TIME",
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(6,
                         wellPanel(imageOutput("plotStatLongLeadBiasRef", width="100%", height="100%"))
                      ),
                      column(6,
                         wellPanel(imageOutput("plotStatLongLeadBiasSplit", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatLongLeadBiasRefRFC", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Volume Error:"))),
                   fluidRow(
                       column(6,
                         wellPanel(imageOutput("plotStatLongLeadVolerrRef", width="100%", height="100%"))
                       ),
                       column(6,
                         wellPanel(imageOutput("plotStatLongLeadVolerrSplit", width="100%", height="100%"))
                       )
                   ),
                   fluidRow(
                      column(12,
                         wellPanel(imageOutput("plotStatLongLeadVolerrRefRFC", width="100%", height="100%"))
                      )
                   )
                 ),
                tabPanel("MAPS",
                   fluidRow(column(12, h3("Bias:"))),
                   fluidRow(
                      column(7,
                         wellPanel(imageOutput("mapStatLongCycleBiasRef", width="100%", height="100%"))
                      ),
                      column(5,
                         wellPanel(imageOutput("histStatLongCycleBiasRef", width="100%", height="100%"))
                      )
                   ),
                   fluidRow(column(12, h3("Total Volume Error:"))),
                   fluidRow(
                       column(7,
                         wellPanel(imageOutput("mapStatLongCycleVolerrRef", width="100%", height="100%"))
                       ),
                       column(5,
                         wellPanel(imageOutput("histStatLongCycleVolerrRef", width="100%", height="100%"))
                       ) 
                   )   
                 )

               )
           )
         )

    )
    )  
  
  )


#****************************************************************************************************
#                           SERVER
#****************************************************************************************************

server <- function(input,output){
  
#  output$valLog <- renderText({input$radioLog})
  
  output$map <- renderLeaflet({
#    leaflet(subsetg2) %>% addTiles() %>% 
#      addCircleMarkers(~LNG_GAGE, ~LAT_GAGE, popup=~STAID, layerId = ~STAID, clusterId="g2", group = "Gages II", clusterOptions = markerClusterOptions(),color="red") %>%
    leaflet(masterMeta) %>% addTiles() %>% setView(lng=-96, lat=36, zoom=4) %>%
  addWMSTiles(
#    "http://mesonet.agron.iastate.edu/cgi-bin/wms/nexrad/n0r.cgi",
#    layers = "nexrad-n0r-900913",
    "http://mesonet.agron.iastate.edu/cgi-bin/wms/us/wwa.cgi",
    layers = "wwa",
#    "http://idpgis.ncep.noaa.gov/arcgis/services/NWS_Forecasts_Guidance_Warnings/watch_warn_adv/MapServer/WmsServer", 
#    layers = "1",
#    "http://mesonet.agron.iastate.edu/cgi-bin/wms/us/mrms_nn.cgi", 
#    layers = "mrms_p24h",  
    options = WMSTileOptions(format = "image/png", transparent = TRUE),
    attribution = "Weather data Â© 2012 IEM Nexrad"
  ) %>%
      addCircleMarkers(data=masterMeta, ~dec_long_va, ~dec_lat_va, popup=~paste0(site_no, "\n", station_nm), 
                       radius=~sizefact, color = ~palfun(inGagesIIRef), 
                       stroke=FALSE, fillOpacity=0.5, layerId = ~site_no,
                       clusterId="gages", group = "USGS Gages", 
                       clusterOptions = markerClusterOptions(maxClusterRadius=40)) %>%
      addLegend(position='bottomleft', pal = palfun, values = ~inGagesIIRef,
                title = "Gauges:", opacity = 1) %>%
#      addMarkers(~dec_long_va, ~dec_lat_va, popup=~site_no, 
#                 layerId = ~site_no, clusterId="gages", 
#                 group = "USGS Gages", 
#                 clusterOptions = markerClusterOptions(maxClusterRadius=40), 
#                 icon = ~gageIcons[group]) %>%
    addLayersControl(overlayGroups = c("USGS Gages"))
  })

   GatherPlots <- function(n) {
        # TOP
        output$fcstShort <- renderImage({
          path <- plotType(n, "short", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
        output$fcstShortPath <- renderText({plotType(n, "short", input$radioLog, input$plotDir, input$radioPlotTyp)})
        output$fcstMed <- renderImage({
          path <- plotType(n, "medium", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$fcstMedPath <- renderText({plotType(n, "medium", input$radioLog, input$plotDir, input$radioPlotTyp)})
        output$fcstLong <- renderImage({
          path <- plotType(n, "long", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$fcstLongPath <- renderText({plotType(n, "long", input$radioLog, input$plotDir, input$radioPlotTyp)})
        # BOTTOM
        output$analOL <- renderImage({
          path <- plotType(n, "ol", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
        output$analOLPath <- renderText({plotType(n, "ol", input$radioLog, input$plotDir, input$radioPlotTyp)})
        output$analRetro5yr <- renderImage({
          path <- plotType(n, "retro", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
        output$analRetro5yrPath <- renderText({plotType(n, "retro", input$radioLog, input$plotDir, input$radioPlotTyp)})


        # Gather all the plot info Arezoo added (MAP plots, and a duplicate of open_loo

        output$MAPfcstShort <- renderImage({
          path <- MapPlotType(n, "mapshort", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
        output$MAPfcstShortPath <- renderText({MapPlotType(n, "mapshort", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)})
        output$MAPfcstMed <- renderImage({
          path <- MapPlotType(n, "mapmedium", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$MAPfcstMedPath <- renderText({MapPlotType(n, "mapmedium", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)})
        output$MAPfcstLong <- renderImage({
          path <- MapPlotType(n, "maplong", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$MAPfcstLongPath <- renderText({MapPlotType(n, "maplong", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)})

        output$MAPanalOL <- renderImage({
          path <- MapPlotType(n, "mapol", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
        output$MAPanalOLPath <- renderText({MapPlotType(n, "mapol", input$radioLog, input$plotDir, input$radioPlotTyp, input$radioMapArea, input$radioAccTyp)})

        output$analOL_top <- renderImage({
          path <- plotType(n, "ol", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
        output$analOLPath_top <- renderText({plotType(n, "ol", input$radioLog, input$plotDir, input$radioPlotTyp)})

       
     }

  # GAGE ID SELECTION
  observe({
     req(input$selGage)
     selGageInfo <- masterMeta[masterMeta$site_no == input$selGage,]
     isolate({
         leafletProxy('map') %>%
                setView(lng = selGageInfo$dec_long_va, lat = selGageInfo$dec_lat_va, zoom = 12)
     })
     GatherPlots(selGageInfo$site_no)
  })  

  # COMID SELECTION
  observe({
     req(input$selComid)
     selGageInfo <- masterMeta[masterMeta$COMID == input$selComid,]
     isolate({
         leafletProxy('map') %>%
                setView(lng = selGageInfo$dec_long_va, lat = selGageInfo$dec_lat_va, zoom = 12)
     })
     GatherPlots(selGageInfo$site_no)
  })

  # MAP SELECTION
  observeEvent(input$map_marker_click,
    {
    stnclick <- input$map_marker_click
    stnclusterId <- stnclick$clusterId
    ##print(stnclusterId)
    
  if (stnclusterId == "gages") {
     GatherPlots(stnclick$id)
      }
    })

output$legendTable <- renderUI({
  # Create a Bootstrap-styled table
  tags$table(class = "table-condensed",
             tags$thead(tags$tr(
               tags$th("Color"),
               tags$th("Gauge Type")
             )),
             tags$tbody(
               tags$tr(
                 tags$td(span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   "navy"
                 ))),
                 tags$td("USGS Gauge")
               ),
               tags$tr(
                 tags$td(span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   "darkmagenta"
                 ))),
                 tags$td("GAGES-II Reference Gauge")
               )
             )
  )
})

# STATS PLOTS

# Short-Range, by Cycle

# Histograms

output$plotStatShortCycleCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleCorSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_cor_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleBiasSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_bias_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleRmseSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_rmse_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleQmaxRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_qmax_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleQmaxSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_qmax_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleQmaxtimeRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_qmaxtime_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortCycleQmaxtimeSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_cycle_qmaxtime_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

# Maps

output$mapStatShortCycleCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/map_short_cycle_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatShortCycleCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/hist_short_cycle_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)

output$mapStatShortCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/map_short_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatShortCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/hist_short_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)

output$mapStatShortCycleRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/map_short_cycle_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatShortCycleRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/hist_short_cycle_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)

output$mapStatShortCycleQmaxRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/map_short_cycle_qmax_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatShortCycleQmaxRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/hist_short_cycle_qmax_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)

output$mapStatShortCycleQmaxtimeRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/map_short_cycle_qmaxtime_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatShortCycleQmaxtimeRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/hist_short_cycle_qmaxtime_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)


# Short-Range, by Lead Time

output$plotStatShortLeadCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortLeadCorSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_cor_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortLeadCorRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_cor_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)

output$plotStatShortLeadBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortLeadBiasSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_bias_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortLeadBiasRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_bias_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)

output$plotStatShortLeadRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortLeadRmseSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_rmse_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatShortLeadRmseRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/short/boxplot_short_leadtime_rmse_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)


# Medium-Range, by Cycle

# Boxplots

output$plotStatMedCycleCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_cycle_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedCycleCorSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_cycle_cor_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedCycleBiasSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_cycle_bias_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedCycleRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_cycle_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedCycleRmseSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_cycle_rmse_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

# Maps

output$mapStatMedCycleCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/map_med_cycle_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatMedCycleCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/hist_med_cycle_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)

output$mapStatMedCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/map_med_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatMedCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/hist_med_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)

output$mapStatMedCycleRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/map_med_cycle_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatMedCycleRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/hist_med_cycle_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)


# Medium-Range, by Lead Time

output$plotStatMedLeadCorRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_cor_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedLeadCorSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_cor_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedLeadCorRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_cor_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)

output$plotStatMedLeadBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedLeadBiasSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_bias_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedLeadBiasRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_bias_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)

output$plotStatMedLeadRmseRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_rmse_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedLeadRmseSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_rmse_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatMedLeadRmseRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/medium/boxplot_med_leadtime_rmse_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)

# Long-Range, by Cycle

# Boxplots

output$plotStatLongCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatLongCycleBiasSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_cycle_bias_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatLongCycleVolerrRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_cycle_volerr_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatLongCycleVolerrSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_cycle_volerr_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

# Maps

output$mapStatLongCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/map_long_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatLongCycleBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/hist_long_cycle_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)

output$mapStatLongCycleVolerrRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/map_long_cycle_volerr_REFONLY.png")
          list(src=path, contentType = "image/png", width=1200, height=900)}, deleteFile = FALSE)

output$histStatLongCycleVolerrRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/hist_long_cycle_volerr_REFONLY.png")
          list(src=path, contentType = "image/png", width=900, height=600)}, deleteFile = FALSE)


# Long-Range, by Lead Time

output$plotStatLongLeadBiasRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_leadtime10d_bias_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatLongLeadBiasSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_leadtime10d_bias_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatLongLeadBiasRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_leadtime10d_bias_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)

output$plotStatLongLeadVolerrRef <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_leadtime10d_volerr_REFONLY.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatLongLeadVolerrSplit <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_leadtime10d_volerr_SPLIT.png")
          list(src=path, contentType = "image/png", width=1000, height=550)}, deleteFile = FALSE)

output$plotStatLongLeadVolerrRefRFC <- renderImage({
          path <- paste0(input$plotDir, "/STATS/long/boxplot_long_leadtime10d_volerr_REFONLY_RFC.png")
          list(src=path, contentType = "image/png", width=1500, height=825)}, deleteFile = FALSE)



}


#****************************************************************************************************
#                           CONFIG
#****************************************************************************************************

options(shiny.host = "0.0.0.0")
shinyApp(ui = ui, server = server)



