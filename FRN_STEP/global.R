library(shiny)
library(leaflet)
library(rwrfhydro)
library(png)

load("data/masterStrMetadata.Rdata")

# Plot Specs
globalPlotDir <- '/d7/adugger/WRF_Hydro/InspGadget/FRN_STEP/plots/'
prefList <- list(now="/FCST/nowcast/",
                 noradar="/FCST/wrfnoradar/",
                 radar="/FCST/wrfradar/",
                 analysis="/ANALYSIS/openloop/",
                 retro="/RETRO/5yr/",
                 test="/ANALYSIS/test/")
suff1List <- list(now="_nowcast_range_plot",
                 noradar="_wrfnoradar_range_plot",
                 radar="_wrfradar_range_plot",
                 analysis="_open_loop-assimilation_plot",
                 retro="_5yr_retro",
                 test="_test")
suff2List <- list(fcsttime=".byFcstTime",
                 leadtime=".byLeadTime",
                 none="")
suff3List <- list(standard=".Linear.png",
                 log=".Log.png")

# Graphics
gageIcons <- iconList(red = makeIcon("marker_darkred.png", iconWidth = 24, iconHeight =32),
                      blue = makeIcon("marker_darkblue.png", iconWidth = 24, iconHeight =32))

palfun <- colorFactor(palette=c("navy", "darkmagenta"), domain=c(TRUE, FALSE))


# Setup gauge points
masterMeta <- masterMeta[order(masterMeta$lon_dd),]
#gagesDf <- plyr::mutate(masterMeta, group = factor(masterMeta$inGagesIIRef, labels = c("red", "blue")))
masterMeta$sizefact <- cut(masterMeta$qmean, breaks=c(0, 5, 20, 50, 100, 20000), labels=c(4,6,8,10,12))
masterMeta$sizefact <- as.character(masterMeta$sizefact)
masterMeta$sizefact <- ifelse(is.na(masterMeta$sizefact), "4", masterMeta$sizefact)
masterMeta$sizefact <- factor(masterMeta$sizefact, levels=c(4,6,8,10,12))
masterMeta$sizefact <- as.integer(as.character(masterMeta$sizefact))

# Function to get plot path
plotType <- function(x, plottype, axistype, plotdir, fcstplottyp) {
  if (plottype == "retro") fcstplottyp <- "none"
  if (plottype == "analysis") fcstplottyp <- "none"
  if (plottype == "test") fcstplottyp <- "none"
  axistype_full <- paste0(axistype, "_", fcstplottyp)
  switch(axistype_full,
         "log_fcsttime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "standard_fcsttime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "log_leadtime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "standard_leadtime" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "log_none" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]]),
         "standard_none" = paste0(plotdir, prefList[[plottype]], x, suff1List[[plottype]], suff2List[[fcstplottyp]], suff3List[[axistype]])
         )
}


#****************************************************************************************************
#                           UI
#****************************************************************************************************

ui <- fluidPage(
    theme = "bootstrap.css",
    
    titlePanel(tags$h1("HydroInspector(Gadget!): Front Range STEP",  
        style = "font-family: 'Lato', 'Helvetica Neue', Helvetica, Arial, sans-serif;
        color: #fff; text-align: left;
        background-color: #4372AA;
        padding: 20px"), windowTitle="HydroInspectorGadget"),
    
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
             column(9,
# FCST PLOT TYPE
                radioButtons("radioPlotTyp", label="Forecast Plot Type:", 
                        choices = list("By Forecast Time" = "fcsttime", "By Lead Time" = "leadtime"),
                        selected = "fcsttime", inline=TRUE))
             ),
# PLOT SOURCE DIRECTORY
          textInput("plotDir", "Plot Directory:", 
                    value=globalPlotDir,
                    width="80%")
          ),
# PLOTS
       column(6,
# TOP FRAME
             tabsetPanel(selected="NOWCAST",
               tabPanel("NOWCAST",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("fcstNow", width="100%", height="50%")),
                      p(verbatimTextOutput("fcstNowPath"))
                    )
                  )
               ),
               tabPanel("WRF_NORADAR",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("fcstNoRadar", width="100%", height="50%")),
                      p(verbatimTextOutput("fcstNoRadarPath"))
                    )
                  )
               ),
               tabPanel("WRF_RADAR",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("fcstRadar", width="100%", height="50%")),
                      p(verbatimTextOutput("fcstRadarPath"))                   
                    )
                  )
               )
             ),
# BOTTOM FRAME
             tabsetPanel(selected="ANALYSIS",
               tabPanel("ANALYSIS",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("analysisRoll", width="100%", height="50%")),
                      p(verbatimTextOutput("analysisPath"))
                    )
                  )
               ),
               tabPanel("RETROSPECTIVE",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("retro5yr", width="100%", height="50%")),
                      p(verbatimTextOutput("retroPath"))
                    )
                  )
               ),
               tabPanel("TEST",
                  fluidRow(
                    column(12,
                      wellPanel(imageOutput("analysisTest", width="100%", height="50%")),
                      p(verbatimTextOutput("analysisTestPath"))
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
    leaflet(masterMeta) %>% addTiles() %>% setView(lng=-104.87, lat=39.52, zoom=8) %>%
      addCircleMarkers(data=masterMeta, ~lon_dd, ~lat_dd, popup=~paste0(site_no, "\n", site_name), 
                       radius=~sizefact, color = ~palfun(allRef), 
                       stroke=FALSE, fillOpacity=0.5, layerId = ~site_no,
                       clusterId="gages", group = "Streamflow Gages", 
                       clusterOptions = markerClusterOptions(maxClusterRadius=40)) %>%
      addLegend(position='bottomleft', pal = palfun, values = ~allRef,
                title = "Reference Gage:", opacity = 1) %>%
#      addMarkers(~dec_long_va, ~dec_lat_va, popup=~site_no, 
#                 layerId = ~site_no, clusterId="gages", 
#                 group = "USGS Gages", 
#                 clusterOptions = markerClusterOptions(maxClusterRadius=40), 
#                 icon = ~gageIcons[group]) %>%
    addLayersControl(overlayGroups = c("Streamflow Gages"))
  })

  GatherPlots <- function(n) {
        # TOP
        output$fcstNow <- renderImage({
          path <- plotType(n, "now", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
        output$fcstNowPath <- renderText({plotType(n, "now", input$radioLog, input$plotDir, input$radioPlotTyp)})
        output$fcstNoRadar <- renderImage({
          path <- plotType(n, "noradar", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$fcstNoRadarPath <- renderText({plotType(n, "noradar", input$radioLog, input$plotDir, input$radioPlotTyp)})
        output$fcstRadar <- renderImage({
          path <- plotType(n, "radar", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$fcstRadarPath <- renderText({plotType(n, "radar", input$radioLog, input$plotDir, input$radioPlotTyp)})
        # BOTTOM
        output$analysisRoll <- renderImage({
          path <- plotType(n, "analysis", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$analysisPath <- renderText({plotType(n, "analysis", input$radioLog, input$plotDir, input$radioPlotTyp)})
        output$retro5yr <- renderImage({
          path <- plotType(n, "retro", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$retroPath <- renderText({plotType(n, "retro", input$radioLog, input$plotDir, input$radioPlotTyp)})
        output$analysisTest <- renderImage({
          path <- plotType(n, "test", input$radioLog, input$plotDir, input$radioPlotTyp)
          list(src=path,contentType = "image/png")}, deleteFile = FALSE)
          output$analysisTestPath <- renderText({plotType(n, "test", input$radioLog, input$plotDir, input$radioPlotTyp)})
  }

  # GAGE ID SELECTION
  observe({
     req(input$selGage)
     selGageInfo <- masterMeta[masterMeta$site_no == input$selGage,]
     isolate({
         leafletProxy('map') %>%
                setView(lng = selGageInfo$lon_dd, lat = selGageInfo$lat_dd, zoom = 12)
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
  
  # MAP CLICK SELECTION
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
               tags$th("Gage Type")
             )),
             tags$tbody(
               tags$tr(
                 tags$td(span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   "navy"
                 ))),
                 tags$td("Gage")
               ),
               tags$tr(
                 tags$td(span(style = sprintf(
                   "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
                   "darkmagenta"
                 ))),
                 tags$td("Reference Gage")
               )
             )
  )
})


}


#****************************************************************************************************
#                           CONFIG
#****************************************************************************************************

options(shiny.host = "0.0.0.0")
shinyApp(ui = ui, server = server)









