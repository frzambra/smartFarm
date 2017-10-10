library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(highcharter)

function(input, output, session) {
  
  output$mapSelect <- renderLeaflet({
    leaflet() %>%
      setView(-71.95883291,-36.53359195,14) %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',group='Satellite') %>%
      addDrawToolbar(
        targetGroup='draw',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE))
  })
  
  output$mapaIndices <- renderLeaflet({
    load(file='RData/raster_VIs_20161220.Rdata')
    
    op <- switch(input$VIindex,
                 ndvi=1,
                 ndwi=2,
                 rendvi=3)
    
    names <- c('NDVI','NDWI','RE-NDVI')
    img <- raster::subset(vis_R,c(16:7,20))
    
    colors <- rev(colorRampPalette(c("darkgreen","yellow", "darkred"))(30))
    colorsPal <- colorNumeric(colors, values(img[[op]]), na.color = "transparent")
    
    
    leaflet() %>% 
      # addTiles(group='Base'
      #         ) %>%
      #       addProviderTiles("OpenTopoMap", group = "DEM") %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',group='Satellite') %>%
      addRasterImage(img[[op]], colors = colorsPal, opacity = 0.8,group=names[op]) %>%
       addLegend(pal = colorsPal, values = values(img[[op]]),
         title = names[op]) %>%
      addLayersControl(
        baseGroups = c("Satellite"),
        overlayGroups = c(names[op]),
        position="topright",
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$mapaSectores <- renderLeaflet({
    load(file='RData/RF_classes.Rdata')
    pal <- colorRampPalette(brewer.pal(11,"Spectral"))(5)
    leaflet() %>% 
      # addTiles(group='Base'
      #         ) %>%
      #       addProviderTiles("OpenTopoMap", group = "DEM") %>%
      addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',group='Satellite') %>%
      addRasterImage(rf_raster, colors=pal, opacity = 0.8) %>%
      addLegend(colors=pal,labels=1:5) 
  })
  
  output$plotHC <- renderHighchart({
    load(file='RData/data.frame_sectores.Rdata')
    dataG <- data %>% gather(indice,value,-sector) %>% mutate(value=round(value,1))
    hchart(dataG, "column", hcaes(x = sector, y = value, group = indice))
  })
  
  output$pieHC <- renderHighchart({
    pal <- colorRampPalette(brewer.pal(11,"Spectral"))(5)
    load(file='RData/RF_classes.Rdata')
    df<-data.frame(freq(rf_raster)[1:5,1:2])
    names(df) <- c('sector','value')
    df$perc <- round(df$value*100/sum(df$value))
    highchart() %>%
      hc_chart(type = 'pie',
               polar = TRUE) %>% 
      hc_add_series(df$perc, name = "Área") %>% 
      hc_add_series_labels_values(paste0('Sector ',df$sector),df$perc, name = "Área") %>% 
      hc_plotOptions(
        pie = list(
          colors=pal,
          colorByPoint = TRUE,
          dataLabels = list(enabled = FALSE)
        )) %>% 
      hc_tooltip(pointFormat = "{point.y}%")
    
  })
  # pkgStream is a reactive expression that represents a stream of
  # new package download data; up to once a second it may return a
  # data frame of new downloads since the last update.
  output$rate <- renderValueBox({
    valueBox(
      value = 5,
      subtitle = "Número de sectores",
      icon = icon("area-chart"),
      color = "aqua"
    )
  })
  
  output$count <- renderValueBox({
    valueBox(
      value = 319.6,
      subtitle = "Superficie total (ha)",
      icon = icon("download")
    )
  })
  
  output$users <- renderValueBox({
    valueBox(
      usrCount(),
      "Unique users",
      icon = icon("users")
    )
  })
}