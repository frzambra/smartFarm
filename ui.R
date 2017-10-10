library(shinydashboard)
library(highcharter)
library(leaflet)
library(rgdal)

dashboardPage(
  dashboardHeader(title = "SmartFarm"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Seleccion", tabName = "seleccionMapa"),
      menuItem("Estado vegetacional", tabName = "muestraIndices"),
      menuItem("Clima", tabName = "climatica"),
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("seleccionMapa",
              box(
                width = 8, status = "info", solidHeader = TRUE,
                title = "Seleccione área de interes",
              leafletOutput("mapSelect", width="100%", height=500))
      ),
      tabItem("muestraIndices",
              box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "Indice vegetacional área de interes",
                  leafletOutput("mapaIndices", width="100%", height=500)
                ),
              box(
                width = 2, status = "info", solidHeader = TRUE,
                title = "Seleccionar",
                selectInput("VIindex", "Índice:",
                            c("NDVI" = "ndvi",
                              "NDWI" = "ndwi",
                              "RE-NDVI" = "rendvi"))
              )
      ),
      tabItem("climatica",
              tags$iframe(
                seamless = "seamless", 
                src = "https://forecast.io/embed/#lat=-36.3583&lon=-71.0603&name=Downtown Chillan", 
                height = 300, width = 800)
              ),
      tabItem("dashboard",
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count")
               ),
              fluidRow(
                box(
                  width = 4, status = "info", solidHeader = TRUE,
                  title = "Sectores identificado",
                  leafletOutput("mapaSectores", width="100%", height=600)
                ),
                fluidRow(
                  box(
                    width = 7, status = "info",solidHeader = TRUE,
                    title = "Valores indices por sector",
                    highchartOutput("plotHC",height = "300px")
                ),
                fluidRow(
                  box(
                    width = 3, status = "info",solidHeader = TRUE,
                    title = "Superficie por sector",
                    highchartOutput("pieHC",height='200px')
                )))
              )
      )
    )
  )
)