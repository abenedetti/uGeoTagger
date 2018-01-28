########################
# uGeoTagger           #
# by Alessio Benedetti #
# ui.R file            #
########################

# load libraries
library(leaflet)
library(sweetalertR)

# load shinyjs parameter used for sweet alerts
jscode <- "shinyjs.swal = function(params) { swal.apply(this, params); }"

# set app status
wip <- "NO"

# LOAD SHINYUI
shinyUI(fluidPage(
                  
 #load google analytics
 tags$head(includeScript("www/doc/google-analytics-uGeoTagger.js")),
 
 shinyjs::useShinyjs(),
 shinyjs::extendShinyjs(text = jscode),
 
 
 if (wip == "YES") {includeMarkdown("www/md/wip.md")}
  else
 {
 navbarPage(
  theme = "doc/style.css",
  footer = includeMarkdown("www/md/footer.md"),
  windowTitle = "uGeoTagger",
  title = 'uGeoTagger',
  tabPanel('Maps',
           leafletOutput("map"),
           absolutePanel(id = "flyingBoard", fixed = TRUE, draggable = TRUE, top = 440, left = 48, right = "auto", bottom = "auto", width = "auto", height = "auto",
                         h2("Tag uTester!"),h6(em("All fields are mandatory")),br(),
                         textInput("slug", "Slug:", value = ""),
                         radioButtons("position", "Position:", c("Location" = "loc","Coordinates (decimal)" = "coord"), inline = TRUE, selected = "loc"),
                         div(id = "location",
                          htmlOutput("countriesMap"),
                          htmlOutput("citiesMap")
                         ),
                         shinyjs::hidden(
                          div(id = "coordinates",
                           numericInput("lat", "Latitude:",""),
                           numericInput("long", "Longitude:","")
                          )
                         ),
                         shinyjs::hidden(
                          div(id = "checkLat",
                           h6("Latitude should be between -90 and 90!")
                          )
                         ),
                         shinyjs::hidden(
                          div(id = "checkLong",
                           h6("Longitude should be between -180 and 180!")
                          )
                         ),
                         tags$head(tags$style(HTML('#submit{background-color:#f0e7e7;}'))),
                         column(6, align="center", actionButton("submit", "Submit")),
                         sweetalert(),
                         tags$head(tags$style(HTML('#reset{background-color:#f0e7e7;}'))),
                         column(6, align="center", actionButton("reset", "Reset"))
                         )
          ),
  
  tabPanel('Charts & Stats',
           fluidPage(
            
            fluidRow(
             column(6, offset=2, selectInput("chart1Select","", c("Regions","Countries","Cities"))),
             column(4, selectInput("chart2Select","", c("Localization","Usability","Security","Load","Functional")))
            ),
            
            fluidRow(
             column(6, "", htmlOutput('chart1Chart')),
             column(6, "", htmlOutput('chart2Chart'))
            ),
            
            fluidRow(
             column(12, "", htmlOutput('chart3Chart'))
            )
            
           )
          
          ),
  
  tabPanel("?",
  
   navlistPanel(widths = c(2, 10),
    "User guide",
    tabPanel("Map",includeMarkdown("www/md/map.md")),
    tabPanel("uTester details",includeMarkdown("www/md/utester_details.md")),
    tabPanel("Register/Update uTester",includeMarkdown("www/md/reg_upd_uTester.md")),
    tabPanel("Charts & Stats",includeMarkdown("www/md/charts_stats.md")),
    tabPanel("Versioning",includeMarkdown("www/md/versioning.md"))
   )
  
  )
  
 )
 }
 ))