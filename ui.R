library(ggplot2)
library(shinydashboard)
library(shiny)
library(highcharter)
library(leaflet)
library("viridisLite")
library("dplyr")
library("DT")


sf_inmates <- read.csv("sf_inmates.csv")
sf_offense <- read.csv("sf_offense.csv")
sf_offense$inmates <- as.integer(sf_offense$inmates)
sf_offense$inmates_pct <- as.numeric(sf_offense$inmates_pct)
sf_convicted <- read.csv("sf_convicted.csv")
sf_convicted$inmates <- as.integer(sf_convicted$inmates)
sf_convicted$inmates_pct <- as.numeric(sf_convicted$inmates_pct)
sf_gender <- read.csv("sf_gender.csv")
sf_gender$inmates <- as.numeric(sf_gender$inmates)
sf_gender$inmates_pct <- as.numeric(sf_gender$inmates_pct)
inmates_trend <- read.csv("inmates_trend.csv")
heatmap_data <- read.csv("heatmap_state_data.csv")
char_trend <- read.csv("char_trend.csv")
char_trend$year <- as.character(char_trend$year)
staff_trend <- read.csv("staff_trend.csv")
facility_size_data <- read.csv("facility_size_data.csv")

header <- dashboardHeader(title='Jails in Indian Country')

sidebar <- dashboardSidebar(
  
  sidebarMenu(id="menu1",
              
              menuItem("Overview", tabName = "Overview", icon = icon("line-chart")),
              menuItem("State & Facility", tabName = "S_F_Summary", icon = icon("dashboard")),
              menuItem("Comparison", tabName = "Comparison", icon = icon("globe", lib = "glyphicon")),
              menuItem("Glossary", tabName = "Glossary", icon = icon("th-list", lib = "glyphicon"))
              
  ),
  ## All views common filter!!
  
  conditionalPanel(
    condition = "( input.menu1 == 'S_F_Summary')"  ,
    
    htmlOutput("state_selector"),
    htmlOutput("facility_selector"),
    uiOutput("dropdown_ui"),
    hr()
  )
  
  ## View 0, only filter!
  
  #   conditionalPanel(
  #     condition = "(input.view_sd == '0')" ,
  #     hr()
  #   )
  
  #  conditionalPanel(
  #     condition = "( input.menu1 == 'Comparison')"  ,
  #     
  #     htmlOutput("type_selector"),
  #     htmlOutput("value_selector"),
  #     uiOutput("dropdown_ui"),
  #     hr()
  #   )
) 



body <- dashboardBody(
  
  #tags$head(tags$style(HTML('.small-box {height: 30px;} .small-box-icon {height: 30px; line-height: 30px;} .small-box-content {padding-top: 0px; padding-bottom: 0px;}'))),
  #tags$head(tags$style(HTML(".small-box {height: 30px}"))),
  tabItems(
    tabItem("S_F_Summary",
            fluidPage(
              fluidRow(
                infoBoxOutput("inmates", width=3),
                infoBoxOutput("adp", width=3),
                infoBoxOutput("peak_pop", width=3),
                infoBoxOutput("rated_cap", width=3)
              ),
              tags$head(tags$style(HTML("
                                        div.box-header {
                                        
                                        text-align: center;
                                        }
                                        "))),
              column(8, box(width=NULL, title = strong("Offense Type"), solidHeader = F, 
                            status = "primary", highchartOutput("offensePlot", height=430)
              )),
              
              column(4, 
                     box(width=NULL, title = strong("Conviction Status"), solidHeader = F, 
                         status = "success", highchartOutput("convictPlot", height=150))
                     ,box(width=NULL, title = strong("Demographics"), solidHeader = F, 
                          status = "success", highchartOutput("genderPlot", height=200)
                          #valueBoxOutput("adultpct",width=12), 
                          
                     ))
              )
            
            ),
    tabItem("Overview",
            fluidPage(
              fluidRow(
                box(width=6, title = strong("Inmates"), solidHeader = F, status = "primary",
                    #                     tags$head(
                    #                       tags$style(type="text/css", "label.control-label, .selectize-control.single{ display: table-cell; text-align: center; vertical-align: middle; } .form-group { display: table-row;}")
                    #                     ),
                    column(4, h5(strong("Type of inmates:"), align='center')),
                    column(6, selectInput('type_of_inmates',NULL,
                                          choices=c('MidYear','ADP'),width='80%')),
                    highchartOutput("inmates_trend",height=260),height=350),
                box(width=6, title = strong("Operating Facilities"), solidHeader = F, status = "primary",
                    column(4, h5(strong("Type of inmates:"), align='center')),
                    column(6, selectInput('type_of_inmates_fac',NULL,
                                          choices=c('MidYear','ADP'),width='80%')),
                    highchartOutput("facility_trend",height=260),height=350)
                
              ),
              fluidRow(
                box(width=6, title = strong("Characteristics"), solidHeader = F, status = "success", 
                    column(4, h5(strong("Characteristic Type:"), align='center')),
                    column(6, htmlOutput("char_selector")),
                    highchartOutput("char_trend",height=260), height=350),
                box(width=6, title = strong("Job Function"), solidHeader = F, status = "success", 
                    column(4, h5(strong("Job Function:"), align='center')),
                    column(6, htmlOutput("job_selector")),
                    highchartOutput("staff_trend",height=260), height=350)
              ),
              fluidRow(
                box(width=12, title = strong("Facility Size Distribution"), solidHeader = F, status = "primary", 
                    dataTableOutput('fac_size_table'), height=350)
              )
            )
    ),
    tabItem("Comparison",
            fluidPage(
              fluidRow(
                column(4, htmlOutput("type_selector",width='70%')),
                column(4, htmlOutput("value_selector",width='70%')),
                column(2, radioButtons("color_scale",NULL, c("Graded Color Scale" = "graded", "% Color Scale" = "pct"))),
                box(width=10, title = strong("US States HeatMap"), solidHeader = T, status = "primary",
                    highchartOutput("heatPlot",height=450),height=500)
                
              )
            )
    ),
    tabItem("Glossary",
            fluidPage(
              fluidRow(
                tabBox(width=12,
                       title = "Glossary",
                       # The id lets us use input$tabset1 on the server to find the current tab
                       id = "tabset1", height = "250px",
                       tabPanel("Data & Definitions", 
                                strong("Data Source:",style = 'color:rgb(0, 112, 192)'),br(),
                                "Bureau of Justice Statistics, Annual Survey of Jails in Indian Country, 2000 and 2010-2015.",
                                hr(),
                                h4(strong("Terminology"), align = "center",style = 'color:rgb(0, 112, 192)'),
                                strong("Midyear:",style = 'color:rgb(0, 112, 192)'),
                                "The number of inmates held on the last weekday in June",
                                br(),br(),
                                strong("ADP:",style = 'color:rgb(0, 112, 192)'),
                                "Average daily population (ADP) is the number of inmates confined each day in June, divided by 30",
                                br(),br(),
                                strong("Peak Population:",style = 'color:rgb(0, 112, 192)'),
                                "The number of inmates held on the day in June in which the custody population of a facility was the largest",
                                br(),br(),
                                strong("Rated Capacity:",style = 'color:rgb(0, 112, 192)'),
                                "The maximum number of beds or inmates assigned by a rating official",
                                br(),br(),
                                strong("Percent of Capacity Occupied:",style = 'color:rgb(0, 112, 192)'),
                                "Calculated by dividing the population count of a facility by its rated capacity and multiplying by 100",
                                hr(),
                                strong("Points to note for Offense data:",style = 'color:rgb(0, 112, 192)'),
                                tags$ul(
                                  tags$li("Larceny theft excludes motor vehicle theft"), 
                                  tags$li("Public intoxication includes drunk and disorderly"), 
                                  tags$li("DWI/DUI includes driving while intoxicated and driving while under the influence of drugs or alcohol")
                                ),
                                hr(),
                                strong("Points to note for Job function and staff data:",style = 'color:rgb(0, 112, 192)'),
                                tags$ul(
                                  tags$li("Administrative includes jail administrators, assistants, and other personnel who work in an administrative capacity more than 50% of the time"), 
                                  tags$li("Jail operations includes correctional officers, guards, and other staff who spend more than 50% of their time supervising inmates")
                                ),
                                hr()
                       ),
                       tabPanel("Dashboards", 
                                h4(strong("Overview"), align = "center",style = 'color:rgb(0, 112, 192)'),
                                strong("Inmates:",style = 'color:rgb(0, 112, 192)'),
                                "Trend of mid year and ADP inmates vs trend of inmates as % of rated capacity",
                                br(),br(),
                                strong("Operating Facilities:",style = 'color:rgb(0, 112, 192)'),
                                "Trend of operating facilities vs trend of inmates per operating facility",
                                br(),br(),
                                strong("Characteristics:",style = 'color:rgb(0, 112, 192)'),
                                "Trend of inmates characteristic distribution like conviction status, gender and offense types",
                                br(),br(),
                                strong("Job Function:",style = 'color:rgb(0, 112, 192)'),
                                "Trend of different job staff contribution vs trend of inmates per fucntional role",
                                hr(),
                                
                                h4(strong("State & Facility"), align = "center",style = 'color:rgb(0, 112, 192)'),
                                strong("Inmate Metrics:",style = 'color:rgb(0, 112, 192)'),
                                "Metrics like Midyear, ADP and peak population and their % with Rated Capacity",
                                br(),br(),
                                strong("Offense Type:",style = 'color:rgb(0, 112, 192)'),
                                "Distribution of different offense types and comparison with overall",
                                br(),br(),
                                strong("Conviction Status:",style = 'color:rgb(0, 112, 192)'),
                                "Distribution of conviction status and comparison with overall",
                                br(),br(),
                                strong("Demographics:",style = 'color:rgb(0, 112, 192)'),
                                "Distribution of Male & Female along with overall comparison",
                                hr(),
                                
                                h4(strong("Comparison"), align = "center",style = 'color:rgb(0, 112, 192)'),
                                strong("US States Heatmap:",style = 'color:rgb(0, 112, 192)'),
                                "Heatmap comparison of different inmate characteristics across US states",
                                hr()
                                
                       ),
                       tabPanel("About Us", 
                                strong("Story - Screenplay - Direction ..."),br(),
                                h4(strong("We mean Data processing - Designing - Story boarding by -"),style = 'color:rgb(0, 112, 192)'),br(),
                                tags$ul(
                                  tags$li(strong("Murali Mohana Krishna",style = 'color:rgb(0, 112, 192)'), em(", Sr. Business Analyst, Tredence Analytics")), br(),
                                  tags$li(strong("Bhavya Kohli",style = 'color:rgb(0, 112, 192)'), em(", Sr. Business Analyst, Tredence Analytics"))
                                ),
                                hr()
                                
                       )
                )
              )
            )
    )
    )
  
)

ui <- dashboardPage(skin = 'blue', header,sidebar,body)

