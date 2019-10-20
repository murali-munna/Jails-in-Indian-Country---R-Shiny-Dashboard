library(ggplot2)
library(shinydashboard)
library(shiny)
library(highcharter)
library(leaflet)
library("viridisLite")
library("dplyr")
library("DT")

server <- function(input, output){
  
  
  output$inmates_trend <- renderHighchart({
    if(input$type_of_inmates == 'MidYear'){
      highchart() %>% 
        hc_xAxis(categories = inmates_trend$year, title=NULL) %>%
        #               hc_yAxis_multiples(
        #                 list(lineWidth = 2),
        #                 list(showLastLabel = FALSE, opposite = TRUE, title=NULL)
        hc_yAxis_multiples(
          list(title = list(text = "MidYear Inmates")),
          list(title = list(text = "Inmates wrt Rated Capacity"),
               opposite = TRUE)
        ) %>% 
        hc_add_series(data=inmates_trend$inmates,name="MidYear Inmates" ,type="line") %>%
        hc_add_series(data=inmates_trend$inmates_pct,name="Inmates wrt Rated Capacity" ,type="line",yAxis=1) %>% hc_tooltip(crosshairs=T,shared=T)
    }
    
    else if(input$type_of_inmates == 'ADP'){
      highchart() %>% 
        hc_xAxis(categories = inmates_trend$year, title = NULL) %>%
        #               hc_yAxis_multiples(
        #                 list(lineWidth = 2),
        #                 list(showLastLabel = FALSE, opposite = TRUE, title=NULL)
        hc_yAxis_multiples(
          list(title = list(text = "Avg. Daily Pop")),
          list(title = list(text = "ADP wrt Rated Capacity"),
               opposite = TRUE)
        ) %>% 
        hc_add_series(data=inmates_trend$adp,name="Avg. Daily Pop" ,type="line") %>%
        hc_add_series(data=inmates_trend$adp_pct,name="ADP wrt Rated Capacity" ,type="line",yAxis=1) %>% hc_tooltip(crosshairs=T,shared=T)
    }
    
  })
  
  output$facility_trend <- renderHighchart({
    if(input$type_of_inmates_fac == 'MidYear'){
      highchart() %>% 
        hc_xAxis(categories = inmates_trend$year, title=NULL) %>%
        #             hc_yAxis_multiples(
        #               list(lineWidth = 2),
        #               list(showLastLabel = FALSE, opposite = TRUE, title=NULL)
        hc_yAxis_multiples(
          list(title = list(text = "Facilities")),
          list(title = list(text = "Inmates per facility"),
               opposite = TRUE)
        ) %>% 
        hc_add_series(data=inmates_trend$op_facs,name="Facilities" ,type="column") %>%
        hc_add_series(data=inmates_trend$inmates_per_fac,name="Inmates per facility" ,type="line",yAxis=1) %>% hc_tooltip(crosshairs=T,shared=T)
    }
    
    else if(input$type_of_inmates_fac == 'ADP'){
      highchart() %>% 
        hc_xAxis(categories = inmates_trend$year, title=NULL) %>%
        #             hc_yAxis_multiples(
        #               list(lineWidth = 2),
        #               list(showLastLabel = FALSE, opposite = TRUE, title=NULL)
        hc_yAxis_multiples(
          list(title = list(text = "Facilities")),
          list(title = list(text = "ADP per facility"),
               opposite = TRUE)
        ) %>% 
        hc_add_series(data=inmates_trend$op_facs,name="Facilities" ,type="column") %>%
        hc_add_series(data=inmates_trend$adp_per_fac,name="ADP per facility" ,type="line",yAxis=1) %>% hc_tooltip(crosshairs=T,shared=T)
    }
    
  })
  
  # Characteristics Trend
  output$char_selector <- renderUI({
    selectInput("char_trend_type",label = NULL,choices = as.character(unique(char_trend$type)),width='80%', selected = "Offense Type")
  })
  
  data_char <- reactive({
    data_char <- subset(char_trend, type == input$char_trend_type)
    #print(data_char)
    data_char
  })
  
  output$char_trend <- renderHighchart({
    hchart(data_char(), "column", hcaes(x = year, y=inmates, group = value)) %>% 
      hc_plotOptions(column = list(stacking = "percent"))
  })
  
  output$job_selector <- renderUI({
    selectInput("job_type",label = NULL,choices = as.character(unique(staff_trend$job_function)),width='80%')
  })
  
  data_staff <- reactive({
    data_staff <- subset(staff_trend, job_function == input$job_type)
    #print(data_staff)
    data_staff
  })
  
  output$staff_trend <- renderHighchart({
    highchart() %>% 
      hc_xAxis(categories = data_staff()$year, title=NULL) %>%
      hc_yAxis_multiples(
        list(title = list(text = "Staff %")),
        list(title = list(text = "Inmates per job function"),
             opposite = TRUE)
      ) %>% 
      hc_add_series(data=data_staff()$staff_pct,name="Staff %" ,type="column") %>%
      hc_add_series(data=data_staff()$inmates_per_job,name="Inmates per job function" ,type="line",yAxis=1) %>% hc_tooltip(crosshairs=T,shared=T)
  })
  
  output$fac_size_table = renderDataTable({
    datatable(facility_size_data, rownames = F,
              options = list(paging = FALSE, searching = FALSE, scrollX = TRUE,
                             initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#0070C0', 'color': '#fff'});",
                               "}"),
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))))
  })
  
  ################# S_F_Summary #####################
  ###################################################
  output$state_selector <- renderUI({
    selectInput("state",label = "State:",choices = as.character(unique(sf_inmates$state)),selected = "Arizona")
  })
  
  output$facility_selector <- renderUI({
    available <- sf_inmates[sf_inmates$state == input$state, "facility"]
    selectInput("facility",label = "Facility:",choices = unique(available),selected = unique(available)[1])
  })
  
  output$inmates <- renderInfoBox({
    value_inmates <- paste0(subset(sf_inmates,state == input$state & facility ==input$facility)$inmates," (",subset(sf_inmates,state == input$state & facility ==input$facility)$pop_midyr_pct,"%)")
    infoBox("MidYear inmates",tags$p(value_inmates, style = "font-size: 150%;"),
            color = "blue", icon = icon("users"), fill=T, width=3)
  })
  output$adp <- renderInfoBox({
    value_adp <- paste0(subset(sf_inmates,state == input$state & facility ==input$facility)$adp," (",subset(sf_inmates,state == input$state & facility ==input$facility)$adp_pct,"%)")
    infoBox("Avg. Daily Pop",tags$p(value_adp, style = "font-size: 150%;"),
            color = "green", icon = icon("address-card-o"), fill=T, width=3)
  })
  output$peak_pop <- renderInfoBox({
    value_peakpop <- paste0(subset(sf_inmates,state == input$state & facility ==input$facility)$peak_pop," (",subset(sf_inmates,state == input$state & facility ==input$facility)$peak_pop_pct,"%)")
    infoBox("Peak population",tags$p(value_peakpop, style = "font-size: 150%;"),
            color = "purple", icon = icon("line-chart"), fill=T, width=3)
  }) 
  output$rated_cap <- renderInfoBox({
    value_ratedcap <- subset(sf_inmates,state == input$state & facility ==input$facility)$rated_cap
    infoBox("Rated Capacity",tags$p(value_ratedcap, style = "font-size: 150%;"),
            color = "maroon", icon = icon("hotel"), fill=T, width=3)
  })
  
  data_offense <- reactive({
    data_offense <- subset(sf_offense, (state == input$state | state == 'Overall')
                           & (facility == input$facility | facility == 'Overall')
                           & offense_type != 'overall')
    data_offense <- data_offense[with(data_offense, order(facility, -inmates_pct)),]
    #print(data_offense)
    data_offense
  })
  
  output$offensePlot <- renderHighchart({
    hchart(data_offense(), "bar", hcaes(x = offense_type, y=inmates_pct, group = facility)) %>% 
      hc_add_theme(hc_theme_538()) %>% 
      hc_xAxis(title = NULL) %>% hc_yAxis(title = "") %>% hc_exporting(enabled = TRUE)
  })
  
  data_convict <- reactive({
    data_convict <- subset(sf_convicted, (state == input$state | state == 'Overall')
                           & (facility == input$facility | facility == 'Overall')
                           & conviction_type != 'overall')
    #print(data_convict)
    data_convict
  })
  
  output$convictPlot <- renderHighchart({
    hchart(data_convict(), "bar", hcaes(x = conviction_type, y=inmates_pct, group = facility)) %>% 
      hc_add_theme(hc_theme_538()) %>% hc_xAxis(title = NULL) %>% hc_yAxis(title = "") %>% hc_tooltip(pointFormat = "{point.y}%")
  })
  
  data_agegrp <- reactive({
    data_agegrp <- subset(sf_demo, (state == input$state | state == 'Overall')
                          & (facility == input$facility | facility == 'Overall'))
    #print(data_agegrp)
    data_agegrp
  })
  
  #     output$adultpct = renderDataTable({
  #       data.frame(a = "Adult %", b = data_agegrp()$adult_t_pct)
  #     })
  
  #     output$adultpct <- renderValueBox({
  #       valueBox(subset(sf_demo,state == input$state & facility ==input$facility)$adult_t_pct,
  #                "Adult %",color = "aqua", icon = icon("users"))
  #     })
  
  data_gender <- reactive({
    data_gender <- subset(sf_gender, (state == input$state | state == 'Overall')
                          & (facility == input$facility | facility == 'Overall'))
    #print(data_gender)
    data_gender
  })
  
  output$genderPlot <- renderHighchart({
    hchart(data_gender(), "column", hcaes(x = gender, y=inmates_pct, group = facility)) %>% 
      hc_add_theme(hc_theme_538()) %>% hc_xAxis(title = NULL) %>% hc_yAxis(title = "Inmates")
  })
  
  ################# Comparison #####################
  ###################################################
  output$type_selector <- renderUI({
    selectInput("type",label = "Type:",choices = as.character(unique(heatmap_data$type)),width='70%',selected = "Offense")
  })
  
  
  
  output$value_selector <- renderUI({
    available <- heatmap_data[heatmap_data$type == input$type, "value"]
    selectInput("value",label = "Value:",choices = unique(available),width='70%',selected = unique(available)[1])
  })
  
  data_heatmap <- reactive({
    data_heatmap <- subset(heatmap_data, (type == input$type) & (value == input$value))
    #print(data_heatmap)
    data_heatmap
  })
  
  output$heatPlot <- renderHighchart({
    
    if(input$color_scale == 'graded'){
      
      highchart() %>% 
        #hc_title(text = "US States Heatmap") %>% 
        hc_add_series_map(usgeojson, data_heatmap(), name = 'Inmates %',
                          value = "inmates_pct", joinBy = c("woename","state"),
                          dataLabels = list(enabled = TRUE,
                                            format = '{point.properties.postalcode}')) %>% 
        #hc_colorAxis(dataClasses = dclass) %>% 
        hc_colorAxis(stops = color_stops()) %>%
        hc_legend(valueDecimals = 0, floating = TRUE, align = "right",valueSuffix = "%") %>%
        #hc_legend(layout = "vertical", align = "right", valueDecimals = 0,valueSuffix = "%") %>% 
        hc_mapNavigation(enabled = TRUE)
    }
    
    else if(input$color_scale == 'pct'){
      
      dclass <- data_frame(from = seq(0, 90, by = 10),
                           to = c(seq(10, 100, by = 10)),
                           color = substring(viridis(length(from), option = "C"), 0, 7))
      dclass <- list.parse3(dclass)
      
      
      highchart() %>% 
        #hc_title(text = "US States Heatmap") %>% 
        hc_add_series_map(usgeojson, data_heatmap(), name = 'Inmates %',
                          value = "inmates_pct", joinBy = c("woename","state"),
                          dataLabels = list(enabled = TRUE,
                                            format = '{point.properties.postalcode}')) %>% 
        hc_colorAxis(dataClasses = dclass) %>% 
        #hc_colorAxis(stops = color_stops()) %>%
        hc_legend(layout = "vertical", align = "right",
                  floating = TRUE, valueDecimals = 0,
                  valueSuffix = "%") %>% 
        hc_mapNavigation(enabled = TRUE)
      
    }
    
    
    
    
  })
  
  
  
}