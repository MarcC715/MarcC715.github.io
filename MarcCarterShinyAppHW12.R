library(shiny)
library(dplyr)
library(caret)
library(class)
library(jsonlite)
library(ggplot2)
library(plotly)
library(ggthemes)
library(tidyverse)
library(e1071)

#################################################
# User Side
#################################################
ui <- fluidPage(
  titlePanel("Beer Study Application"),
  sidebarLayout(
    sidebarPanel(
      actionButton("LoadDataButton", label = "Load Data"),
      selectInput("StateFilter", label = "Filter by State",
                  choices = list(
                    "No Filter" = "NOF",
                    "Alabama" = "AL",
                    "Alaska" = "AK",
                    "Arizona" = "AZ",
                    "Arkansas" = "AR",
                    "California" = "CA",
                    "Colorado" = "CO",
                    "Connecticut" = "CT",
                    "Delaware" = "DE",
                    "District Of Columbia" = "DC",
                    "Florida" = "FL",
                    "Georgia" = "GA",
                    "Hawaii" = "HI",
                    "Idaho" = "ID",
                    "Illinois" = "IL",
                    "Indiana" = "IN",
                    "Iowa" = "IA",
                    "Kansas" = "KS",
                    "Kentucky" = "KY",
                    "Louisiana" = "LA",
                    "Maine" = "ME",
                    "Maryland" = "MD",
                    "Massachusetts" = "MA",
                    "Michigan" = "MI",
                    "Minnesota" = "MN",
                    "Mississippi" = "MS",
                    "Missouri" = "MO",
                    "Montana" = "MT",
                    "Nebraska" = "NE",
                    "Nevada" = "NV",
                    "New Hampshire" = "NH",
                    "New Jersey" = "NJ",
                    "New Mexico" = "NM",
                    "New York" = "NY",
                    "North Carolina" = "NC",
                    "North Dakota" = "ND",
                    "Ohio" = "OH",
                    "Oklahoma" = "OK",
                    "Oregon" = "OR",
                    "Pennsylvania" = "PA",
                    "Rhode Island" = "RI",
                    "South Carolina" = "SC",
                    "South Dakota" = "SD",
                    "Tennessee" = "TN",
                    "Texas" = "TX",
                    "Utah" = "UT",
                    "Vermont" = "VT",
                    "Virginia" = "VA",
                    "Washington" = "WA",
                    "West Virginia" = "WV",
                    "Wisconsin" = "WI",
                    "Wyoming" = "WY",
                    selected = 1
                  )),
      checkboxGroupInput("DataPoints", label = h4("Data Points"),
                         choices = list("ABV", "IBU")),
      radioButtons("PlotType", label = h4("Plot Type"),
                   choices = list("histogram", "box", "scatter")),
      conditionalPanel(condition = "input.PlotType == 'scatter'",
        checkboxInput("WithLine", label = "Regression Line", value = FALSE)
      ),
      hr(),
      actionButton("MapButton", label = "State Map")
    ),
    mainPanel(
      plotlyOutput("ShinyPlot"),
      hr(),
      fluidRow(column(10, verbatimTextOutput("LoadMessage"))),
      hr(),
      plotlyOutput("MapPlot")
      # ,
      # hr(),
      # fluidRow(column(10, verbatimTextOutput("LoadMessage2"))),
      # hr(),
      # fluidRow(column(3, verbatimTextOutput("valueLoadDataButton"))),
      # hr(),
      # fluidRow(column(10, verbatimTextOutput("valueStateFilter"))),
      # hr(),
      # fluidRow(column(8, verbatimTextOutput("valueDataPoints"))),
      # hr(),
      # fluidRow(column(4, verbatimTextOutput("valuePlotType"))),
      # hr(),
      # fluidRow(column(3, verbatimTextOutput("valueWithLine")))
    )
  )
)




#################################################
# SERVER SIDE
#################################################

serv <- function(input, output, session){
  
  output$ShinyPlot <- renderPlotly({
    
  ################################################
  # LOAD DATA
  ################################################

   if(input$LoadDataButton >= 1){  # Dont do anything until data is loaded

     ######################################
     ##  Read in Files
     ######################################
     if(!exists("finalData")){  # Only load Data once.

       BeerFile <- read.csv("Beers.txt", strip.white = TRUE, header = TRUE)
       BreweriesFile <- read.csv("Breweries.txt", strip.white = TRUE, header = TRUE)
       StatesFile <- read.csv("states_lat_long.csv", strip.white = TRUE, header = TRUE)

       names(StatesFile)[1] = "State"                        # Rename Column to be able to merge the two data sets.
       names(BreweriesFile)[1] = "Brewery_id"                # Rename Column to be able to merge the two data sets.

       st <- merge(StatesFile, BreweriesFile, by=c("State")) # Merge Data Frames by State.
       fullData <- merge(st, BeerFile, by=c("Brewery_id"))   # Merge Data Frames by Brewery ID.


       # Modify data for Plotting
       abvData <- fullData %>% select(Value = ABV, State)  # Select ABV & State Variables.
       abvData$Type = as.factor("ABV")                     # Add a Type factor to determine what kind of value it is.
       abvData$ValueAdj = abvData$Value * 1000             # Adjust ABV value for plotting on same grid as IBU.

       ibuData <- fullData %>% select(Value = IBU, State)  # Select IBU & State Variables.
       ibuData$Type = as.factor("IBU")                     # Add a Type factor to determine what kind of value it is.
       ibuData$ValueAdj = ibuData$Value                    # NO ADJUSTMENT for IBU, but need this column in order to use Row Bind.

       preData <- rbind(abvData, ibuData)                  # Bind Data Frames into a single data frame

       finalData <- preData %>% select(Type, State, Value, ValueAdj)  # Reorder columns

       output$LoadMessage <- renderPrint("Data Loaded Successfully!")  # Output message about loading data.
     } # end if !exists

    ################################################
    # DETERMINE FILTERS
    ################################################

     if(!input$StateFilter == "NOF"){
       FilteredData <- filter(finalData, State == input$StateFilter)
       ffData <- filter(fullData, State == input$StateFilter)
       output$LoadMessage <- renderPrint(paste0("Data Filtered for ",input$StateFilter))
     } else{
       FilteredData <- finalData
       ffData <- fullData
     }

    ################################################
    # DETERMINE DATA TO PLOT
    ################################################

    if(length(input$DataPoints) == 1 && input$DataPoints[1] == "ABV"){
      FilteredData <- filter(FilteredData, Type == "ABV")
      xlabs = list(title = paste0(input$DataPoints[1]," Value"), tickformat = "%")
    } else if(length(input$DataPoints) == 1 && input$DataPoints[1] == "IBU"){
      FilteredData <- filter(FilteredData, Type == "IBU")
      xlabs = list(title = paste0(input$DataPoints[1]," Value"))
    } else if(length(input$DataPoints) == 2){

    }

    ################################################
    # DETERMINE PLOT TYPE AND Data
    ################################################
    if(length(input$DataPoints) == 1){                # Only try to determine a plot type if a Column has been selected.
      
      if(input$PlotType == "histogram"){
        plot1 <- plot_ly(FilteredData, x = ~Value, color = ~Type, type = input$PlotType,
                         marker = list(color = "Blue", line = list(color = "Red", width = 1))) %>%
          layout(xaxis = xlabs, yaxis=list(title = "Count"), 
                 title = paste0("Histogram of ",input$DataPoints[1]))
  
        observeEvent(input$PlotType,{
          updateCheckboxInput(session, "WithLine", value = FALSE)
        })
        
      } else if(input$PlotType == "box"){
        plot1 <- plot_ly(FilteredData, x = ~Value, color = ~Type, type = input$PlotType,
                         marker = list(color = "Blue", line = list(color = "Red", width = 1))) %>%
          layout(xaxis = xlabs, yaxis=NULL, 
                 title = paste0("Histogram of ",input$DataPoints[1]))
        
        observeEvent(input$PlotType,{
          updateCheckboxInput(session, "WithLine", value = FALSE)
        })
      }
    } else if(length(input$DataPoints) > 1){ 
      if(input$PlotType == "scatter"){
        plot1 <- plot_ly(data = ffData, x = ffData$ABV, y = ffData$IBU, 
                         type = input$PlotType, mode = "markers")

        ################################################
        # DETERMINE REGRESSION LINE
        ################################################
        
        if(input$WithLine){
          noNA_IBU <- filter(ffData, !is.na(ffData$IBU))
          fit <- lm(IBU ~ ABV, data = noNA_IBU)
          plot1 <- plot1 %>% add_lines(x=~ABV, y=fitted(fit), data = noNA_IBU)
        }
        
        plot1 <- plot1 %>% layout(xaxis = list(title = "ABV"), 
                                  yaxis = list(title = "IBU"), 
                                  title = "Scatter Plot of ABV vs. IBU",
                                  showlegend = FALSE)
        
      } else {
        plot1 <- plot_ly(FilteredData, x = ~ValueAdj, color = ~Type, type = input$PlotType) %>%
          layout(xaxis = list(title = "Value (ABV adjusted 1K"))
      } # END if(input$PlotType == "scatter")
    } # END if(length(input$DataPoints) > 0)
    
    ################################################
    # CALL PLOT
    ################################################
    if(length(input$DataPoints) == 1 && !input$PlotType == "scatter"){ 
      plot1
    } else if(length(input$DataPoints) == 2){
      plot1
    }

   } # END if(input$LoadDataButton >= 1)
  }) # END output$ShinyPlot <- renderPlot(
  
  output$MapPlot <- renderPlotly({
    
    if(input$MapButton >= 1){
      
      BeerFile <- read.csv("Beers.txt", strip.white = TRUE, header = TRUE)
      BreweriesFile <- read.csv("Breweries.txt", strip.white = TRUE, header = TRUE)
      StatesFile <- read.csv("states_lat_long.csv", strip.white = TRUE, header = TRUE)

      Breweries_By_State <- BreweriesFile %>% count(State)
      names(Breweries_By_State)[2] = "BreweriesCount"  # Rename Column to ensure understanding.
      
      names(StatesFile)[1] = "State"  # Rename Column to be able to merge the two data sets.
      st <- merge(StatesFile, Breweries_By_State, by=c("State")) # Merge Data Frames by State.
      
      # Change the BreweriesFile "Brew_ID" column header to "Brewery_ID" to merge.
      names(BeerFile)[1] <- "BeerName"
      names(BeerFile)[5] <- "Brewery_ID"
      names(BreweriesFile)[1] <- "Brewery_ID"
      names(BreweriesFile)[2] <- "BreweryName"
      
      BeersData <- merge(BreweriesFile,BeerFile, by=c("Brewery_ID"))
      
      BeersByState <- group_by(BeersData, State)
      
      BeerStatsByState <- summarise(BeersByState,
                                    abv_mean = mean(ABV, na.rm = TRUE),
                                    abv_sd = sd(ABV, na.rm = TRUE),
                                    abv_med = median(ABV, na.rm = TRUE),
                                    abv_min = min(ABV, na.rm = TRUE),
                                    abv_max = max(ABV, na.rm = TRUE),
                                    ibu_mean = mean(IBU, na.rm = TRUE),
                                    ibu_sd = sd(IBU, na.rm = TRUE),
                                    ibu_med = median(IBU, na.rm = TRUE),
                                    ibu_min = min(IBU, na.rm = TRUE),
                                    ibu_max = max(IBU, na.rm = TRUE),
                                    rec_cnt = n())
      
      st_stats <- merge(st, BeerStatsByState, by=c("State")) # Merge Data Frames by State.
      
      ######################################
      ##  Plot USA 
      #####################################
      g2 <- list(
        scope = 'usa',
        projection = list(type = 'albers usa'),
        showland = TRUE,
        landcolor = toRGB("gray95"),
        showlakes = TRUE,
        lakecolor = toRGB("blue"),
        subunitcolor = toRGB("Red"),
        countrycolor= toRGB("gray85"),
        contrywidth = 0.5,
        subunitwidth = 0.5
      )
      
      plot_geo(st_stats, locationmode = 'USA-states') %>%
        add_markers(x=st_stats$Long, y=st_stats$Lat, size = st_stats$ibu_mean,
                    color = st_stats$abv_mean,
                    colors = "Reds", hoverinfo = "text",
                    text = paste0(
                      "State : ", st_stats$State, "<br />",
                      "Population = ", format(st_stats$Over21Pop,big.mark = ","), "<br />",
                      "Breweries Count : ", st_stats$BreweriesCount, "<br />",
                      "Mean ABV : ", sprintf("%.2f %%",100*st_stats$abv_mean), "<br />",
                      "Mean IBU : ", round(st_stats$ibu_mean,2))
        ) %>%
        layout(title = "Mean Statistics by State (Marker Size = IBU, Color = ABV)",
               geo = g2)
      
    } # END if Map button >= 1
  }) # END output$MapPlot

  
  ################################################
  # OTHER OUTPUTS FOR TROUBLESHOOTING / LEARNING
  ################################################
  
  # output$valueLoadDataButton <- renderPrint({input$LoadDataButton})
  # output$valueStateFilter <- renderPrint({input$StateFilter})
  # output$valueDataPoints <- renderPrint({input$DataPoints})
  # output$valuePlotType <- renderPrint({input$PlotType})
  # output$valueWithLine <- renderPrint({input$WithLine})
  
} # END serv function

################################################
# CALL SHINY APP
################################################

shinyApp(ui, serv)





