##########
# UI
##########
library(lubridate)
library(stringr)
library(tidyverse)
library(shiny)
library(DT)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Sample Processing Log Book"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cleaned_Data", tabName = "Cleaned_Data"),
      menuItem("Trend", tabName = "Trend"),
      menuItem("PieChart", tabName = "PieChart"),
      menuItem("BarChart", tabName = "BarChart"),
      menuItem("TransferTime", tabName = "TransferTime")
    )
  ),
  dashboardBody(
    tabItems(  # cleaned dataframe
      tabItem(
        tabName = "Cleaned_Data",
        h2("Cleaned Data"),
        DT::dataTableOutput("cleandata")
      ),
      tabItem( # trend all time
        tabName = "Trend",
        h2("IMF All Time Sample Processing Trend"),
        box(
          width = 11, solidHeader = TRUE, status = "primary",
          plotOutput(outputId = 'plot_all')
        )
      ),
      tabItem( #1 factor pie chart
        tabName = "PieChart",
        fluidRow(
          box(
            title = "All Time Pie Chart View Single Factor", width = 3, solidHeader = TRUE, status = "primary",
            "Select Factor for Visulization",
            selectInput(inputId = "pie_f1",
                        label = "Factor to View by", 
                        choices = c("Year" = "Year", 
                                    "Proc.By" = "Proc.By", 
                                    "Trial" = "Trial", 
                                    "Site" = "Site",
                                    "ON_Scheduled" = "ON_Scheduled",
                                    "Month" = "Month",
                                    "Weekday" = "Weekday",
                                    "OverNight_Proc" = "OverNight_Proc")
            ),
            dateRangeInput(inputId = "date_range",
                           label = "Date Range",
                           startview = "month",
                           start = min(df3$Date.Proc),
                           end = max(df3$Date.Proc),
                           min = min(df3$Date.Proc),
                           max = max(df3$Date.Proc))
          ),
          box(
            width = 8, offset = 1, solidHeader = TRUE, 
            plotOutput('plot_by1')
          )
        )
      ),
      tabItem(
        tabName = "BarChart",
        fluidRow(
          box(
            title = "Bar Chart 2 Factor View", width = 3, solidHeader = TRUE, status = "primary",
            "Select 2 Factors for Visulization",
            
            selectizeInput(inputId = "sort_by", 
                           label = "2 Factors to Summarize by", 
                           options = list(maxItems = 2),
                           selected = NULL, multiple = T,
                           choices = c("Year", "Proc.By", "Trial", "Site", "ON_Scheduled", "Weekday", "OverNight_Proc")),
            dateRangeInput(inputId = "date_range1",
                           label = "Date Range",
                           startview = "month",
                           start = min(df3$Date.Proc),
                           end = max(df3$Date.Proc),
                           min = min(df3$Date.Proc),
                           max = max(df3$Date.Proc))
          ),
          br(),
          box(
            width = 8, offset = 1, solidHeader = TRUE, 
            plotOutput('plot_by2')
          )
        )
      ),
      tabItem(
        tabName = "TransferTime",
        fluidRow(
          dateRangeInput(inputId = "date_range2",
                         label = "Date Range",
                         startview = "month",
                         start = min(df3$Date.Proc),
                         end = max(df3$Date.Proc),
                         min = min(df3$Date.Proc),
                         max = max(df3$Date.Proc))
        ),
        box(
          title = "Sample Receiving Time vs Sample Transfering Duration", status = "primary",
          width = 10,solidHeader = TRUE,  plotOutput('plot_tl', height = 400)
        ),
        box( 
          title = "Site vs Sample Transfering Duration",
          width = 10,solidHeader = TRUE, status = "warning",
          plotOutput('plot_t_site', height = 400)
        ),
        box(
          title = "Top 20 Trials of long Sample Transfering Duration",
          width = 10, status = "primary",
          solidHeader = TRUE, 
          plotOutput('plot_t_trial', height = 400)
        )
      )
    )
  )
)

