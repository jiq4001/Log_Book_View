##########
# UI
##########
library(lubridate)
library(stringr)
library(tidyverse)
library(shiny)
library(DT)
library(shinydashboard)


df<- read.csv("data/Log_Book_Final.csv")
colnames(df)<- c("Proc.By", "Time.Drawn", "Date.Drawn", "Date.Proc", "Time.Rec", "Time.Proc", "Trial", "Site", "ON_Scheduled")

###############
# Clear blank char and 
# fill missing input
###############

df$Proc.By<- substr(str_trim(df$Proc.By, "both"), 1, 2)
df[, "ON_Scheduled"]<- str_trim(toupper(df[, "ON_Scheduled"])) 
df$ON_Scheduled <- as.character(df$ON_Scheduled)
df$ON_Scheduled[which(df$ON_Scheduled != "YES")]<- "NO"


###############
# Coerce Date format
###############

df%>% mutate(Proc.By= substr(str_trim(df$Proc.By, "both"), 1, 2), 
             Drawn= as.POSIXct(paste(df$Date.Drawn, df$Time.Drawn), format="%m / %d / %Y %H : %M"),
             Rec= as.POSIXct(paste(df$Date.Drawn, df$Time.Rec), format="%m / %d / %Y %H : %M"),
             Proc= as.POSIXct(paste(df$Date.Proc, df$Time.Proc), format="%m / %d / %Y %H : %M"),
             Trial= str_trim(df$Trial), 
             Site= toupper(str_trim(df$Site)
             ) ) %>%
  drop_na()%>%
  select(Proc.By, Date.Proc, Drawn, Rec, Proc, Trial, Site, ON_Scheduled, Time.Rec) -> df11

###############
# Clean String input information
###############

#unique(df11$Site)
df11$Site <- case_when(
  str_detect(df11$Site, "(EXT)") ~ "EXTERNAL",
  df11$Site %in% c("WOLCHOK LAB", "MANHATTAN", "NYP", "WCMC CTRU", "MAIN") ~ "MAIN",
  str_detect(df11$Site, "(BASKING RIDGE)") | str_detect(df11$Site, "(BSK)") ~ "BASKING_RIDGE",
  str_detect(df11$Site, "(LVH)") | str_detect(df11$Site, "(LEHIGH)")~ "LEHIGH_VALLEY",
  str_detect(df11$Site, "(53)") | str_detect(df11$Site, "(35)")~ "53RD ST",
  str_detect(df11$Site, "(60)")  ~ "60TH_ST",
  str_detect(df11$Site, "(68)")  ~ "68TH_ST",
  str_detect(df11$Site, "(64)")  ~ "64TH_ST",
  str_detect(df11$Site, "(BER)")  ~ "BERGEN",
  str_detect(df11$Site, "(NOR)")  ~ "NORTHWELL",
  str_detect(df11$Site, "(WES)") | str_detect(df11$Site, "(WST)") ~ "WESTCHESTER",
  str_detect(df11$Site, "(NAS)")  ~ "NASSAU",
  str_detect(df11$Site, "(MON)")  ~ "MONMOUTH",
  str_detect(df11$Site, "(RVC)") | str_detect(df11$Site, "(ROC)") ~ "ROCKVILLE",
  str_detect(df11$Site, "(KI)")  ~ "KIMMEL",
  str_detect(df11$Site, "(HAR)")  ~ "HARTFORD",
  str_detect(df11$Site, "(COM)") | str_detect(df11$Site, "(CMK)")  ~ "COMMACK",
  TRUE ~ "OTHER"
)

###############
# Create vars for summary
###############

df11 %>% mutate(Date.Proc = as.Date(Date.Proc, format = "%m/%d/%Y"),
                Year = factor(year(Proc)), 
                Month = month(Proc), 
                
                Weekday = factor(wday(Proc), levels = c(2, 3, 4, 5, 6), labels = c("Mon", "Tue", "Wed", "Thur", "Fri")), 
                Week = factor(week(Proc)), 
                Draw.Rec = round(difftime(Rec, Drawn, units = "hours"), 1),
                Draw.Rec = ifelse(Draw.Rec <= 0, NA, Draw.Rec),
                Proc.Rec = round(difftime(Proc, Rec, units = "hours"), 1), 
                Proc.Rec = ifelse(Proc.Rec < 0, NA, Proc.Rec),
                OverNight_Proc = ifelse(Proc.Rec> 10, "YES", "NO") ) %>%
  drop_na_(c("Draw.Rec", "Proc.Rec"))-> df3

df3$Month <- factor(df3$Month)


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

