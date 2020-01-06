########
# Server 
########

server <- function(input, output) {
  ###########
  # cleaned data all 
  ###########
  #df3 <- readRDS("data/Log_Book.RDS")
  
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
               If_AfterHour = as.POSIXct(paste(df$Date.Drawn, "17:30"), format="%m / %d / %Y %H : %M"),
               If_AfterHour = ifelse(Rec > If_AfterHour, "Late", "Norm"),
               Round_Rec.Time = round_date(Rec, "30 mins"),
               Site= toupper(str_trim(df$Site)
               ) ) %>%
    drop_na()%>%
    select(Proc.By, Date.Proc, Drawn, Rec, Proc, Trial, Site, ON_Scheduled, Round_Rec.Time, If_AfterHour) -> df11
  
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
                  Month = factor(month(Proc), levels = 1:12, labels = c("Jan", "Feb", "Mar", 
                                                                        "April", "May", "June",
                                                                        "July", "Aug", "Sep",
                                                                        "Oct", "Nov", "Dec")),
                  Weekday = factor(wday(Proc), levels = c(2, 3, 4, 5, 6), labels = c("Mon", "Tue", "Wed", "Thur", "Fri")), 
                  #Week = factor(week(Proc)),
                  Round_Rec.Time = strftime(Round_Rec.Time, "%H:%M"),
                  Draw.Rec = round(difftime(Rec, Drawn, units = "hours"), 1),
                  Draw.Rec = ifelse(Draw.Rec <= 0, NA, Draw.Rec),
                  Proc.Rec = round(difftime(Proc, Rec, units = "hours"), 1), 
                  Proc.Rec = ifelse(Proc.Rec < 0, NA, Proc.Rec),
                  OverNight_Proc = ifelse(Proc.Rec> 10, "YES", "NO") ) %>%
    drop_na_(c("Draw.Rec", "Proc.Rec"))-> df3
  
  
  
  output$cleandata <- DT::renderDataTable({
    datatable(df3)
  })
  ###########
  # trend all 
  ###########  
  output$plot_all<- renderPlot({ 
    df3 %>%
      group_by(Date.Proc) %>%
      summarise(Daily = n()) %>%
      inner_join(df3 %>% select(Date.Proc, Year))%>%
      unique() %>%
      group_by(Year) %>%
      ggplot()+
      geom_line(aes(Date.Proc, Daily, color = Year))+
      geom_smooth(aes(Date.Proc, Daily, color = Year), method = "lm", se = F)+
      theme_classic()
  })
  ###########
  # 1 factor pie chart with time range 
  ###########    
  pie_data <- reactive({
    len <- ifelse(length(unique(df3[, input$pie_f1])) > 12, 12, length(unique(df3[, input$pie_f1])))
    df <- df3 %>%
      filter(Date.Proc >= input$date_range[[1]] & Date.Proc <= input$date_range[[2]])%>%
      select(!!rlang::sym(input$pie_f1)) %>%
      group_by(!!rlang::sym(input$pie_f1)) %>%
      summarise(total = n()) %>%
      arrange(desc(total)) %>%
      head(len)
    df
  })
  
  output$plot_by1<- renderPlot({ 
    pie_data() %>%
      ggplot(aes(x= "", y = total, fill= !!rlang::sym(input$pie_f1)))+
      geom_bar(width = 1, stat = "identity",color="white")+
      coord_polar("y")+
      theme_void()  
  })
  ###########
  # 2 factor bar chart with time range 
  ###########    
  bar_data <- reactive({
    df <- df3 %>%
      filter(Date.Proc >= input$date_range1[[1]] & Date.Proc <= input$date_range1[[2]])
    df <- df %>%
      select(!!!rlang::sym(input$sort_by[[1]]), !!!rlang::sym(input$sort_by[[2]])) #%>%
      #cbind.data.frame(group = paste(df[, input$sort_by[[1]]], df[, input$sort_by[[2]]], sep = ","))%>%
      #group_by(group)%>%
      #summarise(total = n())%>%
      #separate(group, into = c(input$sort_by[[1]], input$sort_by[[2]]), sep = ",")
    df
  })
  
  
  output$plot_by2<- renderPlot({ 
    req(input$sort_by)
    bar_data() %>%
      ggplot()+
      geom_bar(aes_string(input$sort_by[[1]], fill = input$sort_by[[2]]))+
      theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )  
  })
  ###########
  # sample transfering time 
  ###########   
  time_data <- reactive({
    df <- df3 %>%
      filter(Date.Proc >= input$date_range2[[1]] & Date.Proc <= input$date_range2[[2]])
    df
  })
  
  output$plot_tl <- renderPlot({
    time_data()%>%
      group_by(Round_Rec.Time) %>%
      arrange(Round_Rec.Time) %>%
      summarise(Draw.to.Rec = mean(Draw.Rec))%>%
      ggplot()+
      geom_bar(aes(Round_Rec.Time, Draw.to.Rec), stat = "identity")+
      theme(
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })
  
  output$plot_t_site <- renderPlot({
    time_data()%>%
      group_by(Site) %>%
      summarise(Draw.to.Rec = mean(Draw.Rec))%>%
      ggplot()+
      geom_bar(aes(Site, Draw.to.Rec), stat = "identity")+
      theme(
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })
  
  
  output$plot_t_trial <- renderPlot({
    time_data()%>%
      group_by(Trial) %>%
      summarise(Draw.to.Rec = mean(Draw.Rec))%>%
      arrange(desc(Draw.to.Rec)) %>%
      head(20)%>%
      ggplot()+
      geom_bar(aes(Trial, Draw.to.Rec), stat = "identity")+
      theme(
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        #axis.title.y = element_blank(),
        #axis.text.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
  })

}