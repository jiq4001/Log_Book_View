########
# Server 
########
library(lubridate)
library(stringr)
library(tidyverse)
library(shiny)
library(DT)
library(shinydashboard)


server <- function(input, output) {
  ###########
  # cleaned data all 
  ###########
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
      select(!!!rlang::sym(input$sort_by[[1]]), !!!rlang::sym(input$sort_by[[2]])) %>%
      cbind.data.frame(group = paste(df[, input$sort_by[[1]]], df[, input$sort_by[[2]]], sep = ","))%>%
      group_by(group)%>%
      summarise(total = n())%>%
      separate(group, into = c(input$sort_by[[1]], input$sort_by[[2]]), sep = ",")
    df
  })
  
  
  output$plot_by2<- renderPlot({ 
    req(input$sort_by)
    bar_data() %>%
      ggplot()+
      geom_bar(aes_string(input$sort_by[[1]], input$sort_by[[2]], fill = input$sort_by[[1]]), stat = "identity")+
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
      mutate(Time.Rec = strftime(data.table::as.ITime(round_date(Rec, "30 mins")), "%H:%M") ) %>%
      group_by(Time.Rec) %>%
      arrange(Time.Rec) %>%
      summarise(Draw.to.Rec = mean(Draw.Rec))%>%
      ggplot()+
      geom_bar(aes(Time.Rec, Draw.to.Rec), stat = "identity")+
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