library(shiny)
library(tidyverse)
library(sf)

# TODO
# Fix maps (maybe change to plotly?)
# Fix slope graphs (not rendering properly, and need to remove the values that have only before or after but not both)

ui <- fluidPage(
  
  textOutput(outputId = "question_text"),
  plotOutput(outputId = "donors_plot1"),
  plotOutput(outputId = "donors_plot2"),
  plotOutput(outputId = "donors_plot3"),
  plotOutput(outputId = "recip_plot1"),
  plotOutput(outputId = "recip_plot2"),
  plotOutput(outputId = "recip_plot3"),
  textOutput(outputId = "results_text")
)

server <-  function(input, output){
  
  # Stating the question
  output$question_text <- renderText("What are the differences in worldwide aid before and after 2008?")

  # Read in data
  data <- read.csv("~/Y4-25-Spring/DATA 4980 Data Visualization/Git/aid-dashboard/data/Aid Data Example - Sheet1.csv")
  world_map <- read_sf("~/Y4-25-Spring/DATA 4980 Data Visualization/Git/aid-dashboard/mkuehn/World_Countries_(Generalized)_9029012925078512962.geojson")
  
  # Aggregate the data
  aid_agg <- data %>% 
    group_by(year,donor,recipient) %>% 
    summarize(total_commitment = sum(commitment_amount_usd_constant))
  
  # Donors
  aid_beforeafter_donor <- aid_agg %>% filter(year>=2005) %>% 
    mutate(period=ifelse(year<2008,'Before','After')) %>% 
    group_by(donor,period) %>% 
    summarize(aid_given=sum(total_commitment)) %>% 
    arrange(desc(period))

  aid_beforeafter_donor <- aid_beforeafter_donor %>% 
    mutate(COUNTRY=donor)
  
  donor_beforeafter_joined <- world_map %>% 
    left_join(aid_beforeafter_donor)
  
  df <- donor_beforeafter_joined %>% filter(is.na(period)|period=="Before")
  
  p_before <- ggplot(df) +
    geom_sf(aes(fill=aid_given)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Commitments (2005-2007)",fill="Aid Commitment")
  
  df <- donor_beforeafter_joined %>% filter(is.na(period)|period=="After")
  
  p_after <- ggplot(df) +
    geom_sf(aes(fill=aid_given)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Commitments (2008-2010)",fill="Aid Commitment")
  
  output$donors_plot1 <- renderPlot(p_before)
  output$donors_plot2 <- renderPlot(p_after)
  
  df <- filter(donor_beforeafter_joined,!is.na(aid_given))
  
  p1 <- ggplot(df,aes(x=forcats::fct_rev(period),group=COUNTRY,y=aid_given)) +
    geom_point()+
    geom_line()+
    scale_y_continuous(breaks = seq(0, 150000000, by = 20000000),labels=scales::dollar)+
    labs(y="Aid Contribution (USD2009)",x="Time Period",title="Worldwide Aid Commitments (2005-2010)",subtitle="Before and After 2008")+
    theme_minimal()+
    facet_wrap(~COUNTRY,ncol=6)
  
  output$donors_plot3 <- renderPlot(p1)
  
  # Recipients
  aid_beforeafter_recipient <- aid_agg %>% filter(year>=2005) %>% 
    mutate(period=ifelse(year<2008,'Before','After')) %>% 
    group_by(recipient,period) %>% 
    summarize(aid_received=sum(total_commitment)) %>% 
    arrange(desc(period))
  
  aid_beforeafter_recipient <- aid_beforeafter_recipient %>% 
    mutate(COUNTRY=recipient)
  
  recipient_beforeafter_joined <- world_map %>% 
    left_join(aid_beforeafter_recipient)
  
  df <- recipient_beforeafter_joined %>% filter(is.na(period)|period=="Before")
  
  p_before <- ggplot(df) +
    geom_sf(aes(fill=aid_received)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Received (2005-2007)",fill="Aid Received")
  
  df <- recipient_beforeafter_joined %>% filter(is.na(period)|period=="After")
  
  p_after <- ggplot(df) +
    geom_sf(aes(fill=aid_received)) +
    scale_fill_gradient(low="navy",high="gold",na.value = "grey70",labels=scales::dollar)+
    labs(title="Worldwide Aid Received (2008-2010)",fill="Aid Received")
  
  output$recip_plot1 <- renderPlot(p_before)
  output$recip_plot2 <- renderPlot(p_after)
  
  df <- filter(recipient_beforeafter_joined,!is.na(aid_received))
  
  p1 <- ggplot(df,aes(x=forcats::fct_rev(period),group=COUNTRY,y=aid_received)) +
    geom_point()+
    geom_line()+
    scale_y_continuous(labels=scales::dollar)+
    labs(y="Aid Received (USD2009)",x="Time Period",title="Worldwide Aid Received (2005-2010)",subtitle="Before and After 2008")+
    theme_minimal()+
    facet_wrap(~COUNTRY,ncol=4)
  
  output$recip_plot3 <- renderPlot(p1)
  
  # Results and discussion
  output$results_text <- renderText("Differences in aid before and after 2008 could be related to the Great Recession and new political leaders, such as the election of Barack Obama.")
  
}

shinyApp(ui, server)

