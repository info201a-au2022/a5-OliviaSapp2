#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("tidyverse")
library("dplyr")
library("ggplot2")
library("shiny")
library("scales")
library("plotly")

countries_list <- c("China", "United States", "Russia", "Japan", "Iran", "Germany",
                    "Saudi Arabia", "Indonesia", "South Korea", "Canada")
co2_clean_data <- co2_data %>% 
  select(year, country, co2, co2_per_capita, co2_per_gdp) %>% 
  filter(country == countries_list)
co2_clean_data$co2 <- as.numeric(co2_clean_data$co2)
co2_clean_data$co2_per_capita <- as.numeric(co2_clean_data$co2_per_capita)
co2_clean_data$co2_per_gdp <- as.numeric(co2_clean_data$co2_per_gdp)

co2_col_names <- co2_clean_data %>% 
  select(co2, co2_per_capita, co2_per_gdp)
  


# Define UI for application 
ui <- navbarPage(
  title = "A5 CO2 data",
  tabPanel(
    "Introduction",
    fluidPage(
      img(src = "https://scx2.b-cdn.net/gfx/news/2018/co2shortagew.jpg", height="50%", width="50%", align="left"),
    ),
    h3("Over View"),
    h4("This project looks at CO2 data from around the world. I got my CO2 data
    frame from OWID (Our World In Data). My project focuses on the CO2 production 
    from 10 high producing countries in 2021. It looks at the CO2 produced, CO2 
    produced per capita, and the CO2 produced per GDP of these countries. These 
    values are interesting to look at because it breaks down the CO2 production 
    by the country's wealth, and population. Since most of these countries are 
    quite large and powerful, these are important factors to consider. Concentrating 
    on CO2 production by GDP or per capita highlights different economic impacts 
    that may correlate with each country's contribution to global warming."),
    
    h3("Three variables"),
    h4("Top 10 CO2 producing countries in 2021: "),
    textOutput(outputId = "var1"),
    h4("CO2 Per Capita:"),
    textOutput(outputId = "var2"),
    h4("CO2 Per GDP:"),
    textOutput(outputId = "var3")
    
  ),
  tabPanel(
    "Interactive visualization",
    fluidPage(
      titlePanel("CO2 Trends Over Time in 10 Countries"),
      sidebarLayout(
        
        sidebarPanel(
          # widget 1
          sliderInput("years", label = h3("Date Range"), min = 1800, 
                      max = 2021, value = c(1800, 2021)),
         
          # add widget 2
          selectInput("co2_value", label = h3("Select a CO2 Production Variable"), 
                      choices = list("CO2", "CO2 Per Capita","CO2 Per GDP"), 
                      selected = 1),
        ),
          
      mainPanel(
        # chart
        plotlyOutput(outputId = "line_graph"),
      )),
    ),
        h4("The map looks at three different CO2 production variables over time.
        It shows data from 10 countries; I chose these because they were the  
        top 10 CO2 producing countries in 2021."),
        h2("Interact With The Graph"),
        h4("The user can change the date range to look at different time periods
        over the past 200 years. If the user hovers their mouse above a trend 
        line it will tell them the exact year, country, and CO2 produced at the 
        point in the graph. The user can change the CO2 production variable, to 
        CO2: Annual total production-based emissions of carbon dioxide measured 
        in million tonnes, CO2 Per Capita: Annual total production-based emissions 
        of carbon dioxide measured in tonnes per person, or CO2 Per GDP: Annual 
        total production-based emissions of carbon dioxide measured in kilograms 
        per dollar of GDP (2011 international-$)."),
        h2("Trends"),
        h4("There seems to have been a large increase in CO2 production around 
        the 1960s. Starting in the 200s China’s Co2 production skyrocketed. But 
        The United States, Russia, and Germany actually started to decrease their
        CO2 production around the same time. In general the CO2 per capita of each 
        country greatly increased between 1900-1970. After that some continued to
        increase, while the United States, Canada, Germany, and Russia all saw 
        drops. The CO2 Per GDP peaked for its top 3 countries (the United States,
        Canada, and Germany) around the year 1920, and after that it steadily 
        decreased again. South Korea, Japan, and Indonesia had much smaller 
        peaks in the late 1970s.")
    
  ),
)
