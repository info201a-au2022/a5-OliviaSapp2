#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
source("ui.R")
library("plotly")


co2_data <- read.csv("owid-co2-data.csv")
#co2_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

countries_list <- c("China", "United States", "Russia", "Japan", "Iran", "Germany",
                    "Saudi Arabia", "Indonesia", "South Korea", "Canada")

co2_clean_data <- co2_data %>% 
  select(year, country, co2, co2_per_capita, co2_per_gdp) %>% 
  filter(country == countries_list)
co2_clean_data$co2 <- as.numeric(co2_clean_data$co2)
co2_clean_data$co2_per_capita <- as.numeric(co2_clean_data$co2_per_capita)
co2_clean_data$co2_per_gdp <- as.numeric(co2_clean_data$co2_per_gdp)


shinyServer(function(input, output) {

  
  output$var1 <- renderText({
    
    highest_co2 <- co2_data %>% 
      select(year, country, co2) %>% 
      filter(year == 2021) %>% 
      arrange(-co2) %>% 
      na.omit() %>% 
      filter(!row_number() %in% c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 16, 17,
                                  18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 30, 41, 58, 61)) %>% 
      top_n(10, co2) %>% 
      pull(country)
    
    message_str <- paste0(highest_co2[1], ", ", highest_co2[2], ", ", highest_co2[3], ", ",
                          highest_co2[4], ", ", highest_co2[5], ", ", highest_co2[6], ", ",
                          highest_co2[7], ", ", highest_co2[8], ", ", highest_co2[9], ", and ",
                          highest_co2[10], ".")
    # Return the value to be rendered by the UI
    message_str
  })
  
  output$var2 <- renderText({
    highest_co2_per_capita <- co2_data %>% 
      select(year, country, co2_per_capita) %>% 
      arrange(-co2_per_capita) %>% 
      na.omit() %>% 
      top_n(1) 
    highest_co2_per_capita_country <- highest_co2_per_capita %>% 
      pull(country)
    highest_co2_per_capita_year <- highest_co2_per_capita %>% 
     pull(year)  
    highest_co2_per_capita_val <- highest_co2_per_capita %>% 
      pull(co2_per_capita) 
    
    message_str <- paste0("The highest CO2 per capita was in ", highest_co2_per_capita_country,
                         " in the year ", highest_co2_per_capita_year, 
                         ". They produced ", highest_co2_per_capita_val, 
                         " tons of CO2 per person.")
    
    # Return the value to be rendered by the UI
    message_str
  })
  output$var3 <- renderText({
    highest_co2_per_gdp <- co2_data %>% 
      select(year, country, co2_per_gdp) %>% 
      arrange(-co2_per_gdp) %>% 
      na.omit() %>% 
      top_n(1) 
    highest_co2_per_gdp_country <- highest_co2_per_gdp %>% 
      pull(country)
    highest_co2_per_gdp_year <- highest_co2_per_gdp %>% 
      pull(year)  
    highest_co2_per_gdp_val <- highest_co2_per_gdp %>% 
      pull(co2_per_gdp)  
    
    message_str <- paste0("The highest CO2 per gdp was in ", highest_co2_per_gdp_country,
                          " in the year ", highest_co2_per_gdp_year, 
                          ". They produced ", highest_co2_per_gdp_val, 
                          " kilograms of CO2 per dollar of GDP.")
    
    # Return the value to be rendered by the UI
    message_str
  })
  
    output$line_graph <- renderPlotly({
      
        if(input$co2_value == "CO2"){
          # data frame for the plot
          get_co2_data  <- co2_clean_data %>% 
            select(year, country, co2) %>% 
            filter(country %in%  countries_list) %>% 
            filter(co2_clean_data$year >= input$years[1], 
                   co2_clean_data$year <= input$years[2])
          # plots an interactive line chart 
          plot_co2 <- ggplot(data = get_co2_data, 
                             aes(x = year, y = co2)) +
            geom_line(aes(color = country)) +
            ggtitle("CO2 Production 1800-2021 Per Country") +
            scale_y_continuous(labels = comma) +
            xlab("Year") + ylab("CO2 Produced (millions of tonnes)")
          
          ggplotly(plot_co2)
        } else
          if(input$co2_value == "CO2 Per Capita"){
            # data frame for the plot
            get_co2_data  <- co2_clean_data %>% 
              select(year, country, co2_per_capita) %>% 
              filter(country %in%  countries_list) %>% 
              filter(co2_clean_data$year >= input$years[1], 
                     co2_clean_data$year <= input$years[2])
            # plots an interactive line chart 
            plot_co2 <- ggplot(data = get_co2_data, 
                               aes(x = year, y = co2_per_capita)) +
              geom_line(aes(color = country)) +
              ggtitle("CO2 Production Per Capita 1800-2021 Per Country") +
              scale_y_continuous(labels = comma) +
              xlab("Year") + ylab("CO2 Per Capita (tonnes per person)")
            
            ggplotly(plot_co2)
          } else{
            # data frame for the plot
            get_co2_data  <- co2_clean_data %>% 
              select(year, country, co2_per_gdp) %>% 
              filter(country %in%  countries_list) %>% 
              filter(co2_clean_data$year >= input$years[1], 
                     co2_clean_data$year <= input$years[2])
            # plots an interactive line chart 
            plot_co2 <- ggplot(data = get_co2_data, 
                               aes(x = year, y = co2_per_gdp)) +
              geom_line(aes(color = country)) +
              ggtitle("CO2 Production Per GDP 1800-2021 Per Country") +
              scale_y_continuous(labels = comma) +
              xlab("Year") + ylab("CO2 Per GDP (kilograms per dollar)")
            
            ggplotly(plot_co2)
          }

    })

})
