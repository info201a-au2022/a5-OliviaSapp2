# A5

# libraries
library("tidyverse")
library("dplyr")
library("ggplot2")
library("shiny")
library("scales")
library("plotly")



# download data table
#owid_co2_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")
#write.csv(owid_co2_data,"/Users/oliviasapp/Documents/info201/assignments/a5-OliviaSapp2/owid-co2-data.csv", row.names = FALSE)
co2_data <- read.csv("owid-co2-data.csv")

countries <- co2_data %>% 
  select(country)
countries <- unique(countries)

co2_clean_data <- co2_data %>% 
  select(year, country, co2, co2_per_capita, co2_per_gdp) %>% 
  filter(country == countries_list)

# Countries w/ the highest CO2 production in 2021
# use this list in the chart, goes into countries_list
# China, United States, Russia, Japan, Iran, Germany, Saudi Arabia, Indonesia,
# South Korea, Canada
highest_co2 <- co2_data %>% 
  select(year, country, co2) %>% 
  filter(year == 2021) %>% 
  arrange(-co2) %>% 
  na.omit() %>% 
  filter(!row_number() %in% c(1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 16, 17,
                              18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 30, 41, 58, 61)) %>% 
  top_n(10, co2) %>% 
  pull(country)

# Countries w/ the lowest CO2 production in 2021
# use this list in the chart, goes into countries_list
# Tuvalu, Niue, Saint Helena, Montserrat, Wallis and Futuna, Nauru, 
# Saint Pierre and Miquelon, Kiribati, Cook Islands, Bonaire Sint Eustatius and Saba
lowest_co2 <- co2_data %>% 
  select(year, country, co2) %>% 
  filter(year == 2021) %>% 
  arrange(co2) %>% 
  na.omit() %>% 
  top_n(-10, co2) %>% 
  pull(country)

# use this for the countries that will appear on the graph
countries_list <- c("China", "United States", "Russia", "Japan", "Iran", "Germany",
                    "Saudi Arabia", "Indonesia", "South Korea", "Canada")
?paste
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

# df and graph 1 co2
get_co2_data <- function(countries) {
  df <- co2_data %>% 
    select(year, country, co2) %>% 
    filter(country %in%  countries)
  return(df)
}

plot_co2 <- ggplot(data = get_co2_data(countries_list), 
            aes(x = year, y = co2)) +
  geom_line(aes(color = country)) +
  ggtitle("CO2 Production 1750-2021 Per Country") +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Total CO2")
ggplotly(plot_co2)

# df and graph 2 co2_per_capita
get_co2_per_capita_data <- function(countries) {
  df <- co2_data %>% 
    select(year, country, co2_per_capita) %>% 
    filter(country %in%  countries)
  return(df)
}

plot_co2_per_capita <- ggplot(data = get_co2_per_capita_data(countries_list), 
                   aes(x = year, y = co2_per_capita)) +
  geom_line(aes(color = country)) +
  ggtitle("CO2 Per Capita Production 1750-2021 Per Country") +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Total CO2 Per Capita")
ggplotly(plot_co2_per_capita)

# df and graph 1 co2_per_gdp
get_co2_per_gdp_data <- function(countries) {
  df <- co2_data %>% 
    select(year, country, co2_per_gdp) %>% 
    filter(country %in%  countries)
  return(df)
}

plot_co2_per_gdp <- ggplot(data = get_co2_per_gdp_data(countries_list), 
                   aes(x = year, y = co2_per_gdp)) +
  geom_line(aes(color = country)) +
  ggtitle("CO2 Production Per GDP 1750-2021 Per Country") +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Total CO2 Per GDP")
ggplotly(plot_co2_per_gdp)

# 3 values of interest 
  # 1. co2
  # 2. co2_per_capita
  # 3. co2_per_gdp
  # widget presentation: change country
  # widget presentation: change year 1750-2021
  # widget data displayed: change values of interest 


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
  ggtitle("CO2 Production Per GDP 1750-2021 Per Country") +
  scale_y_continuous(labels = comma) +
  xlab("Year") + ylab("Total CO2 Per GDP")

ggplotly(plot_co2)
