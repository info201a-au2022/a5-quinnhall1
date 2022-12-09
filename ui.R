library(dplyr)
library(ggplot2)
library(shiny)
library(tidyr)
library(plotly)


data <- read.csv('https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv')


barplot_data <- data %>% select(iso_code, co2_per_capita, population, year) %>%
  filter(!is.na(iso_code)) %>%
  filter(population > 71000000)  %>%
  filter(iso_code != '') %>%
  filter(iso_code != "OWID_WRL") %>%
  group_by(iso_code) %>%
  arrange(desc(co2_per_capita)) 

average_pop_2020_mostpop <- mean((barplot_data %>% filter(year == 2020))$population)

average_co2cap <- mean((barplot_data %>% filter(year == 2020))$co2_per_capita, na.rm = TRUE)

max_co2_per <- max((barplot_data %>% filter(year == 2020))$co2_per_capita)

max_pop <- max((barplot_data %>% filter(year == 2020))$population)

min_co2_per <- min((barplot_data %>% filter(year == 2020))$co2_per_capita)


plot <- ggplot(data = barplot_data, aes(x= reorder(iso_code, -co2_per_capita), y=co2_per_capita)) +
  xlab('Country') +
  ylab('CO2 Per Capita') +
  geom_bar(stat="identity")

plot

page_one <- tabPanel(
  "Introduction",
  h1("Introduction to CO2 Gas Emissions"),
  img(src="https://grist.org/wp-content/uploads/2015/12/emissions.jpg",
      height="75%", width="90%", align="center"),
  p("Human emissions of carbon dioxide and other greenhouse gases – are a primary driver of climate change – and present one of the world’s most pressing challenges.A changing climate has a range of potential ecological, physical, and health impacts, including extreme weather events (such as floods, droughts, storms, and heatwaves); sea-level rise; altered crop growth; and disrupted water systems. Resulting in climate change being on of the biggest contributers to change the new world. One way to change the world for the better is to decrease CO2 emmisions which is what we are looking at today."),
  p("The main outcome of this project is to show the erlationship between countries with a population over 710 million and the ammount of their CO2 they release. The country withh the max population was China with 1.424 billion, however they had 7.689 CO2 per capita. The lowest population is in Thailand with 7.14 million and there CO2 emissions are 3.81. THe averages for population and CO2 emissions per capita are 280 million and 0.028 CO2 emitted."),
)


page_two <- tabPanel(
  "Data",
  
  titlePanel("How does Population correlate with CO2 emissions per capita?"),
  p("Controls"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Year:",
                  min = 1858, max = 2020, value = 2020,
                  ticks = FALSE, sep = ""),
      selectInput(inputId = "pop_or_co2", label = "Change Variable",
                  choices = c("CO2 per Capita" = "co2_per_capita","Population" = "population"))
    ),
    mainPanel(
      plotlyOutput('chart_1'),
      p("This chart shows the relationship between populous countries with populations over 710 million and their CO2 per capita. From the information on the chart there does not seem to be a large correlation between high CO2 per capita and population. The chart is organized in descending order of CO2 per capita for each country; However, when you switch the graph to population, there seems to be zero correlation at all.")
    )
  )
)


ui <- (
  fluidPage(
    navbarPage(
      "Harry Hall A5: Data Application", 
      page_one,         
      page_two
    )
  )
)

shinyApp(ui = ui, server = server)

