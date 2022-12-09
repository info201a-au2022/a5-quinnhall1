server <- function(input, output){
  output$chart_1 <- renderPlotly({
    chart <- barplot_data %>%
      filter(year == input$year) %>% 
      ggplot(aes(x= reorder(iso_code, -co2_per_capita), y= !!as.name(input$pop_or_co2))) +
      xlab('Country') +
      ylab('CO2 Per Capita/Population') +
      geom_bar(stat="identity") + 
      theme(axis.title.y = element_text(margin=margin(r=20)), axis.title.x = element_text(margin=margin(t=20)))
    chart
  })
}
