#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ehlib)

data <- readRDS("./data/dataset.rds")

choices <- names(data)[-(1:2)]

SPI_plot <- function(data, catagory, range) {
  var <- rlang::sym(catagory)
  data %>% 
    filter(!is.na(!!var)) %>%
    mutate(Country = fct_reorder(as.factor(Country), !!var)) %>%
    arrange(Country) %>%
    slice(range[1]:range[2]) %>%
    ggplot(aes(x = Country, 
               y = !!var, 
               fill = Country)) +
    geom_col() +
    scale_fill_rep(values = c("#72CAD8", "#E0944E", "#A7C06B")) +
    guides(fill = "none") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = NULL, y = NULL, title = catagory)
}

# Define UI for application that draws a histogram
ui <- bootstrapPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),

   selectInput(inputId = "catagory",
               label = "Catagory:",
               choices = choices,
               selected = "Social Progress Index"),
   
   sliderInput("range", "Range:",
               min = 1, max = 236,
               value = c(1, 236)),
   
   plotOutput("Plot")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
     
      # draw the histogram with the specified number of bins
    SPI_plot(data, input$catagory, input$range)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
