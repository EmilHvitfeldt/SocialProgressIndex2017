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

limits <- apply(data[-(1:2)], 2, max, na.rm = TRUE) * 1.1

SPI_name <- function(data, catagory) {
  var <- rlang::sym(catagory)
  
  countries <- data %>% 
    filter(is.na(!!var)) %>%
    pull(Country) %>%
    sort()
  
  length_contries <- length(countries)
  if (length_contries == 1) {
    str_c("Data is unavaliable for ", countries, ".")
  }
  if (length_contries > 1) {
    str_c("Data for this catagory is unavaliable for ", 
          paste0(countries[-length(countries)], sep = ", ", collapse = ""),
          "and ", countries[length(countries)], ".")
  }

}

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
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10)) +
    labs(x = NULL, y = NULL, title = catagory) +
    ylim(0, limits[catagory])
}

# Define UI for application that draws a histogram
ui <- bootstrapPage(
   
   # Application title
   titlePanel("Social Progress Index 2017 dataviz"),
   
   sidebarPanel(
     helpText("Select a indicator from the dropdown."),
     
     selectInput(inputId = "catagory",
                 label = "Catagory:",
                 choices = choices,
                 selected = "Social Progress Index"),
     
     helpText("Filter colunms to be shown using the range to zoom."),
     
     sliderInput("range", "Range:",
                 min = 1, max = 236,
                 value = c(1, 236))
     ),
   
   mainPanel(
     helpText("This shiny application aims to present the",
              a("2017 Social Progress Index", 
                href = "http://www.socialprogressindex.com/"),
              ", in a way such that all countries can be compared within each 
              indicators."),
     helpText("Additional information regarding data descriptions can be found
               at ",
              a("2017 Social Progress Index", 
                href = "http://www.socialprogressindex.com/definitions/"),
              "."
              ),

     hr(),
     
     textOutput("selected_var")
   ),
   


   plotOutput("Plot")

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Plot <- renderPlot({
     
      # draw the histogram with the specified number of bins
    SPI_plot(data, input$catagory, input$range)
   })
  
  output$selected_var <- renderText({ 
    SPI_name(data, input$catagory)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
