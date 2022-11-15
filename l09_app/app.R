#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# runApp("l09_app")
# R  -e "library(shiny); shiny::runGitHub('01PR', 'francji1', subdir = 'l09_app', launch.browser = TRUE)"


# Load packages ----
library(shiny)
library(tidyverse)
library(Rcpp)
library(sf)
library(scales)
library(leaflet)
library(cowplot)
library(ggthemes)
library(eurostat)

# Load data ----
df <- get_eurostat("demo_r_mlifexp", time_format = "num")

# Source helper functions -----
#source("helpers.R")


# Process data ----


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Eurostat analysis"),
    # Sidebar with a slider input for number of bins 

    sidebarLayout(
      sidebarPanel(
        helpText(img(src = "index.png", height = 100, width = 200)),
        h2("Data input:", align = "center"),
        selectInput("country", 
                    label = "Choose a country",
                    choices = list("CZ", 
                                   "SK",
                                   "DE"),
                    selected = 1),
        
        sliderInput("yearslider", 
                    label = "Range of years:",
                    min = 1990, max = 2030, value = c(2015, 2020))
      ),
      mainPanel(h2("Visualisation:", align = "center"),
                textOutput("selected_country"),
                h3("Line:"),
                plotOutput("lines"),
                h3("Map:"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$selected_country <- renderText({ 
    paste("You have selected country:", input$country, "and time range from", input$yearslider[1], "to", input$yearslider[2])
  })
  
  output$lines <- renderPlot({
    country_name = input$country
    country_names = df$geo[startsWith(df$geo, country_name)] %>% unique()
    
    df_country <- df %>% 
      filter(age == "Y_LT1",
             geo %in% country_names,
             sex %in% c("F","M")) %>% 
      mutate_if(is.character,as.factor)
    
    ggplot(df_country, aes(x=time, y=values, color=geo)) + 
          geom_line(aes(linetype = sex))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
