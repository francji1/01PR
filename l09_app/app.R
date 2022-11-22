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

country_index = str_sub(df$geo, end = 2) %>% unique()

map_render = get_eurostat_geospatial(resolution = 10, 
                              nuts_level = 2, 
                              year = 2016) %>% 
  arrange(geo) %>% 
  st_as_sf()


# Source helper functions -----
source("helpers.R")


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
                    choices = country_index,
                    selected = 1),
        
        checkboxGroupInput("sex_group", label = "Select sex group", 
                           choices = list("F" = "F", "M" = "M", "T" = "T"),
                           selected = "T"),
        
        sliderInput("yearslider", 
                    label = "Range of years:",
                    min = 1990, max = 2030, value = c(2015, 2020)),
        
        sliderInput("x_lim", 
                    label = "Range of x:",
                    min = -10, max = 35, value = c(-10, 35)),
        
        sliderInput("y_lim", 
                    label = "Range of y:",
                    min = 35, max = 65, value = c(35, 65))
      ),
      mainPanel(h2("Visualisation:", align = "center"),
                textOutput("selected_country"),
                h3("Line:"),
                plotOutput("lines"),
                h3("Map All Countries:"),
                plotOutput("map_all"),
                h3("Map Selected Country:"),
                plotOutput("map_country"))
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$selected_country <- renderText({ 
    paste("You have selected country:", input$country, "and time range from", input$yearslider[1], "to", input$yearslider[2], "and sex group", input$sex_group[1],input$sex_group[2],input$sex_group[3])
  })
  
  output$lines <- renderPlot({
   
    country_name = input$country
    country_names = df$geo[startsWith(df$geo, country_name)] %>% unique()
    
    df_country <- df %>% 
      filter(age == "Y_LT1",
             geo %in% country_names,
             sex %in% input$sex_group,
             time %in% c(input$yearslider[1]:input$yearslider[2])) %>% 
      mutate_if(is.character,as.factor)
    
    ggplot(df_country, aes(x=time, y=values, color=geo)) + 
          geom_line(aes(linetype = sex))
  })
  
  output$map_all <- renderPlot({
    
    df_country_all = filter_df(df,
                               input_age = "Y_LT1",
                               input_sex = input$sex_group,
                               time_min  = input$yearslider[1],
                               time_max  = input$yearslider[2]
                               ) %>% 
      group_by(geo) %>% 
      summarise(mean_value = mean(values))
    
    df_eu_shp <-  map_render %>% 
      left_join(df_country_all, by = "geo") %>% 
      st_as_sf()
    
    
    df_eu_shp %>% 
      ggplot(aes(fill = mean_value)) +
      scale_fill_continuous(type = "viridis") +
      geom_sf() +
      scale_x_continuous(limits = c(input$x_lim[1], input$x_lim[2])) +
      scale_y_continuous(limits = c(input$y_lim[1], input$y_lim[2])) +
      theme_void()
    
  })
  
  output$map_country <- renderPlot({
    
    
    
    country_name = input$country
    country_names = df$geo[startsWith(df$geo, country_name)] %>% unique()
    
    df_country <- df %>% 
      filter(age == "Y_LT1",
             geo %in% country_names,
             sex %in% input$sex_group,
             time %in% c(input$yearslider[1]:input$yearslider[2])) %>% 
      group_by(geo) %>% 
      summarise(mean_value = mean(values))
    
    df_eu_shp <-  map_render %>% 
      left_join(df_country, by = "geo") %>% 
      st_as_sf()
    

    df_eu_shp %>% 
      ggplot(aes(fill = mean_value)) +
      scale_fill_continuous(type = "viridis") +
      geom_sf() +
      scale_x_continuous(limits = c(input$x_lim[1], input$x_lim[2])) +
      scale_y_continuous(limits = c(input$y_lim[1], input$y_lim[2])) +
      theme_void()
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
