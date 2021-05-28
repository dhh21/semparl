#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(sf)
library(dplyr)
library(stringr)

# Load data ----
map_coordinates <- readRDS("map_coordinates.rds")
not_city <- readRDS("not_city.rds") # municipalities excluded in the study


# Define UI for application 
ui <- fluidPage(
    
    # Application title
    titlePanel("DHH2021 Semparl: Cities in Parliament"),
    
    # Input sidebar
    sidebarLayout(
        sidebarPanel(
            selectInput("period", label = h3("Select time period"), 
                        choices = list("Depression in Finland (1986-1995)" = "1986-1995", 
                                       "Global Financial Crisis (2004-2013)" = "2004-2013"), 
                        selected = 1),
            
            
            radioButtons("values", label = h3("Show values"),
                         choices = list("Absolute" = "mention_count", 
                                        "Per capita" = "per_capita", 
                                        "Rate per 100 000" = "rate_per_100K"), 
                         selected = "mention_count"),
            
            sliderInput("year",
                        "Year",
                        min = 1986,
                        max = 1995,
                        value = 1990,
                        sep = ""),
            
            
            width = 2
        ),
        
        # Show a plot and a table
        mainPanel(
            
            fluidRow(
                column(width = 5, plotOutput("map_plot")),
                column(width = 5, DTOutput('table'))
                
            ),
            
            width = 10
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observeEvent(input$period, {
        if(input$period == "2004-2013"){
            updateSliderInput(inputId = "year", min = 2004, max = 2013)
        } else {
            updateSliderInput(inputId = "year", min = 1986, max = 1995)
            
        }
    })
    
    
    
    selected_data <- reactive({
        map_coordinates %>% 
            filter(period == input$period,
                   year == input$year) 
        
        
    })
    
    
    table_data <- reactive({
        as.data.frame(map_coordinates) %>%
            filter(period == input$period, year == input$year) %>%
            select(-geom, -year, -period) %>%
            mutate(per_capita = round(per_capita, 5),
                   rate_per_100K = round(rate_per_100K, 2),
                   population = ifelse(population == 0, NA, population)) %>%
            arrange_at(desc(input$values)) %>%
            dplyr::rename_with(str_to_title)
    })
    
    
    theme_set(
        theme_minimal(base_family = "Arial") +
            theme(legend.position= "none",
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  panel.grid = element_blank()
            )
    )
    
    global_limits <- reactive({
        
        c(min(as.data.frame(map_coordinates)[map_coordinates$period == input$period, input$values],  na.rm = TRUE),
          max(as.data.frame(map_coordinates)[map_coordinates$period == input$period, input$values],  na.rm = TRUE))   
        
    })
    
    
    output$map_plot <- renderPlot({
        ggplot() +
            geom_sf(data = not_city, col = alpha("white", 1/3), fill = "grey") +
            geom_sf(data = selected_data(), aes_string(fill = input$values, geometry = "geom"), colour = alpha("white", 1/3)) +
            viridis::scale_fill_viridis(limits = global_limits()) +
            labs(title = paste("Year", input$year, input$values),
                 subtitle = "Cities mentioned in plenary sessions of\nFinnish parliament",
                 caption = "Population data: Statistics Finland (2021)",
                 fill = "No. mentions in\nplenary sessions") +
            theme(legend.position = "left",
                  plot.title = element_text(size=36))
    }, height = 900)
    
    output$table <- renderDT(DT::datatable(table_data(), 
                                           rownames = FALSE,
                                           options = list(order = list(list(2, 'desc')))))
}

# Run the application 
shinyApp(ui = ui, server = server)
