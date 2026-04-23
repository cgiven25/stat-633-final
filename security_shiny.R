library(plotly)
library(tidyverse)
library(shiny)

# dunno why this works but it does. thanks stackoverflow
# maybe the csv was generated on a really old govt computer or something
security <- read.csv("food_security/foodsecurity-state-2024.csv", fileEncoding="windows-1252")

# prevalence is the percent of food insecurity in that state
# very low: food insecure to the extent that normal eating patterns of some household members are disrupted
#   (self-reported intake below what is considered adequate)
colnames(security) <- c("Year", "State", "Prevalence", "PrevalenceMOE", "VeryLowPrevalence", "VeryLowPrevalenceMOE")
security <- tibble(security)
security <- security %>% 
  mutate(
    State = case_match(State, c("U.S. total", "U.S.") ~ "US", .default = State)
  )

state_names <- tibble(read.csv("state_names.csv"))

ui <- fluidPage(
  titlePanel("Food Security Visualization"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        "year",
        "Select a data collection period",
        unique(security$Year),
        selected = "2022–2024"
      ),
      
      selectInput(
        "state",
        "Select a state for the graph",
        sort(unique(security$State[security$State != "US"])),
        selected = "MA"
      )
    ),
    
    mainPanel(
      plotlyOutput("map"),
      plotOutput("insecurity_plot"),
    )
  )
)

server <- function(input, output) {
  by.state <- security %>%
    filter(State != "US")
  
  output$map <- renderPlotly({
    
    # the dashes in the year are en dashes. why? i don't know
    # here is an en dash to copy/paste: –
    by.state.year <- by.state %>%
      filter(Year == input$year)
    
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
    
    # https://plotly.com/r/builtin-colorscales/
    plot_geo(by.state.year, locationmode = "USA-states", reversescale = T) %>%
      add_trace(
        z = ~Prevalence,
        locations = ~State,
        color = ~Prevalence,
        colors = "RdYlGn"
      ) %>%
      # decision needs to be made about colorbar:
      # do we want maps for different years to be directly comparable?
      # i.e. should the range of colors be based on the max/min for the entire dataset or just for the year?
      # latter makes it easier to tell which states in a given year are least/most food insecure
      # former makes it easier to compare the maps (if a state changes color from year-to-year, the change in insecurity rate is directly interpretable from the color change)
      colorbar(title = "Mean Food Scarcity (%)", 
               side = "top",
               limits = c(min(by.state.year$Prevalence), max(by.state.year$Prevalence))) %>%
      layout(
        title = paste0("Food Insecurity Rate by State (", input$year, ")"),
        geo = g
      )
    
  })
  
  output$insecurity_plot <- renderPlot({
    state.data <- by.state %>%
      filter(State == input$state)
    
    ggplot(data = state.data, aes(x = Year)) + 
      geom_point(aes(y = Prevalence)) +
      geom_line(aes(y = Prevalence, group = 1, color = "Food insecurity")) +
      geom_point(aes(y = VeryLowPrevalence)) + 
      geom_line(aes(y = VeryLowPrevalence, group = 2, color = "Very low food security")) +
      ylim(0, max(state.data$Prevalence)) +
      scale_color_manual(name = "Food security levels", values = c("Food insecurity" = "orange", "Very low food security" = "darkred")) + 
      labs(title = paste(
          as.character(state_names %>% filter(Alpha.code == input$state) %>% select(State)), 
          "Food Insecurity: 2006–2024"
        )
      )
  })
}

shinyApp(ui, server)
