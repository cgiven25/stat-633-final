library(plotly)
library(tidyverse)
library(shiny)


MEASURE_TO_READ = "FOODSCARCE"
MEASURE_TO_READ2 = "LONGCOVID_1"


cols <- read.csv("time_series.csv", nrows = 1, as.is = T, header = F)
df <- read.csv("time_series.csv", skip = 2, header = F)
colnames(df) <- cols

# add state codes needed for choropleth map (https://www.kaggle.com/datasets/francescopettini/us-state-names-codes-and-abbreviations)
states <- read.csv("state_names.csv") %>%
  select(State, code = Alpha.code)

# ----- CLEANING -----
df$RATE <- as.numeric(df$RATE)
df$RATE_MOE <- as.numeric(df$RATE_MOE)
df$TOTAL <- as.numeric(df$TOTAL)
df$TOTAL_MOE <- as.numeric(df$TOTAL_MOE)
df$UNIVERSE <- as.numeric(df$UNIVERSE)

# lots of repeated data, long strings. refer to column description spreadsheet or comment out if needed
df$MEASURE_DESCRIPTION <- NULL
# df$MEASURE_LABEL <- NULL
df$NAME <- NULL # same as GEO_NAME, GEO_NAME is more descriptive
df[,"NA"] <- NULL

df$MEASURE_NAME <- factor(df$MEASURE_NAME)
df$GEO_NAME <- factor(df$GEO_NAME)
df$CYCLE <- ordered(df$CYCLE, levels = 1:6)

# 37 observations dropped (0.93%)
df.complete <- tibble(df[complete.cases(df),])

# ----- ISOLATING FOOD SCARCITY -----
# measures:
# DISPLACED, DISPLACEDTIME, EMOTIONAL, ENERGYBILL, EVICTFOR, EXPENSE
# FOODFORCHILD1, FOODSCARCE, GASIMPACT, LONELY, LONGCOVID_1, PRINCEINCR, PRICEWORRY
scarcity <- df.complete %>%
  filter(MEASURE_NAME == MEASURE_TO_READ) %>%
  select(GEO_NAME, RATE, RATE_MOE, TOTAL, TOTAL_MOE, UNITS_TOTAL, UNIVERSE, COL_START_DATE, COL_END_DATE, CYCLE) %>%
  rowwise() %>%
  mutate(CODE = states[states$State == GEO_NAME, "code"])

ui <- fluidPage(
  titlePanel("Food Scarcity Visualization"),
  
  sidebarLayout(
    
    # sidebar panel
    sidebarPanel(
      
      # will definitely need to make this easier to understand if we include it
      # # multiple cycle mean
      sliderInput(
         "cycle",
         "Collection cycles to include in mean calculation (lower is earlier)",
         min = 1, max = 6,
         value = c(1, 6)
       ),
      
      # single cycle value
      #sliderInput(
      #  "cycle",
      #  "Collection cycle",
      #  min = 1, max = 6,
      #  value = 1
      #)
      
    ),
    
    # main panel
    mainPanel(
      plotlyOutput("map"),
      plotlyOutput("map2")
    )
    
  ),
)

server <- function(input, output) {
  scarcity <- df.complete %>%
    filter(MEASURE_NAME == MEASURE_TO_READ) %>%
    select(GEO_NAME, RATE, RATE_MOE, TOTAL, TOTAL_MOE, UNITS_TOTAL, UNIVERSE, COL_START_DATE, COL_END_DATE, CYCLE) %>%
    rowwise() %>%
    mutate(CODE = states[states$State == GEO_NAME, "code"])
  
  output$map <- renderPlotly({
    scarcity.rate <- scarcity %>%
      filter(CYCLE %in% input$cycle) %>%
      group_by(CODE) %>%
      summarize(rate = mean(RATE))
  
  g <- list(
    scope = "usa",
    projection = list(type = "albers usa")
  )
  
  # https://plotly.com/r/builtin-colorscales/
  plot_geo(scarcity.rate, locationmode = "USA-states", reversescale = T) %>%
    add_trace(
      z = ~rate,
      locations = ~CODE,
      color = ~rate,
      colors = "RdYlGn"
    ) %>%
    colorbar(title = "Mean Food Scarcity (%)", 
             side = "top",
             limits = c(min(scarcity$RATE), max(scarcity$RATE))) %>%
    layout(
      title = "Mean Food Scarcity Rate by State",
      geo = g
    )
  })
  
  output$map2 <- renderPlotly({
    scarcity.rate <- scarcity %>%
      filter(CYCLE %in% input$cycle) %>%
      group_by(CODE) %>%
      summarize(rate = mean(RATE))
    
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
    
    # https://plotly.com/r/builtin-colorscales/
    plot_geo(scarcity.rate, locationmode = "USA-states", reversescale = T) %>%
      add_trace(
        z = ~rate,
        locations = ~CODE,
        color = ~rate,
        colors = "RdYlGn"
      ) %>%
      colorbar(title = "Mean Food Scarcity (%)", 
               side = "top",
               limits = c(min(scarcity$RATE), max(scarcity$RATE))) %>%
      layout(
        title = "Mean Food Scarcity Rate by State",
        geo = g
      )
  })
}

shinyApp(ui, server)
