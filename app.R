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
    State = case_match(State, c("U.S. total", "U.S.") ~ "US", .default = State),
    Year = sub("–", "-", Year) # looks insane but there's an en-dash which makes it annoying to work with. replacing with hyphen
  )

us <- security %>%
  filter(State == "US")

by.state <- security %>%
  filter(State != "US")

state.codes <- sort(unique(by.state$State))

state.names <- tibble(read.csv("state_names.csv"))

snap <- tibble(read.csv("snap.csv"))
colnames(snap) <- sub("X", "", colnames(snap))
snap <- snap %>% 
  pivot_longer(!State, names_to = "Year", values_to = "Participation") %>%
  left_join(state.names[,c("State", "Alpha.code")], by = "State")
  

states.by.region <- list(
  west = c("WA", "OR", "CA", "NV",
           "ID", "MT", "WY", "UT",
           "CO", "AZ", "NM", "AK",
           "HI"),
  midwest = c("ND", "MN", "SD", "IA",
              "NE", "KS", "MO", "WI",
              "IL", "IN", "MI", "OH"),
  northeast = c("PA", "NY", "NJ", "RI",
                "CT", "MA", "NH", "VT",
                "ME"),
  south = c("TX", "OK", "AR", "LA",
            "MS", "AL", "TN", "KY",
            "GA", "SC", "FL", "NC",
            "VA", "WV", "DC", "MD",
            "DE")
)

emergency <- c(
  "ID", "ND", "AR", "FL", "MT", "NE", "MS", "AK",
  "MO", "TN", "IA", "AZ", "KY", "WY", "GA", "IN", "SC"
)

# thanks marisa!
grocery_price_data <- read.csv("Grocery_state_sales.csv")

grocery_price_data <- grocery_price_data %>%
  rename(
    food_home_nominal = "Food.at.home.per.capita.sales..nominal.U.S..dollars.",
    food_away_nominal = "Food.away.from.home.per.capita.sales..nominal.U.S..dollars.",
    food_total_nominal = "Total.food.per.capita.sales..nominal.U.S..dollars.",
    food_home_constant = "Food.at.home.per.capita.sales..constant.U.S..dollars..1988.100..",
    food_away_constant = "Food.away.from.home.per.capita.sales..constant.U.S..dollars..1988.100..",
    food_total_constant = "Total.food.per.capita.sales..constant.U.S..dollars..1988.100.." 
  )

grocery_price_data$food_total_nominal <- as.numeric(
  gsub(",", "", grocery_price_data$food_total_nominal)
)

unique(grocery_price_data$food_total_nominal)

#Create index to 2019 as pre-covid baseline (maybe change to 2018)
grocery_data <- grocery_price_data %>%
  group_by(State) %>%
  mutate(
    index = food_total_nominal / food_total_nominal[Year == 2019] * 100
  ) %>%
  ungroup()

us.grocery.avg <- grocery_data %>%
  group_by(Year) %>%
  summarise(index = mean(index, na.rm = TRUE))

grocery_data <- grocery_data %>%
  mutate(rel.to.base = index - 100) %>%
  left_join(state.names[,c("State", "Alpha.code")], by = "State") 

ui <- fluidPage(
  titlePanel("Food Security Visualization"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "year1",
        "Select a data collection period for the top map",
        setNames(unique(security$Year), 2007:2023),
        selected = "2022-2024"
      ),
      
      selectInput(
        "year2",
        "Select a data collection period for the bottom map",
        setNames(unique(security$Year), 2007:2023),
        selected = "2022-2024"
      ),
      
      # selectInput(
      #   "state",
      #   "Select a state for the graph",
      #   sort(unique(security$State[security$State != "US"])),
      #   selected = "MA"
      # ),
      
      # selectInput( 
      #   "time", 
      #   "Select time period", 
      #   c(
      #     "All years" = "all",
      #     "Pre-COVID (before 2020)" = "pre", 
      #     "During COVID (2020-2021)" = "during", 
      #     "Post-COVID (after 2021)" = "post" 
      #   ) 
      # ), 
      
      # differs from Yu's work but these are the ones that the Census Bureau uses
      checkboxGroupInput(
        "regions",
        "Select region(s) for the map",
        c(
          "Northeast" = "northeast",
          "Midwest" = "midwest",
          "South + DC" = "south",
          "West" = "west"
        ),
        selected = names(states.by.region)
      ),
      
      # does not change colors, only the limits of the colorscale.
      # subtracting off an integer does not change the distribution of food security
      checkboxInput(
        "vs.us",
        "Compare against US Baseline"
      ),
      
      checkboxInput(
        "emergency_only",
        "Show only states that opted out of SNAP Emergency Allotments"
      ),
      
      checkboxInput(
        "snap",
        "Show SNAP enrollment"
      ),
      
      checkboxInput(
        "grocery",
        "Show grocery spending (indexed against 2019)"
      )
    ),
    
    mainPanel(
      textOutput("click_instructions"),
      plotlyOutput("map_top"),
      plotlyOutput("map_bottom"),
      plotOutput("insecurity_plot"),
      textOutput("warning"),
      textOutput("snap_warning"),
      plotOutput("grocery_plot")
    )
  )
)

server <- function(input, output) {
  
  output$click_instructions <- renderText({
    "Click a state on either map to change the graph output."
  })
  
  output$map_top <- renderPlotly({
    us.prev.year <- us %>% filter(Year == input$year1) %>% pluck("Prevalence")
    
    by.region <- by.state %>%
      filter(State %in% unlist(states.by.region[input$regions], use.names = F))
    
    by.year <- by.region %>%
      filter(Year == input$year1)
    
    if (input$emergency_only) {
      by.year <- by.year %>%
        filter(State %in% emergency)
    }
    
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
    
    colorbar.lims <- quantile(security$Prevalence, c(0, 1), names = F)
    # colorbar.lims <- quantile(by.state.year$Prevalence, c(0, 1), names = F)
    
    # https://plotly.com/r/builtin-colorscales/
    plot_geo(by.year, locationmode = "USA-states", reversescale = T) %>%
      add_trace(
        z = ~(if(input$vs.us) (Prevalence - us.prev.year) else Prevalence),
        locations = ~State,
        color = ~(if(input$vs.us) (Prevalence - us.prev.year) else Prevalence),
        colors = "RdYlGn"
      ) %>%
      colorbar(title = if(input$vs.us) "Difference from US average measurement (%)" else "Mean Food Insecurity Prevalence (%)", 
               side = "top",
               limits = if(input$vs.us) (colorbar.lims - us.prev.year) else colorbar.lims) %>%
               # limits = c(min(by.state.year$Prevalence), max(by.state.year$Prevalence))) %>%
      layout(
        title = paste0("Food Insecurity Rate by State (", input$year1, ")"),
        geo = g
      )
    
  })
  
  output$map_bottom <- renderPlotly({
    us.prev.year <- us %>% filter(Year == input$year2) %>% pluck("Prevalence")
    
    by.region <- by.state %>%
      filter(State %in% unlist(states.by.region[input$regions], use.names = F))
    
    by.year <- by.region %>%
      filter(Year == input$year2)
    
    if (input$emergency_only) {
      by.year <- by.year %>%
        filter(State %in% emergency)
    }
    
    g <- list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
    
    colorbar.lims <- quantile(security$Prevalence, c(0, 1), names = F)
    # colorbar.lims <- quantile(by.state.year$Prevalence, c(0, 1), names = F)
    
    # https://plotly.com/r/builtin-colorscales/
    plot_geo(by.year, locationmode = "USA-states", reversescale = T) %>%
      add_trace(
        z = ~(if(input$vs.us) (Prevalence - us.prev.year) else Prevalence),
        locations = ~State,
        color = ~(if(input$vs.us) (Prevalence - us.prev.year) else Prevalence),
        colors = "RdYlGn"
      ) %>%
      # decision needs to be made about colorbar:
      # do we want maps for different years to be directly comparable?
      # i.e. should the range of colors be based on the max/min for the entire dataset or just for the year?
      # latter makes it easier to tell which states in a given year are least/most food insecure
      # former makes it easier to compare the maps (if a state changes color from year-to-year, the change in insecurity rate is directly interpretable from the color change)
      colorbar(title = if(input$vs.us) "Difference from US average measurement (%)" else "Mean Food Insecurity Prevalence (%)", 
               side = "top",
               limits = if(input$vs.us) (colorbar.lims - us.prev.year) else colorbar.lims) %>%
      # limits = c(min(by.state.year$Prevalence), max(by.state.year$Prevalence))) %>%
      layout(
        title = paste0("Food Insecurity Rate by State (", input$year2, ")"),
        geo = g
      )
    
  })
  
  output$insecurity_plot <- renderPlot({
    click_data <- event_data("plotly_click")
    
    state <- if (is.null(click_data)) "MA" else {
      state.codes[click_data[1, "pointNumber"] + 1]
    }
      
    state.data <- by.state %>%
      filter(State == state)
    
    ylims <- c(0, max(state.data$Prevalence))
    
    line_graph <- ggplot(data = state.data, aes(x = Year)) + 
      geom_point(aes(y = Prevalence)) +
      geom_line(aes(y = Prevalence, group = 1, color = "Food insecurity")) +
      # geom_point(aes(y = VeryLowPrevalence)) + 
      # geom_line(aes(y = VeryLowPrevalence, group = 2, color = "Very low food security")) +
      labs(title = paste0(
          as.character(state.names %>% filter(Alpha.code == state) %>% select(State)), 
          " Food Insecurity", 
          ifelse(input$snap, "/SNAP participation", ""),
          ifelse(input$grocery, "/grocery spending", ""),
          ": 2007-2023"
        )
      )
    
    # these colors will need to be changed, i think. i wanted to group them by color but they might be too low contrast
    colors <- c("Food insecurity" = "orangered1", "Very low food security" = "orchid")
    
    if (input$vs.us) {
      colors <- c(colors, "US average food insecurity" = "orangered4", "US average very low food security" = "orchid4")
      ylims <- c(0, max(ylims, us$Prevalence))

      line_graph <- line_graph +
        geom_point(aes(y = us$Prevalence)) +
        geom_line(aes(y = us$Prevalence, group = 3, color = "US average food insecurity"))
        # geom_point(aes(y = us$VeryLowPrevalence)) +
        # geom_line(aes(y = us$VeryLowPrevalence, group = 4, color = "US average very low food security"))
    }
    
    # add snap data (it's kind of rough, don't have full years)
    # TODO: add notice that we don't have SNAP data before 2018
    if (input$snap) {
      colors <- c(colors, "SNAP participation" = "green")
      ylims <- c(0, max(ylims, snap[snap$Alpha.code == state,]$Participation))
      
      strsplit(state.data$Year, "-") %>%
        lapply(function(y) {
          m <- mean(as.integer(y))
          snap %>% filter((Alpha.code == state) & (Year == m)) %>% pluck("Participation")
        }) -> years
      
      years <- as.numeric(years)
      years <- data.frame(Year = 2008:2024, snap=years)
      
      line_graph <- line_graph +
        geom_point(aes(y = years$snap)) +
        geom_line(aes(y = years$snap, group = 5, color = "SNAP participation")) +
        geom_vline(xintercept = "2017-2019", linetype = "dashed")
    }
    
    if (input$grocery) {
      colors <- c(colors, "Grocery spending" = "purple")
      g.state <- grocery_data %>%
        filter(Alpha.code == state) %>%
        mutate(rel.to.base = lag(rel.to.base))
      
      grocery.y <- c(rep(NA, 10), g.state$rel.to.base)

      ylims <- c(min(0, g.state$rel.to.base), max(ylims, g.state$rel.to.base))
      
      line_graph <- line_graph + 
        geom_point(aes(y = grocery.y)) + 
        geom_line(aes(y = grocery.y, group = 6, color = "Grocery spending")) +
        geom_vline(xintercept = "2017-2019", linetype = "dashed")
    }
    
    # covid lines
    line_graph <- line_graph +
      geom_vline(xintercept = "2018-2020", color = "red", linetype = "dashed") +
      geom_vline(xintercept = "2020-2022", color = "red", linetype = "dashed")
    
    line_graph <- line_graph +
      scale_color_manual(name = "Legend", values = colors) +
      ylim(ylims)
    
    x_labels <- unlist(lapply(strsplit(state.data$Year, "-"), function(y) {
      mean(as.integer(y))
    }))
    
    line_graph <- line_graph +
      scale_x_discrete(labels = x_labels) +
      theme(text = element_text(size = 25)) +
      ylab("Percentage (%)")
    
    line_graph
  })
  
  output$warning <- renderText({
    ifelse((input$snap | input$grocery),
           "Note: We have SNAP participation/grocery spending data only for the years 2018-2023",
           "")
  })
  
  output$snap_warning <- renderText({
    click_data <- event_data("plotly_click")
    
    state <- if (is.null(click_data)) "MA" else {
      state.codes[click_data[1, "pointNumber"] + 1]
    }
    
    ifelse(input$snap & state == "DC",
    "Note: No SNAP data for DC",
    "")
  })
  
  # output$grocery_plot <- renderPlot({
  #   click_data <- event_data("plotly_click")
  #   
  #   state <- if (is.null(click_data)) "MA" else {
  #     state.codes[click_data[1, "pointNumber"] + 1]
  #   }
  #   
  #   g.state <- grocery_data %>%
  #     filter(Alpha.code == state)
  #   
  #   ggplot(data = g.state, aes(x = Year, y = rel.to.base)) + 
  #     geom_point() +
  #     geom_line() +
  #     geom_vline(xintercept = 2018, linetype = "dashed") + 
  #     xlim(c(2008, 2023))
  # })
}

shinyApp(ui, server)

