
library(plotly)
library(dplyr)
setwd("/Users/marisapatel/Documents/STAT633/Final")

#Upload Data:
food_insecurity_data <- read.csv("./foodsecurity-state-2024.csv")
SNAP_data <- read.csv("./SNAP_state_participation_rate_map_2018-2024_RUpload.csv")
SNAP_data$State <- state.abb[match(SNAP_data$State, state.name)]
grocery_price_data <- read.csv("./Grocery_state_sales_per_capita_1997-2024_RUpload.csv")

#rename grocery data columns:

grocery_price_data <- grocery_price_data %>%
  rename(
    food_home_nominal = "Food.at.home.per.capita.sales..nominal.U.S..dollars.",
    food_away_nominal = "Food.away.from.home.per.capita.sales..nominal.U.S..dollars.",
    food_total_nominal = "Total.food.per.capita.sales..nominal.U.S..dollars.",
    food_home_constant = "Food.at.home.per.capita.sales..constant.U.S..dollars..1988.100..",
    food_away_constant = "Food.away.from.home.per.capita.sales..constant.U.S..dollars..1988.100..",
    food_total_constant = "Total.food.per.capita.sales..constant.U.S..dollars..1988.100.." 
  )
colnames(grocery_price_data)
head(grocery_price_data)


#Create map from food insecurity in 2024:


food_insecurity_2024 <- food_insecurity_data %>%
  filter(Year == "2022-2024") %>%
  select(2,3)

food_insecurity_2020 <- food_insecurity_data %>%
  filter(Year == "2018-2020") %>%
  select(2,3)

plot_ly(
  data = food_insecurity_2024,
  type = "choropleth",
  locations = ~State,
  locationmode = "USA-states",
  z = ~Food.insecurity.prevalence,
  colorscale = "Viridis",
  colorbar = list(title = "Food Insecurity Rate 2024")
) %>%
  layout(
    geo = list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
  )


plot_ly(
  data = food_insecurity_2020,
  type = "choropleth",
  locations = ~State,
  locationmode = "USA-states",
  z = ~Food.insecurity.prevalence,
  colorscale = "Viridis",
  colorbar = list(title = "Food Insecurity Rate 2020")
) %>%
  layout(
    geo = list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
  )


SNAP_2024 <- SNAP_data %>%
  select(1,8)

SNAP_2020 <- SNAP_data %>%
  select(1,4)


plot_ly(
  data = SNAP_2024,
  type = "choropleth",
  locations = ~State,
  locationmode = "USA-states",
  z = ~X2024,
  colorscale = "Viridis",
  colorbar = list(title = "SNAP Usage 2024")
) %>%
  layout(
    geo = list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
  )

plot_ly(
  data = SNAP_2020,
  type = "choropleth",
  locations = ~State,
  locationmode = "USA-states",
  z = ~X2020,
  colorscale = "Viridis",
  colorbar = list(title = "SNAP Usage 2020")
) %>%
  layout(
    geo = list(
      scope = "usa",
      projection = list(type = "albers usa")
    )
  )


#Charts for slides:

#Plot 1: SNAP benefit over time, US level
library(tidyr)
library(dplyr)

snap_long <- SNAP_data %>%
  pivot_longer(
    cols = `X2018`:`X2024`,
    names_to = "Year",
    values_to = "SNAP_Usage"
  )

snap_long$Year <- as.numeric(sub("X", "", snap_long$Year))

table(snap_long$SNAP_Usage)

#Cohort the snap usage into bins
snap_long <- snap_long %>%
  mutate(
    cohort = cut(
      SNAP_Usage,
      breaks = seq(0, 100, by = 5),
      include.lowest = TRUE,
      right = FALSE,
      labels = paste0(seq(0, 95, by = 5), "-", seq(5, 100, by = 5), "%")
    )
  )

#Count states in each cohort
library(dplyr)

cohort_counts <- snap_long %>%
  group_by(Year, cohort) %>%
  summarise(n_states = n(), .groups = "drop")

#Convert to proportions for each year
cohort_props <- cohort_counts %>%
  group_by(Year) %>%
  mutate(prop = n_states / sum(n_states))


#Adjust the colors:
library(scales)
unique(cohort_props$cohort)
cohort_levels <- unique(cohort_props$cohort)

n <- length(cohort_levels)


base_colors <- colorRampPalette(c(
  "darkgreen",
  "gold",
  "orange",
  "red"
))(n)

color_map <- setNames(base_colors, cohort_levels)

#Plot
plot_ly(
  cohort_props,
  x = ~Year,
  y = ~prop,
  color = ~cohort,
  colors = color_map,
  type = "bar"
) %>%
  layout(
    barmode = "stack",
    title = "SNAP Participation Distribution Across States",
    yaxis = list(title = "Proportion of States", tickformat = ".0%"),
    xaxis = list(title = "Year")
  )

#Plot 2: Indexed grocery spending by state

#Change the spending values to numeric and get rid of commas
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

str(grocery_data)

#Create US Average - using mean across states
avg <- grocery_data %>%
  group_by(Year) %>%
  summarise(index = mean(index, na.rm = TRUE))

#Create chart

plot_ly() %>%
  add_lines(
    data = grocery_data,
    x = ~Year,
    y = ~index,
    color = ~State,
    opacity = 0.2,
    showlegend = FALSE
  ) %>%
  add_lines(
    data = avg,
    x = ~Year,
    y = ~index,
    line = list(color = "black", width = 4),
    name = "US Average"
  ) %>%
  layout(
    title = "Grocery Spending Growth Indexed to 2019 (COVID Baseline)",
    yaxis = list(title = "Index (2019 = 100)")
  ) %>%
layout(
  shapes = list(
    list(
      type = "line",
      x0 = min(grocery_data$Year),
      x1 = max(grocery_data$Year),
      y0 = 100,
      y1 = 100,
      line = list(
        color = "black",
        width = 1,
        dash = "dot"
      )
    )
  )
) 
