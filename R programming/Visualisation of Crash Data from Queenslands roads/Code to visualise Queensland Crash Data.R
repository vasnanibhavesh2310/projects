# Loading the necessary libraries -----------------------------------------------------------------------
library (tidyverse) 

# Loading the data into a df named crash_data -----------------------------------------------------------
crash_data <- read.csv('crash_incidents_clean.csv')

# Code for visualization --------------------------------------------------------------------------------

# Visualization 1 : Time Series Chart of Crashes
crash_data %>%
  mutate(YearMonth = as.Date(paste(Crash_Year, Crash_Month, "01"), "%Y %B %d")) %>%
  count(YearMonth) %>%
  ggplot(aes(x = YearMonth, y = n)) +
  geom_line() +
  geom_smooth(colour = 'purple', se = FALSE) +
  geom_point(alpha = 0.5, colour = 'red', shape = 19, size = 1.5) +
  labs(
    title = "Monthly Road-Crash Count in Queensland",
    x     = "Year",
    y     = "Number of Crashes"
  ) +
  theme_minimal(base_size = 25) 

# Visualization 2: Geographical distribution of road crashes
library(ggmap)

# Google API key 
register_google(key="Enter you API key here")


# Geo code Queensland's location
qld <- geocode("Queensland, Australia")

# First approach: plotting all the points
ggmap(get_map(qld, zoom=5, maptype = 'roadmap')) +
  geom_point(data = crash_data, mapping=aes(x=Crash_Longitude, y=Crash_Latitude, colour= Crash_Severity), 
             alpha = 0.2, size=1) +
  labs(
    title = "Geographic Distribution of Road Crashes in Queensland",
    subtitle = "Highlighted by Crash Severity",
    x = "Longitude",
    y = "Latitude",
    colour = "Crash Severity"
  ) +
  theme_minimal(base_size = 25) +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(override.aes = list(size = 5)))  

# Second approach: Grouping all the crashes by Local Government Area

# Group crash data by Loc_Local_Government_Area
grouped_data <- crash_data %>%
  group_by(Loc_Local_Government_Area) %>%
  summarise(
    avg_latitude = mean(Crash_Latitude, na.rm = TRUE),
    avg_longitude = mean(Crash_Longitude, na.rm = TRUE),
    crash_count = n()
  )


# Creating the map with geom_point
ggmap(get_map(qld, zoom = 5, maptype = 'roadmap')) +
  geom_point(data = grouped_data, aes(x = avg_longitude, y = avg_latitude, size = crash_count, colour = crash_count), 
             alpha = 0.7) +
  scale_size_continuous(range = c(2, 10)) +  # Adjust point size based on crash count
  scale_colour_gradient(low = "blue", high = "red") +  # Colour scale for crash count
  labs(
    title = "Geographic Distribution of Road Crashes in Queensland",
    subtitle = "Crash Points Grouped by Local Government Area",
    x = "Longitude",
    y = "Latitude",
    size = "Crash Count",
    colour = "Crash Count"
  ) +
  theme_minimal(base_size = 25) +
  theme(
    legend.position = "bottom",            # Place the legend at the bottom
    legend.box.spacing = unit(1, "cm"),    # Add space around the legend box
    legend.key.size = unit(1, "cm"),        # Adjust the size of the legend keys (the symbols)
    legend.spacing.x = unit(0.5, "cm"),    # Adjust the spacing between legend items horizontally
    legend.spacing.y = unit(0.5, "cm"),     # Adjust the spacing between legend items vertically
    legend.title = element_text(size = 15), # Adjust the size of legend title
    legend.text  = element_text(size = 11)  # Adjust the size of legend text
  )

# Visualization 3: Seasonal Boxplots by month
# Compute monthly counts per year
monthly <- crash_data %>%
  count(Crash_Year, Crash_Month) %>%
  mutate(Month = factor(Crash_Month, levels = month.name))

# Box plot using ggplot
ggplot(data = monthly, aes(x = Month, y = n)) +
  geom_boxplot(aes(), fill = 'pink', alpha = 0.7 ) +
  labs(
    title = "Distribution of Monthly Crash Counts",
    x = "Month",
    y = "Crashes per Month"
  ) +
  theme_minimal(base_size = 25)  

# Visualization 4: Crash Severity and Hospitalisation by Hour of the day 
ggplot(crash_data, aes(x = Crash_Hour)) +
  geom_bar(aes(fill = Crash_Severity), position = "stack") +
  geom_line(data = crash_data %>%
              filter(Crash_Severity == "Hospitalisation") %>%
              count(Crash_Hour), aes(x = Crash_Hour, y = n), colour = "purple", size = 1) +
  labs(title = "Crash Severity and Hospitalisation by Hour of the Day", x = "Hour of the Day", y = "Crash Count") +
  theme_minimal(base_size = 25) 

# Visualization 5: Trend of Hospitalization Crashes
crash_data %>%
  filter(Count_Casualty_Hospitalised > 0) %>%
  mutate(YearMonth = as.Date(paste(Crash_Year, Crash_Month, "01"), "%Y %B %d")) %>%
  count(YearMonth) %>%
  ggplot(aes(x = YearMonth, y = n)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE, colour = 'purple') +
  labs(
    title = "Smoothed Trend of Hospitalisation Crashes",
    x     = "Date",
    y     = "Monthly Hospitalisations"
  ) +
  theme_minimal(base_size = 25) 


 

# Clean up ------------------------------------------------------------------------------------------------

# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)

