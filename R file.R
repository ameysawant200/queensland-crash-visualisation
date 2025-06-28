library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(ggplot2)
library(dplyr)
library(maps)
library(ggmap)


crashes <- read_csv("C:/Users/sawan/Downloads/crash_incidents.csv")

#Data cleaning
colnames(crashes)
monthly_trend <- crashes %>%
  group_by(Crash_Year, Crash_Month) %>%
  summarise(Crash_Count = n(), .groups = "drop")

# Remove rows with missing location data
crash_geo <- crashes %>%
  drop_na(Crash_Latitude, Crash_Longitude)

#1. Time Series – Yearly Crash Trends

# Create date object 
monthly_trend$Date <- ym(paste(monthly_trend$Crash_Year, monthly_trend$Crash_Month))

# Plot
ggplot(monthly_trend, aes(x = Date, y = Crash_Count)) +
  geom_line(color = "#0073C2FF", size = 1.2) +  # Clean solid line
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    title = "Annual Road Crash Trends in Queensland",
    x = "Year", y = "Crash Count"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),  # Centered title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey85")
  )

#2. Map – Crash Distribution

# API key
register_google(key = "AIzaSyA7kr5AqR-yE8KYzOPc14t8SdP8FIWdhgo")

# map
qld_map <- get_map(location = "Queensland, Australia", zoom = 5, maptype = "terrain")

# Plot 
ggmap(qld_map) +
  geom_point(data = crashes, aes(x = Crash_Longitude, y = Crash_Latitude),
             alpha = 0.5, color = "red", size = 0.7) +
  labs(
    title = "Crash Locations in Queensland",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 12)
  )

ggsave("qld_crash_map_wide.png", width = 10, height = 6, dpi = 300)

#3. Crash Severity Overview
ggplot(crashes, aes(x = Crash_Severity, fill = Crash_Severity)) +
  geom_bar(show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Crash Severity Categories",
    subtitle = "Fatal, Hospitalised, Treated, Minor Injuries",
    x = "Crash Severity", y = "Number of Crashes"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# 4. Crash Severity by Suburb (Location)
# Step 1: Identify Top 10 Suburbs by Crash Count
top_suburbs <- crashes %>%
  group_by(Loc_Suburb) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  pull(Loc_Suburb)

# Step 2: Filter Crash Data for Top Suburbs
severity_by_suburb <- crashes %>%
  filter(Loc_Suburb %in% top_suburbs)

# Step 3: Create the Plot'
ggplot(severity_by_suburb, aes(x = Loc_Suburb, fill = Crash_Severity)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = "Crash Severity in Top 10 Suburbs",
    subtitle = "Grouped by reported suburb",
    x = "Suburb", y = "Crash Count", fill = "Crash Severity"
  ) +
  theme_minimal(base_size = 20) +  # Global base size increased
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    
    plot.title = element_text(size = 26, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 20, hjust = 0.5),
    
    legend.title = element_text(face = "bold", size = 18),
    legend.text = element_text(size = 16),
    legend.key.size = unit(1, "cm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.spacing.x = unit(0.6, 'cm'),
    legend.box.margin = margin(t = 15),
    
    plot.margin = margin(20, 20, 80, 20),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


ggsave("crash_severity_by_suburb_readable_max.png", width = 16, height = 10, dpi = 300, bg = "white")



#5. Crash Hour (Time of Day)

# Step 1: Summarise crash count by hour
hourly_crashes <- crashes %>%
  group_by(Crash_Hour) %>%
  summarise(Crash_Count = n(), .groups = 'drop') %>%
  arrange(Crash_Hour)

# Step 2: Plot as line chart
ggplot(hourly_crashes, aes(x = Crash_Hour, y = Crash_Count)) +
  geom_line(color = "#2C77B3", size = 1.5) +
  geom_point(color = "#2C77B3", size = 3) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +  # ⬅️ Clean every-2-hours
  labs(
    title = "Crashes by Hour of the Day",
    x = "Hour (0–23)", y = "Crash Count"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 14),  # Clean readable
    axis.text.y = element_text(size = 14),
    plot.margin = margin(20, 20, 20, 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )



