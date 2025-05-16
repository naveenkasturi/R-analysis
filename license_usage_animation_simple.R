# License Usage Animation (Simple Version)
# This script creates visualizations of license usage over time that can be viewed sequentially

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)

# Read the license usage data
license_data <- read.csv("license_usage_mock_data.csv", stringsAsFactors = FALSE)

# Convert timestamps to datetime objects
license_data$data_start_timestamp <- as.POSIXct(license_data$data_start_timestamp, format="%Y-%m-%d %H:%M:%S")
license_data$data_end_timestamp <- as.POSIXct(license_data$data_end_timestamp, format="%Y-%m-%d %H:%M:%S")

# Calculate session duration in hours
license_data$session_duration <- as.numeric(difftime(license_data$data_end_timestamp, 
                                                    license_data$data_start_timestamp, 
                                                    units = "hours"))

# Filter out denials (which have 0 duration)
active_sessions <- license_data %>% filter(denial == 0)
denial_data <- license_data %>% filter(denial == 1)

# Function to convert UTC to Central Time (CT)
utc_to_ct <- function(datetime) {
  with_tz(datetime, tzone = "America/Chicago")
}

# Add CT timestamps
license_data$start_time_ct <- utc_to_ct(license_data$data_start_timestamp)
license_data$end_time_ct <- utc_to_ct(license_data$data_end_timestamp)

# Add local time based on region
license_data <- license_data %>%
  mutate(
    local_offset_hours = case_when(
      region == "texas" ~ -6,
      region == "bangalore" ~ 5,
      region == "amsterdam" ~ 1,
      TRUE ~ 0
    ),
    local_offset_mins = case_when(
      region == "bangalore" ~ 30,
      TRUE ~ 0
    ),
    start_time_local = data_start_timestamp + hours(local_offset_hours) + minutes(local_offset_mins),
    end_time_local = data_end_timestamp + hours(local_offset_hours) + minutes(local_offset_mins)
  )

# Calculate region quotas from original data
region_quotas <- c(
  texas = 50,
  bangalore = 50,
  amsterdam = 20
)
total_licenses <- sum(region_quotas)

# Create a sequence of time points for animation (hourly intervals)
start_date <- min(license_data$data_start_timestamp)
end_date <- max(license_data$data_end_timestamp)

# For a smoother animation, use more frequent time points (every hour)
time_seq <- seq(from = floor_date(start_date, "hour"),
                to = ceiling_date(end_date, "hour"),
                by = "hour")

# Create a data frame to store active sessions at each time point
animation_data <- data.frame(
  timestamp = time_seq,
  timestamp_ct = utc_to_ct(time_seq)
)

# Function to count active sessions at a given timestamp
count_active_sessions <- function(timestamp, region = NULL) {
  if (is.null(region)) {
    sum(license_data$data_start_timestamp <= timestamp & 
          license_data$data_end_timestamp > timestamp &
          license_data$denial == 0)
  } else {
    sum(license_data$data_start_timestamp <= timestamp & 
          license_data$data_end_timestamp > timestamp &
          license_data$region == region &
          license_data$denial == 0)
  }
}

# Count active sessions by time point and region
animation_data$total_active <- sapply(animation_data$timestamp, count_active_sessions)
animation_data$texas_active <- sapply(animation_data$timestamp, function(ts) count_active_sessions(ts, "texas"))
animation_data$bangalore_active <- sapply(animation_data$timestamp, function(ts) count_active_sessions(ts, "bangalore"))
animation_data$amsterdam_active <- sapply(animation_data$timestamp, function(ts) count_active_sessions(ts, "amsterdam"))

# Calculate percentage of total licenses used
animation_data <- animation_data %>%
  mutate(
    utilization = total_active / total_licenses * 100,
    date = as.Date(timestamp_ct),
    hour = hour(timestamp_ct) + minute(timestamp_ct)/60,
    weekday = wday(timestamp_ct, label = TRUE)
  )

# Filter to weekdays only for cleaner visualization
weekday_animation <- animation_data %>%
  filter(weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Create long format data for stacked area chart
animation_long <- weekday_animation %>%
  select(timestamp_ct, date, hour, weekday, 
         texas_active, bangalore_active, amsterdam_active) %>%
  pivot_longer(
    cols = c(texas_active, bangalore_active, amsterdam_active),
    names_to = "region",
    values_to = "active_count"
  ) %>%
  mutate(
    region = gsub("_active", "", region),
    region = factor(region, levels = c("amsterdam", "bangalore", "texas"))
  )

# Create a more focused dataset for visualization (one week of data)
# This makes the visualization more manageable and easier to interpret
one_week <- animation_long %>%
  filter(date >= as.Date("2025-04-15") & date <= as.Date("2025-04-19"))

# Instead of creating one animation, we'll create multiple frames at specific time points
# Create a directory to store the frames
dir.create("animation_frames", showWarnings = FALSE)

# Select specific hours to visualize (e.g., every 2 hours from 8 AM to 6 PM CT)
hours_to_visualize <- seq(8, 18, by = 2)

# Create frames for each hour on each day
for (day in unique(one_week$date)) {
  for (h in hours_to_visualize) {
    # Filter data for this specific hour
    hour_data <- one_week %>%
      filter(date == day & floor(hour) == h)
    
    if (nrow(hour_data) > 0) {
      # Create stacked bar chart for this hour
      p <- ggplot(hour_data, aes(x = weekday, y = active_count, fill = region)) +
        geom_bar(stat = "identity") +
        scale_fill_brewer(palette = "Set1", 
                         labels = c("Amsterdam", "Bangalore", "Texas")) +
        geom_hline(yintercept = total_licenses, linetype = "dashed", color = "red") +
        labs(title = "License Usage by Region",
             subtitle = paste("Date:", as.character(day), "at", h, "CT"),
             x = "Day of Week",
             y = "Active Licenses",
             fill = "Region") +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      # Save the frame
      filename <- paste0("animation_frames/usage_", format(day, "%Y%m%d"), "_", sprintf("%02d", h), "00.png")
      ggsave(filename, p, width = 8, height = 6)
    }
  }
}

# Create hourly snapshots for a single day to show the progression
# Choose a day with high activity
busy_day <- weekday_animation %>%
  group_by(date) %>%
  summarize(max_usage = max(utilization)) %>%
  arrange(desc(max_usage)) %>%
  head(1) %>%
  pull(date)

# Create hourly snapshots for the busy day
busy_day_data <- animation_long %>%
  filter(date == busy_day)

# Create a directory for the busy day frames
dir.create("busy_day_frames", showWarnings = FALSE)

# Create frames for each hour from 7 AM to 7 PM CT
for (h in 7:19) {
  # Filter data for this specific hour
  hour_data <- busy_day_data %>%
    filter(floor(hour) == h)
  
  if (nrow(hour_data) > 0) {
    # Create stacked bar chart for this hour
    p <- ggplot(hour_data, aes(x = region, y = active_count, fill = region)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette = "Set1", 
                       labels = c("Amsterdam", "Bangalore", "Texas")) +
      geom_hline(yintercept = total_licenses, linetype = "dashed", color = "red") +
      labs(title = "License Usage Simulation",
           subtitle = paste("Date:", as.character(busy_day), "at", h, "CT"),
           x = "Region",
           y = "Active Licenses",
           fill = "Region") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      annotate("text", x = 2, y = total_licenses + 10, 
               label = paste0("Total Capacity: ", total_licenses, " licenses"), 
               color = "red")
    
    # Save the frame
    filename <- paste0("busy_day_frames/usage_", sprintf("%02d", h), "00.png")
    ggsave(filename, p, width = 8, height = 6)
  }
}

# Create a heatmap showing license usage over the entire period
heatmap_data <- weekday_animation %>%
  mutate(hour_rounded = floor(hour)) %>%
  group_by(date, hour_rounded) %>%
  summarize(avg_active = mean(total_active), .groups = "drop")

# Create heatmap
p_heatmap <- ggplot(heatmap_data, aes(x = hour_rounded, y = date, fill = avg_active)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", 
                     name = "Active Licenses",
                     limits = c(0, total_licenses)) +
  labs(title = "License Usage Heatmap",
       subtitle = "Number of active licenses by hour and day",
       x = "Hour of Day (Central Time)",
       y = "Date") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal()

# Save the heatmap
ggsave("license_usage_heatmap.png", p_heatmap, width = 12, height = 8)

# Create a line chart showing utilization by hour for each day of the week
p_line <- ggplot(weekday_animation, 
                aes(x = hour, y = utilization, group = date, color = weekday)) +
  geom_line(alpha = 0.7) +
  geom_smooth(aes(group = weekday), se = FALSE, linetype = "dashed", size = 1.2) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  annotate("rect", xmin = 13, xmax = 15, ymin = 0, ymax = Inf, 
           alpha = 0.1, fill = "yellow") +
  labs(title = "License Utilization by Hour",
       subtitle = "Percentage of total licenses used throughout the day",
       x = "Hour of Day (Central Time)",
       y = "Percentage of Licenses Used (%)") +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  facet_wrap(~ weekday)

# Save the line chart
ggsave("license_utilization_by_hour.png", p_line, width = 12, height = 8)

# Create a script to combine the frames into an animated GIF (if gifski is available)
cat('
# Combine frames into an animated GIF
if (requireNamespace("gifski", quietly = TRUE)) {
  # Busy day animation
  busy_day_files <- list.files("busy_day_frames", pattern = "*.png", full.names = TRUE)
  gifski::gifski(busy_day_files, "license_usage_simulation.gif", delay = 0.5, width = 800, height = 600)
  
  # Weekly animation
  week_files <- list.files("animation_frames", pattern = "*.png", full.names = TRUE)
  gifski::gifski(week_files, "weekly_license_usage.gif", delay = 0.5, width = 800, height = 600)
  
  cat("Animation GIFs created successfully!\\n")
} else {
  cat("The gifski package is not available. You can manually review the frames in the animation_frames and busy_day_frames directories.\\n")
}
', file = "create_gifs.R")

cat("Visualization frames created! To view the simulation:\n")
cat("1. Look at the individual frames in the 'animation_frames' and 'busy_day_frames' directories\n")
cat("2. Run the 'create_gifs.R' script if the gifski package is available to create animated GIFs\n")
cat("3. Review the static visualizations: license_usage_heatmap.png and license_utilization_by_hour.png\n")
