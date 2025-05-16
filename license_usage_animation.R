# License Usage Animation
# This script creates an animated visualization of license usage over time

# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
library(gganimate)
library(gifski) # For rendering GIFs

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

# For a smoother animation, use more frequent time points (every 15 minutes)
time_seq <- seq(from = floor_date(start_date, "hour"),
                to = ceiling_date(end_date, "hour"),
                by = "15 mins")

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

# Create a more focused dataset for animation (one week of data)
# This makes the animation more manageable and easier to interpret
one_week <- animation_long %>%
  filter(date >= as.Date("2025-04-15") & date <= as.Date("2025-04-19"))

# Create stacked area chart animation
p_area <- ggplot(one_week, aes(x = hour, y = active_count, fill = region, group = region)) +
  geom_area(position = "stack") +
  facet_wrap(~ weekday) +
  scale_fill_brewer(palette = "Set1", 
                   labels = c("Amsterdam", "Bangalore", "Texas")) +
  geom_hline(yintercept = total_licenses, linetype = "dashed", color = "red") +
  annotate("rect", xmin = 13, xmax = 15, ymin = 0, ymax = Inf, 
           alpha = 0.2, fill = "yellow") +
  labs(title = "License Usage Simulation",
       subtitle = "Time: {frame_time}",
       x = "Hour of Day (Central Time)",
       y = "Active Licenses",
       fill = "Region") +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  scale_y_continuous(breaks = seq(0, total_licenses, 20)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Add animation
anim_area <- p_area +
  transition_time(hour) +
  ease_aes('linear') +
  enter_fade() +
  exit_fade()

# Save the animation
animate(anim_area, 
        nframes = 100, 
        fps = 10, 
        width = 800, 
        height = 500,
        renderer = gifski_renderer("license_usage_area_animation.gif"))

# Create a heatmap animation showing license usage intensity over time
p_heatmap <- ggplot(weekday_animation, 
                   aes(x = hour, y = date, fill = total_active)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", 
                     name = "Active Licenses",
                     limits = c(0, total_licenses)) +
  labs(title = "License Usage Heatmap",
       subtitle = "Simulation over time",
       x = "Hour of Day (Central Time)",
       y = "Date") +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  theme_minimal()

# Add animation to heatmap
anim_heatmap <- p_heatmap +
  transition_time(as.numeric(date)) +
  ease_aes('linear') +
  shadow_mark(past = TRUE, future = FALSE)

# Save the heatmap animation
animate(anim_heatmap, 
        nframes = 100, 
        fps = 5, 
        width = 800, 
        height = 500,
        renderer = gifski_renderer("license_usage_heatmap_animation.gif"))

# Create a line chart animation showing utilization over time
p_line <- ggplot(weekday_animation, 
                aes(x = hour, y = utilization, group = date, color = weekday)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  annotate("rect", xmin = 13, xmax = 15, ymin = 0, ymax = Inf, 
           alpha = 0.1, fill = "yellow") +
  labs(title = "License Utilization Over Time",
       subtitle = "Date: {frame_time}",
       x = "Hour of Day (Central Time)",
       y = "Percentage of Licenses Used (%)") +
  scale_x_continuous(breaks = seq(0, 24, 3)) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal()

# Add animation to line chart
anim_line <- p_line +
  transition_time(as.Date(date)) +
  ease_aes('linear') +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.3)

# Save the line chart animation
animate(anim_line, 
        nframes = 100, 
        fps = 5, 
        width = 800, 
        height = 500,
        renderer = gifski_renderer("license_utilization_animation.gif"))

# Create a bar chart race animation showing top users by active hours
user_hours <- active_sessions %>%
  group_by(user_id, region) %>%
  summarize(
    total_hours = sum(session_duration),
    avg_duration = mean(session_duration),
    session_count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(total_hours))

# Get top 10 users
top_users <- user_hours %>%
  head(10) %>%
  mutate(
    user_label = paste0(user_id, " (", region, ")"),
    user_label = factor(user_label, levels = user_label[order(total_hours)])
  )

# Create bar chart for top users
p_users <- ggplot(top_users, aes(x = user_label, y = total_hours, fill = region)) +
  geom_col() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Top Users by Total License Hours",
       x = "",
       y = "Total Hours") +
  theme_minimal()

# Save the top users chart
ggsave("top_users_by_hours.png", p_users, width = 10, height = 6)

cat("Animation complete! The following files have been created:\n")
cat("- license_usage_area_animation.gif (Stacked area chart showing license usage by region)\n")
cat("- license_usage_heatmap_animation.gif (Heatmap showing license usage intensity over time)\n")
cat("- license_utilization_animation.gif (Line chart showing utilization percentage over time)\n")
cat("- top_users_by_hours.png (Bar chart showing top users by total license hours)\n")
