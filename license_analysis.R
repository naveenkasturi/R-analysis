# License Usage Analysis
# This script analyzes license usage patterns and denial data to understand peak-hour license denials

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

# Print basic statistics
cat("Total sessions:", nrow(license_data), "\n")
cat("Active sessions:", nrow(active_sessions), "\n")
cat("Denied sessions:", nrow(denial_data), "\n")
cat("Denial rate:", round(nrow(denial_data) / nrow(license_data) * 100, 2), "%\n\n")

# 1. Session Duration Buckets Analysis
# Calculate what percentage of active sessions were active for different durations
duration_buckets <- c(1, 2, 3, 4, 5, 6, 7, 8)
duration_counts <- sapply(duration_buckets, function(hours) {
  sum(active_sessions$session_duration >= hours) / nrow(active_sessions) * 100
})

# Create a data frame for plotting
duration_df <- data.frame(
  hours = duration_buckets,
  percentage = duration_counts
)

# Plot session duration distribution
ggplot(duration_df, aes(x = factor(hours), y = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) +
  labs(title = "Percentage of Active Sessions by Duration",
       subtitle = "Shows what % of sessions were active for at least X hours",
       x = "Hours",
       y = "Percentage of Active Sessions") +
  theme_minimal()
ggsave("session_duration_distribution.png", width = 10, height = 6)

# 2. Time-Zone Alignment
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

# Extract date and hour components for analysis
license_data <- license_data %>%
  mutate(
    date_ct = as.Date(start_time_ct),
    hour_ct = hour(start_time_ct),
    weekday_ct = wday(start_time_ct, label = TRUE),
    date_local = as.Date(start_time_local),
    hour_local = hour(start_time_local),
    weekday_local = wday(start_time_local, label = TRUE)
  )

# 3. Peak-Hour Occupancy Analysis
# Calculate concurrent sessions at each hour
# First, create a sequence of hours for the entire period
start_date <- min(license_data$data_start_timestamp)
end_date <- max(license_data$data_end_timestamp)
hour_seq <- seq(from = floor_date(start_date, "hour"),
                to = ceiling_date(end_date, "hour"),
                by = "hour")

# Create a data frame with hour timestamps
hourly_data <- data.frame(timestamp = hour_seq)
hourly_data$timestamp_ct <- utc_to_ct(hourly_data$timestamp)
hourly_data$date_ct <- as.Date(hourly_data$timestamp_ct)
hourly_data$hour_ct <- hour(hourly_data$timestamp_ct)
hourly_data$weekday_ct <- wday(hourly_data$timestamp_ct, label = TRUE)

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

# Count active sessions by hour and region
hourly_data$total_active <- sapply(hourly_data$timestamp, count_active_sessions)
hourly_data$texas_active <- sapply(hourly_data$timestamp, function(ts) count_active_sessions(ts, "texas"))
hourly_data$bangalore_active <- sapply(hourly_data$timestamp, function(ts) count_active_sessions(ts, "bangalore"))
hourly_data$amsterdam_active <- sapply(hourly_data$timestamp, function(ts) count_active_sessions(ts, "amsterdam"))

# Filter to weekdays only (Monday-Friday)
weekday_data <- hourly_data %>%
  filter(weekday_ct %in% c("Mon", "Tue", "Wed", "Thu", "Fri"))

# Calculate region quotas from original data
region_quotas <- c(
  texas = 50,
  bangalore = 50,
  amsterdam = 20
)
total_licenses <- sum(region_quotas)

# Calculate percentage of licenses used
weekday_data <- weekday_data %>%
  mutate(
    total_pct = total_active / total_licenses * 100,
    texas_pct = texas_active / total_licenses * 100,
    bangalore_pct = bangalore_active / total_licenses * 100,
    amsterdam_pct = amsterdam_active / total_licenses * 100
  )

# Prepare data for stacked bar chart
hourly_stacked <- weekday_data %>%
  select(timestamp_ct, date_ct, hour_ct, weekday_ct, 
         texas_pct, bangalore_pct, amsterdam_pct) %>%
  pivot_longer(
    cols = c(texas_pct, bangalore_pct, amsterdam_pct),
    names_to = "region",
    values_to = "percentage"
  ) %>%
  mutate(
    region = gsub("_pct", "", region),
    region = factor(region, levels = c("amsterdam", "bangalore", "texas"))
  )

# Create stacked bar chart for peak hour visualization
peak_hour_plot <- ggplot(hourly_stacked, 
       aes(x = hour_ct, y = percentage, fill = region)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ weekday_ct) +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "red") +
  annotate("rect", xmin = 13, xmax = 15, ymin = 0, ymax = Inf, 
           alpha = 0.2, fill = "yellow") +
  labs(title = "License Usage by Hour and Region",
       subtitle = "Percentage of total licenses (300) used by each region",
       x = "Hour of Day (Central Time)",
       y = "Percentage of Licenses Used",
       fill = "Region") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal()
ggsave("peak_hour_usage.png", plot = peak_hour_plot, width = 12, height = 8)

# 4. License Denials Cleanup and Analysis
# Group denials by user_id and request time (rounded to nearest minute)
denial_data <- license_data %>% 
  filter(denial == 1) %>%
  mutate(request_minute = floor_date(data_start_timestamp, "minute"))

# Count retries (same user requesting within the same minute)
denial_counts <- denial_data %>%
  group_by(user_id, request_minute) %>%
  summarize(retry_count = n(), .groups = "drop")

# Analyze retry patterns
retry_summary <- denial_counts %>%
  group_by(retry_count) %>%
  summarize(
    occurrences = n(),
    percentage = n() / nrow(denial_counts) * 100
  )

# Print retry analysis
cat("License Denial Retry Analysis:\n")
print(retry_summary)

# Count unique denials (after filtering out retries)
unique_denials <- sum(denial_counts$retry_count >= 1)
total_denials <- nrow(denial_data)
cat("\nTotal denial requests:", total_denials, "\n")
cat("Unique denial incidents:", unique_denials, "\n")
cat("Retry percentage:", round((total_denials - unique_denials) / total_denials * 100, 2), "%\n\n")

# 5. Regional Analysis of Long Sessions
# Calculate session duration statistics by region
region_duration <- active_sessions %>%
  group_by(region) %>%
  summarize(
    avg_duration = mean(session_duration),
    median_duration = median(session_duration),
    pct_over_5hr = sum(session_duration > 5) / n() * 100
  )

# Print region duration statistics
cat("Session Duration by Region:\n")
print(region_duration)

# Find top users with longest sessions
top_users <- active_sessions %>%
  group_by(user_id, region) %>%
  summarize(
    avg_duration = mean(session_duration),
    total_sessions = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_duration)) %>%
  head(20)

# Print top users with longest sessions
cat("\nTop Users with Longest Average Session Duration:\n")
print(top_users)

# 6. Create heatmap of license usage by hour and day
# Prepare data for heatmap
heatmap_data <- weekday_data %>%
  mutate(date = as.Date(date_ct)) %>%
  select(date, hour_ct, total_active)

# Create heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = hour_ct, y = date, fill = total_active)) +
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
ggsave("license_usage_heatmap.png", plot = heatmap_plot, width = 12, height = 8)

# 7. Regional heatmaps (local time)
# Function to create regional heatmap
create_regional_heatmap <- function(region_name) {
  # Filter active sessions for this region
  region_sessions <- active_sessions %>% filter(region == region_name)
  
  # Create hourly sequence in local time
  region_offset_hours <- ifelse(region_name == "texas", -6,
                         ifelse(region_name == "bangalore", 5, 1))
  region_offset_mins <- ifelse(region_name == "bangalore", 30, 0)
  
  # Count active sessions by local hour
  region_hourly <- hourly_data %>%
    mutate(
      local_timestamp = timestamp + hours(region_offset_hours) + minutes(region_offset_mins),
      local_hour = hour(local_timestamp),
      local_date = as.Date(local_timestamp)
    )
  
  # Count active sessions for this region at each timestamp
  region_hourly$active_count <- sapply(region_hourly$timestamp, 
                                      function(ts) count_active_sessions(ts, region_name))
  
  # Filter weekdays only
  region_weekday <- region_hourly %>%
    filter(wday(local_date) %in% 2:6) # Monday to Friday
  
  # Create heatmap
  region_quota <- region_quotas[region_name]
  ggplot(region_weekday, aes(x = local_hour, y = local_date, fill = active_count)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red", 
                       name = "Active Licenses",
                       limits = c(0, region_quota)) +
    labs(title = paste0(toupper(substr(region_name, 1, 1)), substr(region_name, 2, nchar(region_name)), " License Usage (Local Time)"),
         subtitle = paste0("Region quota: ", region_quota, " licenses"),
         x = "Hour of Day (Local Time)",
         y = "Date") +
    scale_x_continuous(breaks = seq(0, 23, 3)) +
    theme_minimal()
}

# Create and save regional heatmaps
texas_heatmap <- create_regional_heatmap("texas")
ggsave("texas_usage_heatmap.png", plot = texas_heatmap, width = 10, height = 6)

bangalore_heatmap <- create_regional_heatmap("bangalore")
ggsave("bangalore_usage_heatmap.png", plot = bangalore_heatmap, width = 10, height = 6)

amsterdam_heatmap <- create_regional_heatmap("amsterdam")
ggsave("amsterdam_usage_heatmap.png", plot = amsterdam_heatmap, width = 10, height = 6)

# 8. Summary insights for presentation
cat("\n=== KEY INSIGHTS FOR PRESENTATION ===\n")

# Calculate percentage of sessions over 5 hours
pct_long_sessions <- sum(active_sessions$session_duration > 5) / nrow(active_sessions) * 100
cat("Percentage of sessions lasting over 5 hours:", round(pct_long_sessions, 1), "%\n")

# Top 3 teams with longest sessions
top_regions <- region_duration %>% arrange(desc(avg_duration))
cat("\nRegions ranked by average session duration:\n")
for (i in 1:nrow(top_regions)) {
  cat(i, ". ", toupper(substr(top_regions$region[i], 1, 1)), 
      substr(top_regions$region[i], 2, nchar(top_regions$region[i])),
      " (", round(top_regions$avg_duration[i], 2), " hours)\n", sep="")
}

# Denial analysis
retry_pct <- (total_denials - unique_denials) / total_denials * 100
real_denial_pct <- 100 - retry_pct
cat("\nDenial analysis:\n")
cat("- ", round(retry_pct, 1), "% of denials are retries\n", sep="")
cat("- ", round(real_denial_pct, 1), "% are unique denial incidents\n", sep="")

# Peak hour analysis
peak_hour_data <- weekday_data %>%
  filter(hour_ct >= 13 & hour_ct <= 15) %>%
  summarize(avg_usage = mean(total_pct))
cat("\nAverage license usage during peak hours (1-3 PM CT):", round(peak_hour_data$avg_usage, 1), "%\n")

# Create a summary plot for the presentation
summary_data <- data.frame(
  metric = c("Sessions > 5 hours", "Peak hour utilization", "Unique denials", "Retry noise"),
  value = c(pct_long_sessions, peak_hour_data$avg_usage, real_denial_pct, retry_pct)
)

summary_plot <- ggplot(summary_data, aes(x = reorder(metric, -value), y = value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", value)), vjust = -0.5) +
  labs(title = "License Usage Key Metrics",
       x = "",
       y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("summary_insights.png", plot = summary_plot, width = 8, height = 6)

cat("\nAnalysis complete. All visualizations have been saved to the current directory.\n")
