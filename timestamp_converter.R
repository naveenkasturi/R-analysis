# Timestamp Converter
# This script demonstrates how to properly convert timestamps to local time for each region

# Load required libraries
library(dplyr)
library(lubridate)

# Read the license usage data
license_data <- read.csv("license_usage_mock_data.csv", stringsAsFactors = FALSE)

# Convert timestamps to datetime objects
license_data$data_start_timestamp <- as.POSIXct(license_data$data_start_timestamp, format="%Y-%m-%d %H:%M:%S")
license_data$data_end_timestamp <- as.POSIXct(license_data$data_end_timestamp, format="%Y-%m-%d %H:%M:%S")

# Print a sample of the original data
cat("Original timestamps (UTC):\n")
sample_data <- license_data %>% 
  select(region, user_id, data_start_timestamp, data_end_timestamp) %>%
  head(5)
print(sample_data)
cat("\n")

# Method 1: Using lubridate's with_tz function (for standard time zones)
# Define a function to convert UTC to a specific time zone
convert_to_timezone <- function(datetime, tz_name) {
  # First, ensure the datetime is in UTC
  dt_utc <- force_tz(datetime, tzone = "UTC")
  # Then convert to the target timezone
  with_tz(dt_utc, tzone = tz_name)
}

# Add columns with timestamps in each region's time zone
license_data <- license_data %>%
  mutate(
    # Central Time (for Texas)
    start_time_ct = convert_to_timezone(data_start_timestamp, "America/Chicago"),
    end_time_ct = convert_to_timezone(data_end_timestamp, "America/Chicago"),
    
    # Indian Standard Time (for Bangalore)
    start_time_ist = convert_to_timezone(data_start_timestamp, "Asia/Kolkata"),
    end_time_ist = convert_to_timezone(data_end_timestamp, "Asia/Kolkata"),
    
    # Central European Time (for Amsterdam)
    start_time_cet = convert_to_timezone(data_start_timestamp, "Europe/Amsterdam"),
    end_time_cet = convert_to_timezone(data_end_timestamp, "Europe/Amsterdam")
  )

# Print sample data with converted timestamps
cat("Method 1: Using standard time zones:\n")
sample_converted <- license_data %>%
  select(region, data_start_timestamp, start_time_ct, start_time_ist, start_time_cet) %>%
  head(5)
print(sample_converted)
cat("\n")

# Method 2: Manual offset calculation (useful for non-standard offsets)
license_data <- license_data %>%
  mutate(
    # Define offsets for each region (in hours and minutes)
    # Note: These offsets are from UTC, assuming the data_start_timestamp is in UTC
    offset_hours = case_when(
      region == "texas" ~ -5,        # CT is UTC-5 during DST, UTC-6 during standard time
      region == "bangalore" ~ 5,     # IST is UTC+5:30
      region == "amsterdam" ~ 2,     # CET is UTC+1, CEST (summer) is UTC+2
      TRUE ~ 0
    ),
    offset_mins = case_when(
      region == "bangalore" ~ 30,  # Bangalore has a 30-minute offset
      TRUE ~ 0
    ),
    
    # Apply offsets to create local timestamps
    # First ensure the timestamp is in UTC
    start_time_utc = force_tz(data_start_timestamp, "UTC"),
    end_time_utc = force_tz(data_end_timestamp, "UTC"),
    
    # Then apply the offset
    start_time_local_manual = start_time_utc + hours(offset_hours) + minutes(offset_mins),
    end_time_local_manual = end_time_utc + hours(offset_hours) + minutes(offset_mins)
  )

# Print sample data with manually converted timestamps
cat("Method 2: Using manual offset calculation:\n")
sample_manual <- license_data %>%
  select(region, data_start_timestamp, start_time_local_manual) %>%
  head(5)
print(sample_manual)
cat("\n")

# Verification: Compare the two methods
# For each region, compare the standard timezone conversion with manual calculation
cat("Verification - Comparing both methods:\n")

# For Texas (Central Time)
texas_verification <- license_data %>%
  filter(region == "texas") %>%
  mutate(
    method1 = format(start_time_ct, "%Y-%m-%d %H:%M:%S"),
    method2 = format(start_time_local_manual, "%Y-%m-%d %H:%M:%S"),
    match = method1 == method2
  ) %>%
  select(user_id, method1, method2, match) %>%
  head(3)
cat("Texas (CT) verification:\n")
print(texas_verification)
cat("\n")

# For Bangalore (Indian Standard Time)
bangalore_verification <- license_data %>%
  filter(region == "bangalore") %>%
  mutate(
    method1 = format(start_time_ist, "%Y-%m-%d %H:%M:%S"),
    method2 = format(start_time_local_manual, "%Y-%m-%d %H:%M:%S"),
    match = method1 == method2
  ) %>%
  select(user_id, method1, method2, match) %>%
  head(3)
cat("Bangalore (IST) verification:\n")
print(bangalore_verification)
cat("\n")

# For Amsterdam (Central European Time)
amsterdam_verification <- license_data %>%
  filter(region == "amsterdam") %>%
  mutate(
    method1 = format(start_time_cet, "%Y-%m-%d %H:%M:%S"),
    method2 = format(start_time_local_manual, "%Y-%m-%d %H:%M:%S"),
    match = method1 == method2
  ) %>%
  select(user_id, method1, method2, match) %>%
  head(3)
cat("Amsterdam (CET) verification:\n")
print(amsterdam_verification)
cat("\n")

# Check for any mismatches across all data
mismatches <- license_data %>%
  mutate(
    texas_match = (region == "texas" & 
                  format(start_time_ct, "%Y-%m-%d %H:%M:%S") == 
                  format(start_time_local_manual, "%Y-%m-%d %H:%M:%S")),
    bangalore_match = (region == "bangalore" & 
                      format(start_time_ist, "%Y-%m-%d %H:%M:%S") == 
                      format(start_time_local_manual, "%Y-%m-%d %H:%M:%S")),
    amsterdam_match = (region == "amsterdam" & 
                      format(start_time_cet, "%Y-%m-%d %H:%M:%S") == 
                      format(start_time_local_manual, "%Y-%m-%d %H:%M:%S"))
  )

# Count matches and mismatches by region
texas_matches <- sum(mismatches$region == "texas" & mismatches$texas_match)
texas_total <- sum(mismatches$region == "texas")
bangalore_matches <- sum(mismatches$region == "bangalore" & mismatches$bangalore_match)
bangalore_total <- sum(mismatches$region == "bangalore")
amsterdam_matches <- sum(mismatches$region == "amsterdam" & mismatches$amsterdam_match)
amsterdam_total <- sum(mismatches$region == "amsterdam")

cat("Match summary:\n")
cat("Texas: ", texas_matches, "/", texas_total, " matches (", 
    round(texas_matches/texas_total*100, 2), "%)\n", sep="")
cat("Bangalore: ", bangalore_matches, "/", bangalore_total, " matches (", 
    round(bangalore_matches/bangalore_total*100, 2), "%)\n", sep="")
cat("Amsterdam: ", amsterdam_matches, "/", amsterdam_total, " matches (", 
    round(amsterdam_matches/amsterdam_total*100, 2), "%)\n", sep="")

# Function to demonstrate practical usage - convert all timestamps to local time
convert_all_to_local <- function(data) {
  result <- data %>%
    mutate(
      # For each region, use the appropriate timezone
      local_start_time = case_when(
        region == "texas" ~ format(start_time_ct, "%Y-%m-%d %H:%M:%S"),
        region == "bangalore" ~ format(start_time_ist, "%Y-%m-%d %H:%M:%S"),
        region == "amsterdam" ~ format(start_time_cet, "%Y-%m-%d %H:%M:%S"),
        TRUE ~ format(data_start_timestamp, "%Y-%m-%d %H:%M:%S")
      ),
      local_end_time = case_when(
        region == "texas" ~ format(end_time_ct, "%Y-%m-%d %H:%M:%S"),
        region == "bangalore" ~ format(end_time_ist, "%Y-%m-%d %H:%M:%S"),
        region == "amsterdam" ~ format(end_time_cet, "%Y-%m-%d %H:%M:%S"),
        TRUE ~ format(data_end_timestamp, "%Y-%m-%d %H:%M:%S")
      )
    )
  
  return(result)
}

# Apply the function to our data
local_time_data <- convert_all_to_local(license_data)

# Show sample of the result
cat("\nFinal result - All timestamps converted to local time:\n")
sample_result <- local_time_data %>%
  select(region, user_id, data_start_timestamp, local_start_time) %>%
  head(10)
print(sample_result)

# Summary
cat("\nSummary:\n")
cat("1. Method 1 uses lubridate's with_tz function with standard timezone names\n")
cat("2. Method 2 uses manual offset calculation which is useful for non-standard offsets\n")
cat("3. Both methods produce equivalent results when the correct timezone and offset are used\n")
cat("4. For production use, Method 1 is preferred as it handles daylight saving time automatically\n")
