# Create an animated GIF from the simulation frames
library(gifski)

# Get all frame files in order
frame_files <- list.files("simulation_frames", pattern = "*.png", full.names = TRUE)
frame_files <- sort(frame_files)  # Ensure they're in order

# Create the animated GIF
gifski(frame_files, 
       "license_usage_animation.gif", 
       width = 800, 
       height = 600, 
       delay = 0.8)  # Delay between frames in seconds

cat("Animation created! Open license_usage_animation.gif to view it.\n")
