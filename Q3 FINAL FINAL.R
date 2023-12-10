# Load data from GitHub
github_url <- "https://raw.githubusercontent.com/Stat184-Fall2023/FP-LiamHooks/main/Spotify%20Songs%20NEW.csv"
spotify_data <- read.csv(github_url)

# Install and load required packages
install.packages(c("tidyverse", "dplyr", "ggplot2", "lubridate"))
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
install.packages("ggplot2")
library(ggplot2)

# Filter data for Latin and EDM genres, excluding rows with missing values
selected_genres <- c("latin", "edm")
filtered_data <- spotify_data %>%
  filter(complete.cases(Cleaned.Up.Release.Date, playlist_genre) & playlist_genre %in% selected_genres)

# Filter out rows where 'Cleaned.Up.Release.Date' is NA
filtered_data <- filtered_data[!is.na(filtered_data$Cleaned.Up.Release.Date), ]

# Create a decade variable with updated labels
filtered_data$Decade <- cut(filtered_data$Cleaned.Up.Release.Date, 
                            breaks = c(1950, seq(1960, 2010, by = 10), 2020),
                            labels = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s"))

# Filter out rows where 'track_popularity' is NA
filtered_data <- filtered_data[!is.na(filtered_data$track_popularity), ]

# Filter out rows where 'Decade' is NA
filtered_data <- filtered_data[!is.na(filtered_data$Decade), ]

# Create a trend chart
ggplot(filtered_data, aes(x = Decade, y = track_popularity, color = playlist_genre, group = playlist_genre)) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  labs(title = "Popularity Trend of Latin and EDM Genres Over Decades",
       x = "Decade",
       y = "Average Popularity") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
