# Load data from GitHub
github_url <- "https://raw.githubusercontent.com/Stat184-Fall2023/FP-LiamHooks/main/Spotify%20Songs%20NEW.csv"
spotify_data <- read.csv(github_url)

# Create a decade variable with custom labels
spotify_data$Decade <- cut(spotify_data$Cleaned.Up.Release.Date, breaks = seq(1950, 2020, by = 10),
                           labels = c("1950s", "1960s", "1970s", "1980s", "1990s", "2000s", "2010s"))

# Filter data for the 1960s decade
songs_1960s <- spotify_data %>%
  filter(Decade == "1960s")

# Filter data for the 2010s decade
songs_2010s <- spotify_data %>%
  filter(Decade == "2010s")

# Danceability 1960
ggplot(songs_1960s, aes(x = danceability, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot of Danceability vs Popularity (1960s)",
       x = "Danceability",
       y = "Popularity") +
  theme_minimal()


# Danceability 2010
ggplot(songs_2010s, aes(x = danceability, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatterplot of Danceability vs Popularity (2010s)",
       x = "Danceability",
       y = "Popularity") +
  theme_minimal()


# Loudness 1960
ggplot(songs_1960s, aes(x = loudness, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot of Loudness vs Popularity (1960s)",
       x = "Loudness",
       y = "Popularity") +
  theme_minimal()


# Loudness 2010
ggplot(songs_2010s, aes(x = loudness, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatterplot of Loudness vs Popularity (2010s)",
       x = "Loudness",
       y = "Popularity") +
  theme_minimal()


# Duration 1960
ggplot(songs_1960s, aes(x = duration_ms, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Scatterplot of Duration vs Popularity (1960s)",
       x = "Duration (ms)",
       y = "Popularity") +
  theme_minimal()


# Duration 2010
ggplot(songs_2010s, aes(x = duration_ms, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatterplot of Duration vs Popularity (2010s)",
       x = "Duration (ms)",
       y = "Popularity") +
  theme_minimal()


# Tempo 1960
ggplot(songs_1960s, aes(x = tempo, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  labs(title = "Scatterplot of Tempo vs Popularity (1960s)",
       x = "Tempo",
       y = "Popularity") +
  theme_minimal()


# Tempo 2010
ggplot(songs_2010s, aes(x = tempo, y = track_popularity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(title = "Scatterplot of Tempo vs Popularity (2010s)",
       x = "Tempo",
       y = "Popularity") +
  theme_minimal()





# Statistical Chart for Decades
filtered_data_decades <- spotify_data %>%
  filter(Cleaned.Up.Release.Date > 1960 & Cleaned.Up.Release.Date < 2020)

summary_stats_decades <- filtered_data_decades %>%
  mutate(Decade = cut(Cleaned.Up.Release.Date, breaks = seq(1960, 2020, by = 10),
                      labels = c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s"))) %>%
  group_by(Decade) %>%
  summarise(
    n = n(),
    min = min(track_popularity),
    Q1 = quantile(track_popularity, probs = 0.25),
    Median = median(track_popularity),
    Q3 = quantile(track_popularity, probs = 0.75),
    Max = max(track_popularity)
  ) %>%
  ungroup()
  

print(summary_stats_decades)




#Statistical Chart for Genres
filtered_data_genres <- spotify_data %>%
  filter(Cleaned.Up.Release.Date >= 1960 & Cleaned.Up.Release.Date <= 2019)


summary_stats_genres <- filtered_data_genres %>%
  group_by(playlist_genre) %>%
  summarise(
    n = n(),
    min = min(track_popularity),
    Q1 = quantile(track_popularity, probs = 0.25),
    Median = median(track_popularity),
    Q3 = quantile(track_popularity, probs = 0.75),
    Max = max(track_popularity)
  ) %>%
  ungroup()


print(summary_stats_genres)


