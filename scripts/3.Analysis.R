#  ------------ Plotting ----------------------------

# 1. Total rides by user type (bar plot)
df_all %>% 
  group_by(user_type) %>% 
  summarise(
    total_rides = n()
  ) %>% 
  ggplot(aes(x = user_type, y = total_rides,fill = user_type)) + 
  geom_col() + 
  labs(
    title = "Total Rides by User Type",
    x = "User Type",
    y = "Total Number of Rides"
  ) + 
  theme_minimal()


# 2. Average ride length by user type (bar plot)
df_all %>%
  group_by(user_type) %>%
  summarise(avg_ride = mean(ride_length_min, na.rm = TRUE)) %>%
  ggplot(aes(x = user_type, y = avg_ride, fill = user_type)) +
  geom_col() +
  labs(
    title = "Average Ride Length by User Type",
    x = "User Type",
    y = "Average Ride Length (minutes)"
  ) +
  theme_minimal()

# 3. Total rides by day type (weekend vs weekday)
df_all %>% 
  group_by(user_type, day_type) %>% 
  summarise(total_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = day_type, y = total_rides, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Weekday vs Weekend Usage",
    x = "Day Type",
    y = "Total Rides"
  ) +
  theme_minimal()

# 4. Daily usage pattern (days of week)
df_all %>%
  group_by(user_type, day_of_week) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = total_rides, color = user_type, group = user_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Total Rides by Day of Week",
    x = "Day of Week",
    y = "Total Rides"
  ) +
  theme_minimal()

# 5. Trend: Average ride length by month
df_all %>%
  group_by(user_type, month) %>%
  summarise(avg_ride = mean(ride_length_min, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = month, y = avg_ride, color = user_type, group = user_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Average Ride Duration by Month",
    x = "Month",
    y = "Avg Ride Length (min)"
  ) +
  theme_minimal()


# 6. Ride count comparison between 2019 and 2020
df_all %>%
  group_by(user_type, year) %>%
  summarise(total_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_rides, fill = user_type)) +
  geom_col(position = "dodge") +
  labs(
    title = "Ride Count Comparison (2019 vs 2020)",
    x = "Year",
    y = "Total Rides"
  ) +
  theme_minimal()
