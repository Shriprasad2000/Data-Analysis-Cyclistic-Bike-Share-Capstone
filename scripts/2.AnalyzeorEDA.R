# Questions: How do annual members and casual riders use Cyclistic bikes differently?
#   
#   How often do casual members use rides compared to annual members on the normal weekdays and weekends?
#   How does the average ride duration differ between casual riders and annual members, when analyzed on a weekly and monthly basis?
#   Which days of the week show the highest usage for each rider type?
#   Why do the casual members use their bicycle rides for leisure activity, commute to work or stay fit? (Not the accurate Question- coz data might not be there to justify this question)


# 1. checking number of rides and average time of commute ----
df_all %>%
  # 1. Create weekday vs weekend
  mutate(
    day_type = ifelse(
      day_of_week %in% c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"
    )
  ) %>%
  # 2. Group by rider type, day type, and year
  group_by(user_type, day_type, year) %>%
  # 3. Calculate summary statistics
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # 4. Reshape for year-over-year comparison
  pivot_wider(
    names_from = year,
    values_from = c(total_rides, avg_ride_length)
  )


# weekday vs weekend usage
df_all %>% 
  group_by(user_type, day_type) %>% 
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups = "drop"
  )


# Rate of change from 2019 to 2020
df_all %>% 
  group_by(user_type, year) %>% 
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE)
  ) %>% 
  arrange(user_type, year)

# Monthly usage trends (optional)
df_all %>% 
  group_by(user_type, month) %>% 
  summarise(total_rides = n())


# How does their ride duration differ?
weekday_weekend_summary <- df_all %>%
  group_by(user_type, day_type, year) %>%
  summarise(
    total_rides = n(),
    avg_ride_length = mean(ride_length_min, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot to compare 2019 vs 2020 side by side
weekday_weekend_summary_wide <- weekday_weekend_summary %>% 
  pivot_wider(
    names_from = year,
    values_from = c(total_rides, avg_ride_length)
  )


