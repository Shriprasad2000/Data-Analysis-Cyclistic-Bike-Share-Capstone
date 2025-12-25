# Objective => How do annual members and casual riders use Cyclistic bikes differently?

# How often do casual members use rides compared to annual members on the normal weekdays and weekends?
# How does the average ride duration differ between casual riders and annual members, when analyzed on a weekly and monthly basis?
# Which days of the week show the highest usage for each rider type?
  
df1 <- read.csv("Divvy_Trips_2019_Q1.csv")
df2 <- read.csv("Divvy_Trips_2020_Q1.csv")

head(df1)

colnames(df1)
colnames(df2)

# Keeping column names same for 'Date'
df1 <- df1 %>% 
  rename(
    started_at = start_time,
    ended_at = end_time
  )

#changing column name "ride_id" => to "trip_id" to maintain the same column name in both dfs
df2 <- df2 %>% 
  rename(
    trip_id = ride_id
  )

# keeping the name station names and station id common in both the dfs
df1 <- df1 %>% 
  rename(
    start_station_name = from_station_name,
    end_station_name  = to_station_name,
    start_station_id = from_station_id,
    end_station_id = to_station_id
  )

# changes related to the user type column in both DFs
df1 <- df1 %>% 
  rename(
    user_type = usertype
  )

df2 <- df2 %>% 
  rename(
    user_type = member_casual
  )


# updating user_type values to casual and member in both DFs
df1 <- df1 %>% 
  mutate(user_type = case_when(
    user_type %in% c("Subscriber", "subscriber") ~ "member",
    user_type %in% c("Customer", "casual") ~ "casual",
    TRUE ~ user_type
  ))

# creating new column to find out the time traveled by the customers in minutes for both the dfs
df2 <- df2 %>%
  mutate(ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins"))) %>%
  relocate(ride_length_min, .after = ended_at)

df1 <- df1 %>%
  mutate(ride_length_min = as.numeric(difftime(ended_at, started_at, units = "mins"))) %>%
  relocate(ride_length_min, .after = ended_at)


# df1 %>%
#   +     mutate(
#     +         tripduration_clean = as.numeric(gsub(",", "", tripduration)),
#     +         tripduration_min = round( tripduration_clean / 60, digits = 2)
#     +     ) %>%
#   +     select(
#     +         started_at,
#     +         ended_at,
#     +         ride_length_min,
#     +         tripduration,
#     +         tripduration_min
#     +     ) %>%
#   +     head(50)

# optional: added year
df1 <- df1 %>% mutate(year = 2019)
df2 <- df2 %>% mutate(year = 2020)

# trip_id of df1 is a numeric type converting into string type
df1 <- df1 %>% 
  mutate(trip_id = as.character(trip_id))

# merging 2 dfs 
# 1. get the common columns 
common_cols <- intersect(colnames(df1), colnames(df2))

# 2: Select ONLY those matching columns
df1_common <- df1 %>% select(all_of(common_cols))
df2_common <- df2 %>% select(all_of(common_cols))

# 3. Combine them
df_all <- bind_rows(df1_common, df2_common)

# one liner version
# df_all_common <- bind_rows(
#   df1 %>% select(intersect(names(df1), names(df2))),
#   df2 %>% select(intersect(names(df1), names(df2)))
# )


# created day_of_week to get the week day (Eg: Sunday, Monday, ...)
df_all <- df_all %>% 
  mutate(day_of_week = weekdays(as.Date(df_all$started_at)))

# understand the data structure
# glimpse(df_all)
# summary(df_all)
# colSums(is.na(df_all))

# Check cleaning worked
# table(df_all$user_type)
# summary(df_all$ride_length_min)
# str(df_all$started_at)


# removing the rides which has negative ride time
df_all <- df_all %>%
  filter(ride_length_min > 0)


# creating month column 
df_all <- df_all %>% 
  mutate(
    month = format(as.Date(started_at), "%B"),
  )

# creating type of month (weekday or weekend)
df_all <- df_all %>% 
  mutate(
    day_type = ifelse(
      day_of_week %in% c("Saturday", "Sunday"),
      "Weekend",
      "Weekday"
    )
  )

