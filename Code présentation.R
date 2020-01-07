## R Packages


pacman::p_load(tidyverse,
              data.table,
              lubridate,
              cowplot,
              geosphere,
              ggExtra,
              ggcorrplot)


## Load data

train <- as_tibble(fread('train.csv'))
weather <- as_tibble(fread("weather_data_nyc_centralpark_2016(1).csv"))
fast1 <- as_tibble(fread("fastest_routes_train_part_1.csv"))
fast2 <- as_tibble(fread("fastest_routes_train_part_2.csv"))
fastest_route <- bind_rows(fast1, fast2)


## explore
 
summary(train)
glimpse(train)



## feature engineering and data join

train <- train %>%
           mutate(pickup_datetime = ymd_hms(pickup_datetime),
                  dropoff_datetime = ymd_hms(dropoff_datetime),
                  vendor_id = factor(vendor_id),
                  passenger_count = factor(passenger_count))

train <- train %>%
   mutate(speed = dist/trip_duration*3.6,
          date = date(pickup_datetime),
          month = month(pickup_datetime, label = TRUE),
          wday = wday(pickup_datetime, label = TRUE),
          wday = fct_relevel(wday, c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")),
          hour = hour(pickup_datetime),
          work = (hour %in% seq(8,18)) & (wday %in% c("Mon","Tues","Wed","Thurs","Fri")),
          jfk_trip = (jfk_dist_pick < 2e3) | (jfk_dist_drop < 2e3),
          lg_trip = (lg_dist_pick < 2e3) | (lg_dist_drop < 2e3),
          blizzard = !( (date < ymd("2016-01-22") | (date > ymd("2016-01-29"))) )
   )

 
weather <- weather %>%
   mutate(date = dmy(date),
          rain = as.numeric(ifelse(precipitation == "T", "0.01", precipitation)),
          s_fall = as.numeric(ifelse(`snow fall` == "T", "0.01", `snow fall`)),
          s_depth = as.numeric(ifelse(`snow depth` == "T", "0.01", `snow depth`)),
          all_precip = s_fall + rain,
          has_snow = (s_fall > 0) | (s_depth > 0),
          has_rain = rain > 0,
          max_temp = `maximum temperature`,
          min_temp = `minimum temperature`)



train <- weather %>%
   select(date, rain, s_fall, all_precip, has_snow, has_rain, s_depth, max_temp, min_temp) %>%
   train <- right_join(train, by = "date") 

train <- fastest_route %>%
   select(id, total_distance, total_travel_time, number_of_steps,
          step_direction, step_maneuvers) %>%
   mutate(fastest_speed = total_distance/total_travel_time*3.6,
          left_turns = str_count(step_direction, "left"),
          right_turns = str_count(step_direction, "right"),
          turns = str_count(step_maneuvers, "turn")
   ) %>%
   select(-step_direction, -step_maneuvers) %>%
   right_join(train, by = "id") %>%
   mutate(fast_speed_trip = total_distance/trip_duration*3.6)

# some interesting points (JFK aeroport and La Guardia aeroport)

jfk_coord <- tibble(lon = -73.778889, lat = 40.639722)
la_guardia_coord <- tibble(lon = -73.872611, lat = 40.77725)

# distances to La Guardia and to JFK (distCosine function is part of the geosphere package)

train <- train %>%
          mutate(jfk_dist_pick = distCosine(pick_coord, jfk_coord),
                 jfk_dist_drop = distCosine(drop_coord, jfk_coord),
                 lg_dist_pick = distCosine(pick_coord, la_guardia_coord),
                 lg_dist_drop = distCosine(drop_coord, la_guardia_coord))

# create the distance and bearing between pickup and drop off (bearing function is part of the geosphere package)

pick_coord <- train %>%
   select(pickup_longitude, pickup_latitude)
 
drop_coord <- train %>%
   select(dropoff_longitude, dropoff_latitude)

train <- train %>%
           mutate(dist = distCosine(pick_coord, drop_coord),
                  bearing = bearing(pick_coord, drop_coord))
 
# other usefull transformation of data

day_plus_trips <- train %>%
  filter(trip_duration > 24*3600)


ny_map <- as_tibble(map_data("state", region = "new york:manhattan"))

tpick <- day_plus_trips %>%
  select(lon = pickup_longitude, lat = pickup_latitude)

tdrop <- day_plus_trips %>%
  select(lon = dropoff_longitude, lat = dropoff_latitude)


day_trips <- train %>%
  filter(trip_duration < 24*3600 & trip_duration > 22*3600)


day_trips %>% 
  arrange(desc(dist)) %>%
  select(dist, pickup_datetime, dropoff_datetime, speed)


# Diagramme en bâton (slide 25)

train %>%
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n, fill = passenger_count)) +
  geom_col() +
  scale_y_sqrt() + 
  labs(x = "passengers count", y ="count",
       title = "Column",
       subtitle = "Distribution of number of passengers")+
  theme_bw() +
  theme(legend.position = "none")

# Histogramme (slide 26)

train %>%
  ggplot(aes(trip_duration)) +
  geom_histogram(fill = "red", bins = 150) +
  scale_x_log10() +
  scale_y_sqrt() + 
  theme_bw() + 
  labs(title = "Histogramme",
       subtitle = "trip duration",
       caption = "source : kaggle",
       x = "Trip duration")


# Histogramme (II) (Slide 27)

p1 <- train %>%
  ggplot(aes(pickup_datetime)) +
  geom_histogram(fill = "red", bins = 120) +
  labs(x = "Pickup dates")

p2 <- train %>%
  ggplot(aes(dropoff_datetime)) +
  geom_histogram(fill = "blue", bins = 120) +
  labs(x = "Dropoff dates")

title <- ggdraw() + 
  draw_label(
    "Histogram of pickup and dropoff dates",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(title, p1, p2, nrow = 3, rel_heights = c(0.1,0.45,0.45))


# Nuage de Points (slide 28)

train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  count() %>%
  ggplot(aes(hpick, n, color = vendor_id)) +
  geom_point(size = 4) +
  labs(x = "Hour of the day", y = "Total number of pickups",
       title = "Nuage de Points",
       subtitle ="Total Number of Pickups per Hour") +
  theme(legend.position = "none") +
  theme_minimal_grid()


# Diagramme linéaire (slide 29)

p1 <- train %>%
  mutate(hpick = hour(pickup_datetime),
         Month = factor(month(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, Month) %>%
  count() %>%
  ggplot(aes(hpick, n, color = Month)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")+
  theme_gray()

p2 <- train %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, color = wday)) +
  geom_line(size = 1.5) +
  labs(x = "Hour of the day", y = "count")+
  theme_gray()

title <- ggdraw() + 
  draw_label(
    "Total Number of trips per Month and Weekday",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

plot_grid(title, p1, p2, 
          nrow = 3, 
          rel_heights = 
            c(0.1,0.45,0.45)) 


# Statistiques et Facettes (slide 30) 

train %>%
  mutate(hpick = hour(pickup_datetime)) %>%
  group_by(hpick, vendor_id) %>%
  summarise(median_duration = median(trip_duration)/60) %>%
  ggplot(aes(hpick, median_duration, color = vendor_id)) +
  geom_smooth(method = "loess", span = 1/2) +
  geom_point(size = 4) +
  facet_wrap(~ vendor_id) +
  labs(x = "Hour of the day", y = "Median trip duration [min]",
       title ="Scatterplot with smoother and Facets",
       subtitle = "Median trip duration per Hour and by vendor") +
  theme_half_open() +
  theme(legend.position = "none") 


# Boite à Moustaches (slide 31)

train %>%
  ggplot(aes(passenger_count, trip_duration, color = passenger_count)) +
  geom_boxplot() +
  scale_y_log10() +
  facet_wrap(~ vendor_id) +
  labs(y = "Trip duration [s]", x = "Number of passengers",
       title = "Boxplot with Facets",
       subtitle = "Trip Duration per Number of passengers and vendor") +
  theme_light() + 
  theme(legend.position = "none") 

# Densités (slide 32) 

train %>%
  ggplot(aes(trip_duration, fill = vendor_id)) +
  geom_density(position = "stack") +
  scale_x_log10() + 
  labs(x = "Trip duration",
       title = "density",
       subtitle = "Trip duration per vendor") +
  theme_dark()


# Nuage de Points (slide 33)

set.seed(4321)

train %>%
  sample_n(5e3) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]",
       title = "Scatterplot",
       subtitle = "Trip Duration vs Direct Distance for 5000 
       sampled obsevrations") +
  theme_bw()
  
# Nuage de points (Jitter) à distribution Marginale (slide 34)

p1 <- train %>%
  sample_n(5e3) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_jitter(alpha = 0.5) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]",
       title = "Scatterplot",
       subtitle = "Trip Duration vs Direct Distance for 5000 
       sampled obsevrations") +
  geom_smooth(col = "blue", method = "lm", se = FALSE)

ggMarginal(p1, type = "histogram", fill ="blue")


# slide 35 

ggMarginal(p1, type = "density", fill ="blue")

ggMarginal(p1, type = "boxplot", fill ="blue")

ggMarginal(p1, type = "violin", fill ="blue")

ggMarginal(p1, type = "densigram", fill ="blue")


# Alternative au nuage de points : bins (slide 36)

train %>%
  filter(trip_duration < 3600 & trip_duration > 120) %>%
  filter(dist > 100 & dist < 100e3) %>%
  ggplot(aes(dist, trip_duration)) +
  geom_bin2d(bins = c(500,500)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Direct distance [m]", y = "Trip duration [s]",
       title = "2D Bins",
       subtitle = "Trip Duration vs Direct Distance for filtred data") +
  theme_cowplot()



# Cartes thématiques (Heat Maps) (slide 37)

train %>%
  group_by(wday, hour) %>%
  summarise(median_speed = median(speed)) %>%
  ggplot(aes(hour, wday, fill = median_speed)) +
  geom_tile() +
  labs(x = "Hour of the day", y = "Day of the week",
       title = "Heatmap",
       subtitle = "Median Speed per day and hour") +
  scale_fill_distiller(palette = "Spectral")        # scale_fill_continuous()


# Utilisation des coordonnées : polaires (slide 38)

train %>%
  filter(dist < 1e5) %>%
  ggplot(aes(bearing, dist)) +
  geom_bin2d(bins = c(100,100)) +
  labs(x = "Bearing", y = "Direct distance", 
       title = "2D bins with polar coordinates",
       subtitle = "Bearing Vs Direct Distance") +
  scale_y_log10() +
  theme(legend.position = "none") +
  coord_polar() +
  scale_x_continuous(breaks = seq(-180, 180, by = 45))

train %>%
  filter(speed < 75 & dist < 1e5) %>%
  ggplot(aes(bearing, speed)) +
  geom_bin2d(bins = c(100,100)) +
  labs(x = "Bearing", y = "Speed", 
       title = "2D bins with polar coordinates",
       subtitle = "Bearing Vs Speed") +
  coord_polar() +
  scale_x_continuous(breaks = seq(-180, 180, by = 45))


# Utilisation des boucles pour dessiner un graphique des différents itinéraires (slide 39)

p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1) +
  theme_classic()



for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, 
                                    addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',
                        alpha=.75)
  
}

p1 + ggtitle("Longer than 
              a day trips in 
             relation to 
             Manhattan")



# Utilisation des boucles pour dessiner un graphique des différents itinéraires (II) (slide 40)

set.seed(2017)
day_trips <- day_trips %>%
  sample_n(200)

tpick <- day_trips %>%
  select(lon = pickup_longitude, lat = pickup_latitude)

tdrop <- day_trips %>%
  select(lon = dropoff_longitude, lat = dropoff_latitude)

p1 <- ggplot() +
  geom_polygon(data=ny_map, aes(x=long, y=lat), fill = "grey60") +
  geom_point(data=tpick,aes(x=lon,y=lat),size=1,color='red',alpha=1) +
  geom_point(data=tdrop,aes(x=lon,y=lat),size=1,color='blue',alpha=1) +
  theme_classic()



for (i in seq(1,nrow(tpick))){
  inter <- as.tibble(gcIntermediate(tpick[i,],  tdrop[i,], n=30, addStartEnd=TRUE))
  p1 <- p1 +  geom_line(data=inter,aes(x=lon,y=lat),color='blue',alpha=.25)
  
}


p1 + ggtitle("22h to 24h long trips in relation to Manhattan")


# Matrice de Corrélation (slide 41)

train %>%
  select(-id, -pickup_datetime, -dropoff_datetime, -jfk_dist_pick,
         -jfk_dist_drop, -lg_dist_pick, -lg_dist_drop, -date,
         -store_and_fwd_flag, -hour, -rain, -s_fall, -all_precip,
         -has_rain, -s_depth, -min_temp, -max_temp,
         -wday, -right_turns, -turns, -fast_speed_trip, -work) %>%
  mutate(passenger_count = as.integer(passenger_count),
         vendor_id = as.integer(vendor_id),
         jfk_trip = as.integer(jfk_trip),
         lg_trip = as.integer(lg_trip),
         month = as.integer(month),
         blizzard = as.integer(blizzard),
         has_snow = as.integer(has_snow)) %>%
  select(trip_duration, speed, everything()) %>%
  cor(use="complete.obs", method = "spearman") %>%
  round(1) %>%
  ggcorrplot(hc.order = TRUE, 
             type = "lower", 
             lab = TRUE, 
             lab_size = 3, 
             method="circle", 
             colors = c("tomato2", "white", "springgreen3"), 
             title="Correlation Matrix", 
             ggtheme=theme_bw)


# Surface (slide 42)

train %>%
  mutate(hpick = hour(pickup_datetime),
         wday = factor(wday(pickup_datetime, label = TRUE))) %>%
  group_by(hpick, wday) %>%
  count() %>%
  ggplot(aes(hpick, n, fill = wday)) +
  geom_area(position = "fill") +
  labs(x = "Hour of the day", y = "trips",
       title = "Area",
       subtitle = "Trip duration per Hour")+
  theme_classic()


# Utilisation de la couche "statistiques" (slide 43)


train %>% 
  group_by(passenger_count) %>%
  count() %>%
  ggplot(aes(passenger_count, n)) +
  stat_summary(fun.y = mean, geom = "bar", fill ="skyblue") +
  labs(x = "passengers count", y = "count",
       title ="Summary statistics",
       subtitle = "passengers count")

 
