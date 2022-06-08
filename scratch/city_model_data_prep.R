# arrange the rows like: YYY YYN YNY YNN NYY NYN NNY NNN
# order is categories: mode substitution, trip frequency, trip distance

# labels:
tmp2$ms = apply(tmp2, 1, function(x) all( !is.na(x[27:32])))
tmp2$fr = !is.na(tmp2$mean_trip_freq_7days)
tmp2$di =  !is.na(tmp2$distance_mean_miles)


# put the rws in proper order:
tmp2 = tmp2 %>% filter( !is.na(vehicle) ) %>% arrange( desc(ms), desc(fr), desc(di) )


# scale the mode substitution choices:
city_mode_sub = tmp2[ 1:(n_YYY + n_YYN + n_YNY + n_YNN), c("sub_bike2", "sub_walk2", "sub_ridehail2", "sub_private_car2", "sub_no_trip", "sub_transit") ]
city_mode_sub = sweep(city_mode_sub, 1, rowSums(city_mode_sub), `/`)

d_mode7 = list( 
  # count the rows in each category:
  n_YYY = tmp2 %>% filter( ms & fr & di ) %>% nrow(), #sum( sapply(tmp2, function(x) x[['ms']] & x[['fr']] & x[['di']] ) )
  n_YYN = tmp2 %>% filter( ms & fr & !di ) %>% nrow(),
  n_YNY = tmp2 %>% filter( ms & !fr & di ) %>% nrow(),
  n_YNN = tmp2 %>% filter( ms & !fr & !di ) %>% nrow(),
  n_NYY = tmp2 %>% filter( !ms & fr & di ) %>% nrow(),
  n_NYN = tmp2 %>% filter( !ms & fr & !di ) %>% nrow(),
  n_NNY = tmp2 %>% filter( !ms & !fr & di ) %>% nrow(),
  n_NNN = tmp2 %>% filter( !ms & !fr & !di ) %>% nrow(),  # full predictions (no observed data)
  
  # get the variables in their correct order:
  city_trip_distance = tmp2$distance_mean_miles[ !is.na(tmp2$distance_mean_miles) ],
  city_trip_frequency = tmp2$mean_trip_freq_7days[ !is.na(tmp2$mean_trip_freq_7days) ],
  city_mode_sub = city_mode_sub, #tmp2[ 1:(n_YYY + n_YYN + n_YNY + n_YNN), c("sub_bike2", "sub_walk2", "sub_ridehail2", "sub_private_car2", "sub_no_trip", "sub_transit") ],
  K = ncol(city_mode_sub),
    
  scooter_indicator = ifelse(tmp2$vehicle == "scooter", 1, 0)
)
