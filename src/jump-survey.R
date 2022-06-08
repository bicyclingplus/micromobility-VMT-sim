require('readr')

jump <- read_csv("data/jump_sacramento_data/trips_cleaned.csv")
jump_freq <- read_csv("data/jump_sacramento_data/freq.csv")

# remove rides where the google distance is unreasonable, or where either google or the user reported distance less than 1/4 mile
jump_cleaned <- jump[jump$gg_distance < 100 & jump$gg_distance > 0.25 & jump$distance > 0.25,]

# remove riders who say they've made an unreasonable number of rides
jump_freq_cleaned <- jump_freq[is.na(jump_freq$JUMP_freq_28days) | jump_freq$JUMP_freq_28days <= 28*3,]

# forensics: 15% of frequencies are zero, and 22% are NA
mean( jump_freq_cleaned$JUMP_freq_28days == 0, na.rm=TRUE ) # [1] 0.1505216
mean( is.na(jump_freq_cleaned$JUMP_freq_28days)) # [1] 0.2206736

# aggregate the trips that are car-substituting. For now, that's where the rider said
# their ride substituted for private car or car-share. Might also want to include carpooling.
jump_cleaned[[ "car_substitute" ]] <- ifelse( jump_cleaned[[ 'mode_switching' ]] %in%
                                c("Car, alone"), TRUE, FALSE)


# check for a difference in the length of trips that are car-substituting or not.
# there is an apparent difference, with car-substituting trips being a bit longer.
# this simple comparison doesn't account for the grouping by user ID
hist(log(jump_cleaned$distance[ jump_cleaned$car_substitute ]))
hist(log(jump_cleaned$distance[ !jump_cleaned$car_substitute ]))
with(jump_cleaned, t.test( log(distance[ car_substitute ]), log(distance[ !car_substitute ])))


# attach the ride frequecy for each trip
jump_cleaned[[ 'JUMP_freq_28days' ]] <-
  jump_freq_cleaned[[ 'JUMP_freq_28days' ]][ match(jump_cleaned$ResponseId, jump_freq_cleaned$ResponseId) ]

# 




