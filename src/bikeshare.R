require( "readr" )
require( "dplyr" )
require( "rstan" )

# import the data
jump_trips <- read_csv("data/jump_sacramento_data/trips_cleaned.csv")
jump_freq <- read_csv( "data/jump_sacramento_data/freq.csv")

# import city-level data
substitution <- read_excel("~/Box/NAMP/study_design/micromobility_statistics_updated_v1.xlsx", sheet="Mode Substitution")
distance <- read_excel("~/Box/NAMP/study_design/micromobility_statistics_updated_v1.xlsx", sheet="Trip distances")
frequency <- read_excel("~/Box/NAMP/study_design/micromobility_statistics_updated_v1.xlsx", sheet="Trip Frequency")

# remove incomplete records
jump_freq[[ "ResponseId.1" ]] <- NULL
jump_freq_cleaned <- jump_freq[ complete.cases(jump_freq), ]

# join the frequency and distance data, and create an ID for each user
unique_freq <- jump_freq_cleaned %>% group_by(ResponseId) %>% summarize()
unique_freq$user_ID <- 1:nrow(unique_freq)
jump_freq_cleaned <- left_join(jump_freq_cleaned, unique_freq, by="ResponseId") %>% arrange( user_ID )
jump_trips <- left_join(jump_trips, jump_freq_cleaned, by="ResponseId")
jump_trips <- jump_trips[ complete.cases(jump_trips), ]


# import stan models
m_pred <- stan_model('scratch/bikeshare-pred.stan')




# Import mode-substitution data -------------------------------------------

# Read data and fix a few variables - here I am using Dillon's Dataset_stan which is slightly different from the files that I imported above.
d <- read.csv("data/Dataset_Stan.csv")
d$mode_sub <- as.factor(d$mode_mnl)
levels(d$mode_sub) <- c("bike","walk","ridehail","car, alone","no trip","carpool","transit")


# link the mode-substitution data with the frequency data --------------
d_joined <- left_join( d, jump_freq_cleaned, by=c("ID" = "ResponseId") )
d_joined <- d_joined[ !is.na(d_joined$user_ID), ] # drop rows where the user ID is NA 


# define the data to pass into the stan model
d_pred <- list(
  K = 7, # number of modes
  mode_sub_int = d_joined$mode_mnl, # response (as integer)
  
  n_bikeshare_users = nrow(jump_freq_cleaned),
  ride_freq = jump_freq_cleaned$JUMP_freq_28days,
  
  n_rides = nrow(d_joined), # how many rides are in the linked data
  ride_dist = d_joined$distance,
  
  
  user_ID = d_joined$user_ID,
  
  # how many users to simulate
  n_pred = 10
)


# run the chains - few iteraions for prototyping
fit_pred <- sampling(m_pred,
                    data = d_pred,
                    iter = 600,
                    warmup = 300,
                    chains = 1,
                    cores = 1)

