# generates simulated samples of bike miles traveled and vehicle miles traveled for 2000 survey respondents
require("MASS")
require("dplyr")
require("data.table")


# Establish preliminaries -------------------------------------------------

# draws is the list of posterior samples from the model for weekly vmt

# here we set the number of weekly waves:
n_waves <- 30
n_households <- 100

# function to reconstruct the covariance matrix:
covar_mat <- function(sigma, rho) {
  tmp1 <- matrix(rho * sigma, 2, 2)
  diag(tmp1) <- diag(tmp1) / rho
  
  tmp2 <- diag(sigma)
  
  tmp1 %*% tmp2
}

sim_data <- list()

# Loop over households ----------------------------------------------------

for (hh in 1:n_households) {



# Sample the VMT ----------------------------------------------------------

# select the index that will govern this draw from the distribution of households:
i <- sample(1:nrow(draws$sigma), 1)

# sample the mean of the gamma parameters
gpar_mean <- exp( c( draws$logA[[i]], draws$logB[[i]]) )

# sample a new covariance matrix
gpar_Sigma <- covar_mat( draws$sigma[i, ], draws$Rho[[i]] )

# use the covariance matrix to sample new gamma parameters
gpar <- exp( mvrnorm( mu=gpar_mean, Sigma=gpar_Sigma ) )

# for this household, sample some weeks of VMT:
VMT <- rgamma(n=n_waves, gpar[[1]], gpar[[2]])




# Sample the number of bike share trips -----------------------------------

# bike share frequency is given over a 28 day period, so divide by four to get a one-week period.
mu <- mean(jump_freq_cleaned$JUMP_freq_28days, na.rm = TRUE) / 4
v <- var(jump_freq_cleaned$JUMP_freq_28days / 4, na.rm = TRUE) #really the division should be outside here,
#but that makes overdispersion too large.

# size parameter for the negative binomial
size = mu^2 / (v - mu)

# draw the number of bikeshare trips
n_bikeshare_trips <- rnbinom(n_waves, mu=mu, size=size)


# Sample lengths for the bikeshare trips ----------------------------------

# estimate the trip lengths have a gamma distribution
beta <- mean(jump_cleaned$distance) / var(jump_cleaned$distance)
alpha <- mean(jump_cleaned$distance) * beta

bikeshare_trip_lengths <- rgamma(sum(n_bikeshare_trips), alpha, beta)


  
# Estimate which trips are car-substituting ----------------------------------

# just a straight iid binomial draw, probably too simplistic
p <- mean(jump_cleaned$car_substitute)
car_sub_flag <- sample(c(0,1), size = sum(n_bikeshare_trips), prob=c(1-p, p), replace = TRUE)



# Summarize the bike share data for this household -----------------------------------

# create a data.frame to hold the result
wave <- vector()
for ( i in 1:n_waves ) wave <- c(wave, rep(i, n_bikeshare_trips[[i]]))
bikeshare_trips <- data.frame(wave)

bikeshare_trips$distance <- bikeshare_trip_lengths
bikeshare_trips$car_sub <- car_sub_flag

# sum up the amount of car-substitution
bikeshare_trips$car_sub_distance <- bikeshare_trip_lengths * car_sub_flag




# Summarize the overall data for this household ---------------------------


# Ensure that all waves are represented, even if there were no bikeshare trips in that wave:
bikeshare_trips <- cbind( bikeshare_trips, true_trip=rep(1, nrow(bikeshare_trips) ))

for (i in 1:n_waves) {
  if ( !(i %in% bikeshare_trips$wave) )
    bikeshare_trips[nrow(bikeshare_trips)+1, ] <- c(i, rep(0, ncol(bikeshare_trips) - 1) )
}

# compile results by wave
household <- bikeshare_trips %>%
  group_by(wave) %>%
  summarize(bs_distance=sum(distance),
            bs_n_trips=sum(true_trip),
            bs_n_car_sub=sum(car_sub),
            bs_car_sub_dist=sum(car_sub_distance))


household$vmt <- VMT - household$bs_car_sub_dist
household$id <- hh

household

sim_data[[hh]] <- household

}

sim_data <- rbindlist(sim_data)

