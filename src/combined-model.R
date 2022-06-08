# Read data and fix a few variables
d <- read.csv("data/Dataset_Stan.csv")
d$mode_sub <- as.factor(d$mode_mnl)
levels(d$mode_sub) <- c("bike","walk","ridehail","car, alone","no trip","carpool","transit")
d$distance_log <- log(d$distance)

library(brms)
fit.brms <- brm(mode_sub ~ (1|ID_n) + distance_log,
                data=d,
                prior = c(set_prior("normal(0,1.5)", class = "Intercept",dpar="mucaralone"),
                          set_prior("normal(0,1.5)", class = "Intercept",dpar="mucarpool"),
                          set_prior("normal(0,1.5)", class = "Intercept",dpar="munotrip"),
                          set_prior("normal(0,1.5)", class = "Intercept",dpar="muridehail"),
                          set_prior("normal(0,1.5)", class = "Intercept",dpar="mutransit"),
                          set_prior("normal(0,1.5)", class = "Intercept",dpar="muwalk"),
                          set_prior("normal(0,1.5)", class = "b",dpar="mucaralone"),
                          set_prior("normal(0,1.5)", class = "b",dpar="mucarpool"),
                          set_prior("normal(0,1.5)", class = "b",dpar="munotrip"),
                          set_prior("normal(0,1.5)", class = "b",dpar="muridehail"),
                          set_prior("normal(0,1.5)", class = "b",dpar="mutransit"),
                          set_prior("normal(0,1.5)", class = "b",dpar="muwalk"),
                          set_prior("student_t(6,0,1.5)", class = "sd",dpar="mucaralone"),
                          set_prior("student_t(6,0,1.5)", class = "sd",dpar="mucarpool"),
                          set_prior("student_t(6,0,1.5)", class = "sd",dpar="munotrip"),
                          set_prior("student_t(6,0,1.5)", class = "sd",dpar="muridehail"),
                          set_prior("student_t(6,0,1.5)", class = "sd",dpar="mutransit"),
                          set_prior("student_t(6,0,1.5)", class = "sd",dpar="muwalk")),
                family = categorical(),
                iter=2000,warmup=1000,cores=4,chains=4)


library(rstan)
data <- list(
  # Data for the model of mode substitution
  N=nrow(d), # number of observations
  K = 7, # number of modes
  S = length(unique(d$ID_n)), # number of people
  mode_sub_int = d$mode_mnl, # response (as integer)
  distance_log = d$distance_log, # log of distance
  ID = d$ID_n #, # person ID
  
  # Data for the VMT model
  # n_hhold = length( metadata[[ "n_sampno" ]] ),
  # n_obs = nrow(t3),
  # n_bikeshare = sum( !is.na(jump_freq_cleaned[[ 'JUMP_freq_28days' ]] ) ),
  # 
  # sampno = metadata[[ "sampno" ]],
  # n_sampno = metadata[[ "n_sampno" ]],
  # totdist = t3[[ "totdist" ]],
  
  # Data for the model of bikeshare use
  # ride_freq = jump_freq_cleaned[[ 'JUMP_freq_28days' ]][ !is.na(jump_freq_cleaned[[ 'JUMP_freq_28days' ]] ) ]
  
  
  
)       

# m <- stan_model('combined-model.stan')
m <- stan_model('src/mode_sub_mnl_distanceOnly.stan')
fit.stan <-sampling(m,
                    data = data,
                    iter = 2000,
                    warmup = 1000,
                    chains = 4,
                    cores = 4)

# compare models
summary(fit.brms)
summary(fit.stan,pars=c("asc","bdistance_log","sigma"))$summary

# they look the same to me, and mine runs faster!!! :)
