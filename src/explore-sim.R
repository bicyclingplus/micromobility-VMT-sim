# This script estimates a model for the weekly distance driven by a household.
# Model description:
# outcome: weekly car miles, assume Gamma distribution.
# Gamma parameters: shape and scale, specific to each car (there are repeated weekly measurements)
# Gamma parameters come from a multivariate log-normal hyperprior.

require("tidyverse")
require("rstan")
require("rethinking")
require("lubridate")

source("src/jump-survey.R")

trips <- read_csv("~/Downloads/psrc_06_full_survey/v_gpstrips.csv")
trips$date <- lubridate::as_date(trips$bktp_start_date)
trips$year <- lubridate::year(trips$date)
trips$week <- lubridate::week(trips$date)
trips$weekday <- weekdays(trips$date)

# households that begin with zero may also appear with the leading zeroes stripped.
# these ought to be combined.
trips$sampno <- gsub("^0+([^0][[:digit:]]*)$", "\\1", trips$sampno) # ifelse( substr(trips$sampno, 1, 1) == '0', , trips$sampno)

# group by sampno, year, week and filter out trips that look like glitches (no elapsed time)
t2 <- trips %>%
  filter( bktp_start_date != bktp_end_date & !fake_distance) %>%
  group_by(sampno, year, week) %>%
  summarize(totdist = sum(bktp_mt_total))

t3 <- t2
t3$user_id <- unclass( as.factor(t3$sampno) )
t3 <- t3[ !is.na(t3$user_id), ]

# We use a lognormal distribution that can't abide exactly zero VMT. Change any zeroes to ones (a very minor change)
t3$totdist[ t3$totdist == 0 ] <- 1

# metadata <- t3 %>% filter(user_id != 99999) %>% group_by(user_id) %>% summarize(n_sampno = n())

d_list <- list()
d_list[[ 'n_hhold' ]] <- max( t3[[ "user_id" ]] )
d_list[[ 'n_obs' ]] <- nrow(t3)
# d_list[[ 'n_bikeshare' ]] <- sum( !is.na(jump_freq_cleaned[[ 'JUMP_freq_28days' ]] ) )

d_list[[ "user_id" ]] <- t3[[ "user_id" ]]
# d_list[[ "n_sampno" ]] <- metadata[[ "n_sampno" ]]
d_list[[ "totdist" ]] <- t3[[ "totdist" ]]
# d_list[[ "ride_freq" ]] <- jump_freq_cleaned[[ 'JUMP_freq_28days' ]][ !is.na(jump_freq_cleaned[[ 'JUMP_freq_28days' ]] ) ]

weekly_sim_model <- " data{
      int n_hhold;
      int n_obs;
      //int n_bikeshare;
      
      int user_id[n_obs];
      //int n_sampno[n_hhold]; // number of rows for each starting state
      real totdist[n_obs];
      //int sampno[n_hhold];
      //int ride_freq[n_bikeshare];
    }
    parameters{
      real logA;
      real logB;
      corr_matrix[2] Rho;
      vector<lower=0>[2] sigma;
      vector[n_hhold] logA_i;
      vector[n_hhold] logB_i;
      
      //real laa;
      //real lbb;
      
      //real<lower=0> rate;
      
      
    }
    transformed parameters{
      real A;
      real B;
      
      vector[n_hhold] a_i;
      vector[n_hhold] b_i;
      
      //real aa;
      //real bb;
      
      A = exp( logA );
      B = exp( logB );
      
      a_i = exp( logA_i );
      b_i = exp( logB_i );
      
      //aa = exp( laa );
      //bb = exp( lbb );
    }
    model{
    
      sigma ~ exponential( 1 );
      Rho ~ lkj_corr( 2 );

      logA ~ student_t( 6, 0, 1 );
      logB ~ student_t( 6, 0, 1 );

      {
        vector[2] MU;
        vector[2] GAMMA_PARS[n_hhold];
        
        MU = [ logA, logB ]';
        for ( i in 1:n_hhold ) GAMMA_PARS[i] = [ logA_i[i], logB_i[i] ]';
        GAMMA_PARS ~ multi_normal( MU, quad_form_diag( Rho, sigma ) );
      }
      
      {
        
        //int m;
        //m = 1;
        //
        //for ( j in 1:n_hhold ) {
        //
        //  for ( k in 1:n_sampno[j] ) {
        //    totdist[m] ~ gamma( a_i[j], b_i[j] );
        //    m = m+1;
        //  }
        //  
        //}
        
        for ( i in 1:n_obs) {
          totdist[i] ~ gamma( a_i[ user_id[i] ], b_i[ user_id[i] ]);
        }
      }
      
      //laa ~ normal( 0, 1 );
      //lbb ~ normal( 0, 1 );
      //rate ~ gamma( aa, bb );
      //ride_freq ~ poisson( rate );
      
    } generated quantities {
      vector[n_obs] log_lik;
      
      for ( i in 1:n_obs) {
        log_lik[i] = gamma_lpdf( totdist[i] | a_i[ user_id[i] ], b_i[ user_id[i] ]);
      }
      
    } "
weekly_sim_model <- stan_model(model_code = weekly_sim_model)

fit2 <- sampling(weekly_sim_model,
                 d_list,
                 iter=600,
                 warmup=300,
                 chains=1,
                 cores=1
)


weekly_sim_model3 <- " data{
      int n_hhold;
      int n_obs;
      //int n_bikeshare;
      
      int user_id[n_obs];
      //int n_sampno[n_hhold]; // number of rows for each starting state
      real totdist[n_obs];
      //int sampno[n_hhold];
      //int ride_freq[n_bikeshare];
            int n_pred;
      int n_pred_weeks;
    }
    parameters{
      real logA;
      //real logB;
      //corr_matrix[2] Rho;
      real<lower=0> sigma;
      vector[n_hhold] logA_i;
      //vector[n_hhold] logB_i;
      
      //real laa;
      //real lbb;
      
      //real<lower=0> rate;
      real<lower=0> s;

    }
    transformed parameters{
      real A;
      //real B;
      
      vector[n_hhold] a_i;
      //vector[n_hhold] b_i;
      
      //real aa;
      //real bb;
      
      A = exp( logA );
      //B = exp( logB );
      
      a_i = exp( logA_i );
      //b_i = exp( logB_i );
      
      //aa = exp( laa );
      //bb = exp( lbb );
    }
    model{
    
      sigma ~ exponential( 1 );
      //Rho ~ lkj_corr( 2 );

      logA ~ student_t( 6, 0, 1 );
      //logB ~ student_t( 6, 0, 1 );

      //{
        //vector[2] MU;
        //vector[2] GAMMA_PARS[n_hhold];
        
        //MU = [ logA, logB ]';
        //for ( i in 1:n_hhold ) GAMMA_PARS[i] = [ logA_i[i], logB_i[i] ]';
        //GAMMA_PARS ~ multi_normal( MU, quad_form_diag( Rho, sigma ) );
        
        
      //}
      
      s ~ student_t( 6, 0, 1 );
      logA_i ~ normal( logA, sigma );
      
      {
        
        //int m;
        //m = 1;
        //
        //for ( j in 1:n_hhold ) {
        //
        //  for ( k in 1:n_sampno[j] ) {
        //    totdist[m] ~ gamma( a_i[j], b_i[j] );
        //    m = m+1;
        //  }
        //  
        //}
        
        for ( i in 1:n_obs) {
          //totdist[i] ~ gamma( a_i[ user_id[i] ], b_i[ user_id[i] ]);
          //totdist[i] ~ normal( a_i[ user_id[i] ], s );
          totdist[i] ~ lognormal( logA_i[ user_id[i] ], s );
        }
      }
      
      //laa ~ normal( 0, 1 );
      //lbb ~ normal( 0, 1 );
      //rate ~ gamma( aa, bb );
      //ride_freq ~ poisson( rate );
      
    } generated quantities {
      vector[n_pred] pred_vmt[n_pred_weeks];
      
      vector[n_pred] pred_a_i;
      //vector[n_pred] pred_b_i;
      
      vector[n_obs] log_lik;
      
      
      //vector[2] MU;
      //vector[2] logab;
        
        
      for ( i in 1:n_obs) {
        log_lik[i] = lognormal_lpdf( totdist[i] | logA_i[ user_id[i] ], s);
      }
      
      

      //MU = [ logA, logB ]';
      //for ( i in 1:n_pred ) {
      //  //GAMMA_PARS = [ pred_log_a_i[i], pred_log_b_i[i] ]';
      //  logab = multi_normal_rng( MU, quad_form_diag( Rho, sigma ) );
      //  
      //        
      //  pred_a_i[i] = logab[1];
      //  pred_b_i[i] = exp( logab[2] );
      //
      //}
 
      for (i in 1:n_pred) {
        pred_a_i[i] = normal_rng(logA, sigma);
        
      
        for (j in 1:n_pred_weeks) {
          pred_vmt[j][i] = lognormal_rng( pred_a_i[ i ], s);
        }
      }
      
    }"
weekly_sim_model3 <- stan_model(model_code = weekly_sim_model3)

fit3 <- sampling(weekly_sim_model3,
                 d_list,
                 iter=600,
                 warmup=300,
                 chains=1,
                 cores=1
)



weekly_sim_model4 <- " data{
      int n_hhold;
      int n_obs;
      
      int user_id[n_obs];
      real totdist[n_obs];
      
      int n_pred;
      int n_pred_weeks;
    }
    parameters{
      real logA;
      real logB;
      corr_matrix[2] Rho;
      vector<lower=0>[2] sigma;
      vector[n_hhold] logA_i;
      vector[n_hhold] logB_i;
      
    }
    transformed parameters{
      real A;
      real B;
      
      vector[n_hhold] a_i;
      vector[n_hhold] b_i;
      
      A = exp( logA );
      B = exp( logB );
      
      a_i = exp( logA_i );
      b_i = exp( logB_i );
    }
    model{
    
      sigma ~ exponential( 1 );
      Rho ~ lkj_corr( 4 );

      logA ~ student_t( 6, 0, 1 );
      logB ~ student_t( 6, 0, 1 );


      {
        vector[2] MU;
        vector[2] GAMMA_PARS[n_hhold];
        
        MU = [ logA, logB ]';
        for ( i in 1:n_hhold ) GAMMA_PARS[i] = [ logA_i[i], logB_i[i] ]';
        GAMMA_PARS ~ multi_normal( MU, quad_form_diag( Rho, sigma ) );
      }
      

      for ( i in 1:n_obs) {
        //totdist[i] ~ gamma( a_i[ user_id[i] ], b_i[ user_id[i] ]);
        //totdist[i] ~ normal( a_i[ user_id[i] ], s );
        totdist[i] ~ lognormal( logA_i[ user_id[i] ], b_i[ user_id[i] ]);
      }


    } generated quantities {
      vector[n_pred] pred_vmt[n_pred_weeks];
      
      vector[n_pred] pred_a_i;
      vector[n_pred] pred_b_i;
      
      vector[n_obs] log_lik;
      
      
      vector[2] MU;
      vector[2] logab;
        
        
      for ( i in 1:n_obs) {
        log_lik[i] = lognormal_lpdf( totdist[i] | logA_i[ user_id[i] ], b_i[ user_id[i] ]);
      }
      
      

      MU = [ logA, logB ]';
      for ( i in 1:n_pred ) {
        //GAMMA_PARS = [ pred_log_a_i[i], pred_log_b_i[i] ]';
        logab = multi_normal_rng( MU, quad_form_diag( Rho, sigma ) );
        
              
        pred_a_i[i] = logab[1];
        pred_b_i[i] = exp( logab[2] );
    
      }
      
            
      
      for (i in 1:n_pred) {
        for (j in 1:n_pred_weeks) {
          pred_vmt[j][i] = lognormal_rng( pred_a_i[ i ], pred_b_i[ i ]);
        }
      }
      
    } "
weekly_sim_model4 <- stan_model(model_code = weekly_sim_model4)

fit4 <- sampling(weekly_sim_model4,
                 d_list,
                 iter=600,
                 warmup=300,
                 chains=1,
                 cores=1
)





weekly_sim_model5 <- " data{
      int n_hhold;
      int n_obs;
      //int n_bikeshare;
      
      int user_id[n_obs];
      //int n_sampno[n_hhold]; // number of rows for each starting state
      real totdist[n_obs];
      //int sampno[n_hhold];
      //int ride_freq[n_bikeshare];
    }
    parameters{
      real logA;
      real logB;
      corr_matrix[2] Rho;
      vector<lower=0>[2] sigma;
      vector[n_hhold] logA_i;
      vector[n_hhold] logB_i;
      
      //real laa;
      //real lbb;
      
      //real<lower=0> rate;
      //real<lower=0> s;
      
    }
    transformed parameters{
      real A;
      real B;
      
      vector[n_hhold] a_i;
      vector[n_hhold] b_i;
      
      //real aa;
      //real bb;
      
      A = exp( logA );
      B = exp( logB );
      
      a_i = exp( logA_i );
      b_i = exp( logB_i );
      
      //aa = exp( laa );
      //bb = exp( lbb );
    }
    model{
    
      sigma ~ exponential( 1 );
      Rho ~ lkj_corr( 4 );

      logA ~ student_t( 6, 0, 1 );
      logB ~ student_t( 6, 0, 1 );

      {
        vector[2] MU;
        vector[2] GAMMA_PARS[n_hhold];
        
        MU = [ logA, logB ]';
        for ( i in 1:n_hhold ) GAMMA_PARS[i] = [ logA_i[i], logB_i[i] ]';
        GAMMA_PARS ~ multi_normal( MU, quad_form_diag( Rho, sigma ) );
        
        
      }
      
      //s ~ student_t( 6, 0, 1 );
      //logA_i ~ normal( logA, sigma );
      
      {
        
        //int m;
        //m = 1;
        //
        //for ( j in 1:n_hhold ) {
        //
        //  for ( k in 1:n_sampno[j] ) {
        //    totdist[m] ~ gamma( a_i[j], b_i[j] );
        //    m = m+1;
        //  }
        //  
        //}
        
        for ( i in 1:n_obs) {
          //totdist[i] ~ gamma( a_i[ user_id[i] ], b_i[ user_id[i] ]);
          //totdist[i] ~ normal( a_i[ user_id[i] ], s );
          totdist[i] ~ normal( a_i[ user_id[i] ], b_i[ user_id[i] ]);
        }
      }
      
      //laa ~ normal( 0, 1 );
      //lbb ~ normal( 0, 1 );
      //rate ~ gamma( aa, bb );
      //ride_freq ~ poisson( rate );
      
    } generated quantities {
      vector[n_obs] log_lik;
      
      for ( i in 1:n_obs) {
        log_lik[i] = normal_lpdf( totdist[i] | a_i[ user_id[i] ], b_i[ user_id[i] ]);
      }
      
    } "
weekly_sim_model5 <- stan_model(model_code = weekly_sim_model5)

fit5 <- sampling(weekly_sim_model5,
                 d_list,
                 iter=600,
                 warmup=300,
                 chains=1,
                 cores=1
)

trankplot(fit2)

precis(fit2, 2)

draws <- extract.samples(fit4)

# c(a_i, b_i)[sampno] ~ multi_normal( c(A, B), Rho, sigma ),
# 
# tranpars> vector[1]:A <- exp( logA ),
# tranpars> vector[1]:B <- exp( logB ),
# 
# logA ~ normal(0, 1),
# logB ~ normal(0, 1),
# 
# totdist ~ gamma( a_i, b_i ),
# 
# 
# vector[3] pS;
# vector[3] pC;
# vector[3] pI;
# real sum_pC;
# 
# // prior distributions for the parameters (total guesswork, no thought went into these)
# alpha ~ normal(1, 1);
# beta ~ normal(1, 1);
# delta ~ normal(1, 1);
# 
# // populate the transition probabilities.
# 
# // transition probability from state S
# pS[1] = exp(-beta * I_count);
# pS[2] = 0;
# pS[3] = 1 - exp(-beta * I_count);
# 
# // transition probability from state C
# pC[1] = 1 - exp(-delta);
# pC[2] = exp(-delta - alpha - beta * I_count);
# pC[3] = 1 - exp(-alpha - beta * I_count);
# 
# sum_pC = pC[1] + pC[2] + pC[3];
# 
# pC[1] = pC[1] / sum_pC;
# pC[2] = pC[2] / sum_pC;
# pC[3] = pC[3] / sum_pC;
# 
# // transition probability from state I
# pI[1] = 0;
# pI[2] = 0;
# pI[3] = 1;
# 
# // define the distribution lf the observed data
# S ~ categorical( pS );
# C ~ categorical( pC );
# I ~ categorical( pI );