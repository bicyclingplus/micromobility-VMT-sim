# This script estimates a model for the weekly distance driven by a household.
# Model description:
# outcome: weekly car miles, assume Gamma distribution.
# Gamma parameters: shape and scale, specific to each car (there are repeated weekly measurements)
# Gamma parameters come from a multivariate log-normal hyperprior.

require("tidyverse")
require("rstan")
require("rethinking")
require("lubridate")

trips <- read_csv("~/Downloads/psrc_06_full_survey/v_gpstrips.csv")
trips$date <- lubridate::as_date(trips$bktp_start_date)
trips$year <- lubridate::year(trips$date)
trips$week <- lubridate::week(trips$date)
trips$weekday <- weekdays(trips$date)

# households that begin with zero may also appear with the leading zeroes stripped.
# these ought to be combined.
trips$sampno <- gsub("^0+([^0][[:digit:]]*)$", "\\1", trips$sampno) # ifelse( substr(trips$sampno, 1, 1) == '0', , trips$sampno)

# combine by sampno, year, week and filter out trips that look like glitches (no elapsed time)
t2 <- trips %>% filter( bktp_start_date != bktp_end_date & !fake_distance) %>% group_by(sampno, year, week) %>% summarize(totdist = sum(bktp_mt_total))

t3 <- t2
t3$sampno <- as.integer(t3$sampno)
t3 <- t3[ !is.na(t3$sampno), ]



metadata <- t3 %>% group_by(sampno) %>% summarize(n_sampno = n())

d_list <- list()
d_list[[ "sampno" ]] <- metadata[[ "sampno" ]]
d_list[[ "n_sampno" ]] <- metadata[[ "n_sampno" ]]
d_list[[ "totdist" ]] <- t3[[ "totdist" ]]

weekly_sim_model <- " data{
      int n_sampno[325]; // number of rows for each starting state
      real totdist[17728];
      int sampno[325];
    }
    parameters{
      real logA;
      real logB;
      corr_matrix[2] Rho;
      vector<lower=0>[2] sigma;
      vector[325] logA_i;
      vector[325] logB_i;
      
      //vector[325] a_i;
      //vector[325] b_i;
    }
    transformed parameters{
      real A;
      real B;
      
      vector[325] a_i;
      vector[325] b_i;
      
      A = exp( logA );
      B = exp( logB );
      
      a_i = exp( logA_i );
      b_i = exp( logB_i );
    }
    model{
    
      sigma ~ exponential( 1 );
      Rho ~ lkj_corr( 2 );

      logA ~ normal( 0, 1 );
      logB ~ normal( 0, 1 );

      {
        vector[2] MU;
        vector[2] GAMMA_PARS[325];
        
        MU = [ A, B ]';
        for ( i in 1:323 ) GAMMA_PARS[i] = [ logA_i[i], logB_i[i] ]';
        //for ( i in 1:323 ) GAMMA_PARS[i] = [ a_i[i], b_i[i] ]';
        GAMMA_PARS ~ multi_normal( MU, quad_form_diag( Rho, sigma ) );
      }
      
      {
        int m;
        m = 1;
        
        for ( j in 1:325 ) {
        
          for ( k in 1:n_sampno[j] ) {
            totdist[m] ~ gamma( a_i[j], b_i[j] );
            m = m+1;
          }
          
        }sampling()
      }
      
    } "
weekly_sim_model <- stan_model(model_code = weekly_sim_model)
