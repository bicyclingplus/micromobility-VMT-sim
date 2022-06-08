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

t2 <- trips %>% group_by(sampno, date) %>% summarize(totdist = sum(bktp_mt_total))

t3 <- t2
t3$sampno <- as.integer(t3$sampno)
t3 <- t3[ !is.na(t3$sampno), ]



metadata <- t3 %>% group_by(sampno) %>% summarize(n_sampno = n())

d_list <- list()
d_list[[ "sampno" ]] <- metadata[[ "sampno" ]]
d_list[[ "n_sampno" ]] <- metadata[[ "n_sampno" ]]
d_list[[ "totdist" ]] <- t3[[ "totdist" ]]

daily_sim_code <- " data{
      int n_sampno[325]; // number of rows for each starting state
      real totdist[107993];
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
        for ( i in 1:325 ) GAMMA_PARS[i] = [ logA_i[i], logB_i[i] ]';
        //for ( i in 1:325 ) GAMMA_PARS[i] = [ a_i[i], b_i[i] ]';
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
          
        }
      }
      
    } "

daily_sim_model <- stan_model(model_code = daily_sim_code)

fit3 <- sampling(daily_sim_model, d_list, cores=4)

trankplot(fit3)

#precis(fit3, 2)

draws_daily <- extract.samples(fit3)

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