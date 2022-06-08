data{
  
  ////////////////////////////////////////////////////////////////
  // Individual trip model:
  ////////////////////////////////////////////////////////////////
  
  // Data for bike share ride frequency
  int n_bikeshare_users;
  int n_rides;
  int ride_freq[n_bikeshare_users];


  // data for city-level random effects
  int n_cities;
  int city_ID[ n_bikeshare_users ];

  // How many users to simulate
  int n_pred;
  
  
  int<lower=2> K;           // number of mode choices
  
  int mode_sub_int[ n_rides ];            // Response variable (travel mode substitution):1=walk,2=ridehail,3=car alone,4=no trip,5=carpool,6= transit
  
  // Data for bikeshare trip_distance
  real ride_dist[ n_rides ];
  int user_ID[ n_rides ];
  
  
  
  ////////////////////////////////////////////////////////////////
  // City-level model:
  ////////////////////////////////////////////////////////////////
  
  
}
parameters{
  
  // Parameters for mode substitution
  vector[K-1] asc;                   //alternative specific constants (base=bike)
  vector[K-1] bdistance_log;		 // parameter vector for log distance
  
  vector[K-1] z_Bperson[ n_bikeshare_users ];        //unscaled person-level alternative specific constants
  cholesky_factor_corr[K-1] L_B;
  vector<lower=0>[K-1] sigma_mode_sub;		 //person-level standard devs. for each mode choice
  vector[K-1] Bperson_mean;
  
  real lmu_ranef_rate;
  real lmu_ranef_ride_dist;
  
  real<lower=0> sigma_obs_ride_dist;
  vector<lower=0>[2] sigma_ranef_city;
  vector<lower=0>[2] sigma_ranef_user;
  
  real log_rate[ n_bikeshare_users ];
  real log_ranef_ride_dist[ n_bikeshare_users ];
  
  real log_rate_city[ n_cities ];
  real log_ranef_ride_dist_city[ n_cities ];
  
  corr_matrix[2] rho_ranef_city;
  corr_matrix[2] rho_ranef_user;
  
}
transformed parameters{
  // scaled person-level mode substitution parameters
  vector[K-1] Bperson[ n_bikeshare_users ];

  // Transformed parameters for bikeshare frequency
  real<lower=0> ranef_rate[ n_bikeshare_users ];
  
  // Transformed parameters for bikeshare frequency
  ranef_rate = exp( log_rate );
  
  // scaled person-level mode substitution parameters
  for (i in 1:n_bikeshare_users) {
    Bperson[i,] = (diag_pre_multiply(sigma_mode_sub, L_B) * z_Bperson[i,]) + Bperson_mean;
  }
}
model{
  // //priors
  asc ~ student_t( 6, 0, 1 );
  bdistance_log ~ student_t( 6, 0, 1 );
  sigma_mode_sub ~ student_t( 6, 0, 1 );
  L_B ~ lkj_corr_cholesky( 2 );
  Bperson_mean ~ student_t(6, 0, 1);
  
  for (i in 1:n_bikeshare_users) {
    z_Bperson[ i ] ~ normal(0, 1);
  }
  
  
  // Likelihood for mode substitution
  for ( i in 1:n_rides ) {
    vector[K] theta;
    
    theta[1] = 0;			// base mode = bike, so set to 0.
    for ( x in 2:K ) {
      // theta[x] = asc[x-1] + Bperson[ user_ID[i], x-1 ] + bdistance_log[x-1] * log(ride_dist[ i ]);
      theta[x] = asc[x-1] + Bperson[ user_ID[i], x-1 ] + bdistance_log[x-1] * log(ride_dist[ i ]);
    }
    mode_sub_int[i] ~ categorical_logit(theta);
  }
  
  
  // Likelihood for bikeshare trip distance
  sigma_obs_ride_dist ~ exponential( 1 );
  sigma_ranef_city ~ exponential( 2 );
  rho_ranef_city ~ lkj_corr( 2 );
    
  // log mean parameters for the ride frequency and ride distance distributions
  lmu_ranef_ride_dist ~ student_t( 6, 0, 1 );
  lmu_ranef_rate ~ student_t( 6, 0, 1 );
  
  {
    vector[2] z;
    vector[2] mu;
    
    // generate city-level random effects of frequency and distance
    for (i in 1:n_cities)  {
        z = [ log_rate_city[i], log_ranef_ride_dist_city[i] ]';  // log mean ride frequency and log mean ride distance for this city 
        // mu = [ lmu_ranef_rate, lmu_ranef_ride_dist ]'; // log population mean ride frequency and log population mean ride distance
        mu = [ lmu_ranef_rate, lmu_ranef_ride_dist ]'; // log population mean ride frequency and log population mean ride distance
        
        // sample the varaiables
        z ~ multi_normal( mu, quad_form_diag(rho_ranef_city, sigma_ranef_city) );
    }
  }
  
  
  {
    vector[2] z;
    vector[2] mu;
    
    for ( i in 1:n_bikeshare_users ) {
      
      // define some variables: 
      z = [ log_rate[i], log_ranef_ride_dist[i] ]';  // log mean ride frequency and log mean ride distance for this user 
      // mu = [ lmu_ranef_rate, lmu_ranef_ride_dist ]'; // log population mean ride frequency and log population mean ride distance
      mu = [ log_rate_city[ city_ID[i] ], log_ranef_ride_dist_city[ city_ID[i] ] ]'; // log population mean ride frequency and log population mean ride distance
      
      // sample the varaiables
      z ~ multi_normal( mu, quad_form_diag(rho_ranef_user, sigma_ranef_user) );
      
      // sample the user's ride frequency based on their mean ride frequency (which was just sampled)
      ride_freq[ i ] ~ poisson( ranef_rate[i] );
    }
    
  }
  
  // get likelihood of the observed ride distances, based on the user-level mean ride distance (just sampled)
  for ( j in 1:n_rides ) {
    ride_dist[ j ] ~ lognormal( log_ranef_ride_dist[ user_ID[j] ], sigma_obs_ride_dist );
  }
  
  
  
  
  
  
  
} generated quantities {
  
  vector[ n_bikeshare_users + 2*n_rides ] log_lik;
  vector[K] theta;
  
  // variables to store samples of the number of rides, and how many are substituting for car travel
  int pred_n_rides[ n_pred ];
  int pred_n_rides_carsub[ n_pred ];
  
  // variable to store how many bike share miles substituted for car travel
  real pred_substituted_miles[ n_pred ];
  
  // variables to store the predicted samples of VMT, ridefrequency, and log mean ride distance
  // vector[ n_pred ] pred_vmt;
  real pred_rate[ n_pred ];
  real pred_log_ranef_ride_dist[ n_pred ];
    
  
  // calculate the log-likelihood of the observed ride freuqency
  for (i in 1:n_bikeshare_users) {
    log_lik[ i ] = poisson_lpmf( ride_freq[i] | ranef_rate[i]);
  }
  
  // calculate the log-likelihood of the observed ride distances
  for ( j in 1:n_rides ) {
    log_lik[ n_bikeshare_users + j ] = lognormal_lpdf( ride_dist[ j ] | log_ranef_ride_dist[ user_ID[j] ], sigma_obs_ride_dist );
  }
  
  // calculate the log-likelihood of the mode-substitution data
  for ( i in 1:n_rides ) {
    theta[1] = 0;
    
    for ( x in 2:K ){
      // theta[x] = asc[x-1] + Bperson[ user_ID[i], x-1 ] + bdistance_log[ x-1 ] * log(ride_dist[ i ]);
      theta[x] = asc[x-1] + Bperson[ user_ID[i], x-1 ] + bdistance_log[ x-1 ] * log(ride_dist[ i ]);
    }
    
    log_lik[ n_bikeshare_users + n_rides + i ] = categorical_logit_lpmf( mode_sub_int[i] | theta );
  }
  
  
  // predictions
  {
    vector[2] z;
    vector[2] mu;
    mu = [ lmu_ranef_rate, lmu_ranef_ride_dist ]';
    
    // predictions
    for ( i in 1:n_pred ) {
      
      // sample the predicted user's parameters for mean ride frequency and trip distance
      z = multi_normal_rng( mu, quad_form_diag(rho, sigma_ranef) );
      
      // sample an actual number of rides for this simulation of this user
      pred_rate[i] = exp( z[1] );
      pred_n_rides[ i ] = poisson_rng( pred_rate[i] );
      
      // 
      pred_log_ranef_ride_dist[i] = z[2];
      
      
      // simulate the rides for this user ( simulate pred_n_rides[i] )
      { 
        real pred_ride_dist;
        int pred_modesub;
        vector[K] pred_theta;
        vector[K-1] pred_Bperson;
        vector[K-1] z_pred;
        
        // These varaibles will be incremented as we run through the simulations
        pred_n_rides_carsub[i] = 0;
        pred_substituted_miles[i] = 0;
        
        // 
        for ( j in 1:(K-1) ) {
          z_pred[j] = normal_rng(0, 1);
        }
        pred_Bperson = (diag_pre_multiply(sigma_mode_sub, L_B) * z_pred) + Bperson_mean;
        
        for ( j in 1:pred_n_rides[i] ) {
          
          // simulate the distance of this ride
          pred_ride_dist = lognormal_rng( pred_log_ranef_ride_dist[ i ], sigma_obs_ride_dist );
          
          // calculate the mode-substitution model parameters, now that the user parameters and ride distance are sampled.
          pred_theta[1] = 0;
          for ( x in 2:K ) {
            pred_theta[x] = asc[x-1] + pred_Bperson[ x-1 ] + bdistance_log[x-1] * log(pred_ride_dist);
          }
          
          // simulate what mode this ride substitutes for
          pred_modesub = categorical_logit_rng( pred_theta );
          
          // The mode substitution for car_alone is 3, but stan's categorical predictions begin at one, not zero. So here, car substitution is indexed by 4.
          if (pred_modesub == 4) { 
            
            // we found a ride that substitutes for car travel, so increment the carsub and substituted_miles variables
            pred_n_rides_carsub[i] += 1;
            pred_substituted_miles[i] += pred_ride_dist;
          }
          
        }
      }
      
    }
  }
}





