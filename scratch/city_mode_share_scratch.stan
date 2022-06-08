data{
  
  // int n_bikeshare_users;
  
  // data for city-level random effects
  // int n_cities;
  // int n_cities_freq; // For how many cities do we have trip frequency data?
    // int n_cities_dist; // for how many cities do we have trip distance data?
    // int n_cities_freq_and_dist; // for how many cities do we have trip frequency and distance data?
    // int n_cities_freq_no_dist; // for how many cities do we have trip frequency but no distance?
    // int n_cities_dist_no_freq; // for how many cities do we have trip distance but not frequency?
    // int n_cities_mode_sub; // for how many cities do we have mode substitution data?
    // int n_riders[ n_cities ]; // how many users in each city
  // int n_trips[ n_cities ]; // how many trips in each city
  
  
  // int n_pred_freq_and_dist;
  
  
  // categories: mode substitution, trip frequency, trip distance
  int n_YYY;
  int n_YYN;
  int n_YNY;
  int n_YNN;
  int n_NYY;
  int n_NYN;
  int n_NNY;
  int n_NNN; // full predictions (no observed data)
  // int n_cities_pred;
  
  // int city_ID[ n_bikeshare_users ];
  
  int<lower=2> K;           // number of mode choices
    
  // int scooter_indicator[ n_cities_dist_no_freq ];
  // int scooter_indicator[ n_cities_freq_and_dist + n_pred_freq_and_dist ];
  int scooter_indicator[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + n_NNN ];
  
  // real trip_frequency_paired[ n_cities_freq_and_dist ];
  // real trip_distance_paired[ n_cities_freq_and_dist ];
  // // real trip_frequency_only[ n_cities_freq_no_dist ];
  // // real trip_distance_only[ n_cities_dist_no_freq ];
  real city_trip_distance[ n_YYY + n_YNY + n_NYY + n_NNY ];
  real city_trip_frequency[ n_YYY + n_YYN + n_NYY + n_NYN ];
  vector<lower=0>[ K ] city_mode_sub[ n_YYY + n_YYN + n_YNY + n_YNN ];            // Response variable (travel mode substitution):1=walk,2=ridehail,3=car alone,4=no trip,5=carpool,6= transit



  
  // vector[ K ] mode_sub_freq[ n_cities_mode_sub ];            // Response variable (travel mode substitution):1=walk,2=ridehail,3=car alone,4=no trip,5=carpool,6= transit
  // int mean_ride_dist[ n_cities_mode_sub ];
}
parameters {
  
  // Parameters for mode substitution
  // vector[K-1] asc;                   //alternative specific constants (base=bike)
  // vector[K-1] bdistance_log;		 // parameter vector for log distance
  vector[ K ] A;
  vector[ K ] mu_A;
  
  // vector[K-1] z_Bcity[ n_cities_mode_sub ];        //unscaled city-level alternative specific constants
  // cholesky_factor_corr[K-1] L_B;
  cholesky_factor_corr[ K ] L_A;
  
  // vector<lower=0>[K-1] sigma_mode_sub;		 //city-level standard devs. for each mode choice
  vector<lower=0>[ K ] sigma_A;
  // vector[K-1] Bcity_mean;
  
  
  // // .per-city random effects
  // real log_ranef_rate_city[ n_cities_freq_no_dist ];
  // real log_ranef_dist_city[ n_cities_dist_no_freq ];
  
  // parameters of the per-city random effect distribution
  corr_matrix[2] rho_ranef_city; // correlation between trip frequency and distance random effects
  real lmu_ranef_rate; // mean of city-level random effects (1)
  real lmu_ranef_dist; // mean of city-level random effects (2)
  vector<lower=0>[2] sigma_ranef_city; // variance of city-level random effects
  // real sigma_ranef_rate_city;
  // real sigma_ranef_dist_city;
  
  real beta_scooter_dist;
  
  // real sigma_obs_ride_dist;
  // real sigma_trip_freq_city;
  
  // real trip_frequency_pred[ n_pred_freq_and_dist ];
  // real trip_distance_pred[ n_pred_freq_and_dist ];
  real city_trip_frequency_pred[ n_YNY + n_YNN + n_NNY + n_NNN ];
  real city_trip_distance_pred[ n_YYN + n_YNN + n_NYN + n_NNN  ];
  vector<lower=0>[ K ] city_mode_sub_pred_unscaled[ n_NYY + n_NYN + n_NNY + n_NNN ];
}
transformed parameters {
  // // scaled person-level mode substitution parameters
  // vector[K-1] Bcity[ n_cities_mode_sub ];
  
  // for (i in 1:n_cities_mode_sub) {
  //   // scaled person-level mode substitution parameters
  //   Bcity[i,] = (diag_pre_multiply(sigma_mode_sub, L_B) * z_Bcity[i,]) + Bcity_mean;
  // }
  
  // 
  vector[ K ] city_mode_sub_pred[ n_NYY + n_NYN + n_NNY + n_NNN ];
  
  // real trip_frequency_combined[ n_cities_freq_and_dist + n_pred_freq_and_dist ];
  // real trip_distance_combined[ n_cities_freq_and_dist+ n_pred_freq_and_dist ];
  real city_trip_frequency_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + n_NNN ];
  real city_trip_distance_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + n_NNN ];
  vector[ K ] city_mode_sub_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + n_NNN ];
  
  // for (i in 1:n_cities_freq_and_dist ) {
  //   trip_frequency_combined[ i ] = trip_frequency_paired[ i ];
  //   trip_distance_combined[ i ] = trip_distance_paired[ i ];
  // }
  // 
  // for (i in 1:n_pred_freq_and_dist) {
  //   trip_frequency_combined[ n_cities_freq_and_dist + i ] = trip_frequency_pred[ i ];
  //   trip_distance_combined[ n_cities_freq_and_dist + i ] = trip_distance_pred[ i ];
  // }
  
  // scale the mode substitution predictions
  for (i in 1:(n_NYY + n_NYN + n_NNY + n_NNN) ) {
    city_mode_sub_pred[ i ] = city_mode_sub_pred_unscaled[ i ] / sum( city_mode_sub_pred_unscaled[ i ] );
  }
  
  // start with the cities where we have all three kinds of data
  for (i in 1:n_YYY ) {
    city_mode_sub_combined[ i ] = city_mode_sub[ i ];
    city_trip_frequency_combined[ i ] = city_trip_frequency[ i ];
    city_trip_distance_combined[ i ] = city_trip_distance[ i ];
  }

  // now cities where we know mode substitution and trip frequency, but not distance
  for (i in 1:n_YYN ) {
    city_mode_sub_combined[ n_YYY + i ] = city_mode_sub[ n_YYY + i ];
    city_trip_frequency_combined[ n_YYY + i ] = city_trip_frequency[ n_YYY + i ];
    city_trip_distance_combined[ n_YYY + i ] = city_trip_distance_pred[ i ];
  }
  
  // now cities where we know mode substitution and trip distance, but not frequency
  for (i in 1:n_YNY ) {
    city_mode_sub_combined[ n_YYY + n_YYN + i ] = city_mode_sub[ n_YYY + n_YYN + i ];
    city_trip_frequency_combined[ n_YYY + n_YYN + i ] = city_trip_frequency_pred[ i ];
    city_trip_distance_combined[ n_YYY + n_YYN + i ] = city_trip_distance[ n_YYY + i ];
  }
  
  // now cities where we know mode substitution only
  for (i in 1:n_YNN ) {
    city_mode_sub_combined[ n_YYY + n_YYN + n_YNY + i ] = city_mode_sub[ n_YYY + n_YYN + n_YNY + i ];
    city_trip_frequency_combined[ n_YYY + n_YYN + n_YNY + i ] = city_trip_frequency_pred[ n_YNY + i ];
    city_trip_distance_combined[ n_YYY + n_YYN + n_YNY + i ] = city_trip_distance_pred[ n_YYN + i ];
  }
  
  // now cities where we know trip frequency and trip distance, but not mode substitution
  for (i in 1:n_NYY ) {
    city_mode_sub_combined[ n_YYY + n_YYN + n_YNY + n_YNN + i ] = city_mode_sub_pred[ i ];
    city_trip_frequency_combined[ n_YYY + n_YYN + n_YNY + n_YNN + i ] = city_trip_frequency[ n_YYY + n_YYN + i ];
    city_trip_distance_combined[ n_YYY + n_YYN + n_YNY + n_YNN + i ] = city_trip_distance[ n_YYY + n_YNY + i ];
  }
  
  // now cities where we know trip frequency but not trip distance nor mode substitution
  for (i in 1:n_NYN ) {
    city_mode_sub_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + i ] = city_mode_sub_pred[ n_NYY + i ];
    city_trip_frequency_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + i ] = city_trip_frequency[ n_YYY + n_YYN + n_NYY + i ];
    city_trip_distance_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + i ] = city_trip_distance_pred[ n_YYN + n_YNN + i ];
  }
  
  // now cities where we know trip distance but not trip frequency nor mode substitution
  for (i in 1:n_NNY ) {
    city_mode_sub_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + i ] = city_mode_sub_pred[ n_NYY + n_NYN + i ];
    city_trip_frequency_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + i ] = city_trip_frequency_pred[ n_YNY + n_YNN + i ];
    city_trip_distance_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + i ] = city_trip_distance[ n_YYY + n_YNY + n_NYY + i ];
  }
  
  // now cities where we know nothing (full prediction)
  for (i in 1:n_NNN ) {
    city_mode_sub_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + i ] = city_mode_sub_pred[ n_NYY + n_NYN + n_NNY + i ];
    city_trip_frequency_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + i ] = city_trip_frequency_pred[ n_YNY + n_YNN + n_NNY + i ];
    city_trip_distance_combined[ n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + i ] = city_trip_distance_pred[ n_YYN + n_YNN + n_NYN + i ];
  }
  
}
model {
  
  int n;
  
  n = n_YYY + n_YYN + n_YNY + n_YNN + n_NYY + n_NYN + n_NNY + n_NNN;
  
  // parameters for the city-level random effect ride frequency and ride distance distributions
  lmu_ranef_dist ~ student_t( 6, 0, 1 );
  lmu_ranef_rate ~ student_t( 6, 0, 1 );
  // sigma_ranef_rate_city ~ exponential( 2 );
  // sigma_ranef_dist_city ~ exponential( 2 );
  sigma_ranef_city ~ exponential( 2 );
  rho_ranef_city ~ lkj_corr( 2 );
  
  
  
  beta_scooter_dist ~ student_t(6, 0, 1);
  
  // //priors
  mu_A ~ student_t(6, 0, 1);
  // asc ~ student_t( 6, 0, 1 );
  // bdistance_log ~ student_t( 6, 0, 1 );
  // sigma_mode_sub ~ student_t( 6, 0, 1 );
  sigma_A ~ student_t( 6, 0, 1 );
  // L_B ~ lkj_corr_cholesky( 2 );
  L_A ~ lkj_corr_cholesky( 2 );
  // Bcity_mean ~ student_t(6, 0, 1);
  
  // for (i in 1:n_cities_mode_sub) {
  //   z_Bcity[ i ] ~ normal(0, 1);
  // }
  
  
  A ~ multi_normal_cholesky( mu_A, diag_pre_multiply(sigma_A, L_A) );
  
  // Likelihood for mode substitution
  for ( i in 1:n ) {
    vector[K] log_theta;
    // vector[K] theta;
    
    log_theta[1] = 0;			// base mode = bike, so set to 0.
    for ( j in 1:K ) {
      // theta[j] = asc[j-1] + Bperson[ user_ID[i], x-1 ] + bdistance_log[j-1] * log(ride_dist[ i ]);
      // log_theta[j] = asc[j-1] + Bcity[ i, j-1 ] + bdistance_log[j-1] * log(mean_ride_dist[ i ]);
      log_theta[j] = A[j];
    }
    
    // theta = exp(log_theta);
    city_mode_sub_combined[i] ~ dirichlet( exp(log_theta) );
    
    // trip_frequency_only[ i ] ~ lognormal( A[ K+1 ], sigma_ranef_rate_city );
    // trip_distance_only[ i ] ~ lognormal( A[ K+2 ], sigma_ranef_dist_city );
    
  }
  
  // for (i in 1:n_cities_freq_no_dist) {
  //   trip_frequency_only[ i ] ~ lognormal( lmu_ranef_rate, sigma_ranef_rate_city );
  // }
  // 
  // for (i in 1:n_cities_dist_no_freq) {
  //   trip_distance_only[ i ] ~ lognormal( lmu_ranef_dist + beta_scooter_dist * scooter_indicator[i], sigma_ranef_dist_city );
  // }
  
  {
    vector[2] mu;
    vector[2] z;
    
    
    
    // for (i in 1:(n_cities_freq_and_dist+n_pred_freq_and_dist)) {
    for (i in 1:n) {
      mu = [ lmu_ranef_rate, lmu_ranef_dist + beta_scooter_dist * scooter_indicator[i] ]';
      // z = [ trip_frequency_paired[i], trip_distance_paired[i] ]';
      z = [ city_trip_frequency_combined[i], city_trip_distance_combined[i] ]';
      z ~ multi_normal_cholesky( mu, quad_form_diag(rho_ranef_city, sigma_ranef_city) );
      // trip_frequency_only[ i ] ~ lognormal( lmu_ranef_rate, sigma_ranef_rate_city );
    }
  }
  
  // parameters for individual trips
  // sigma_obs_ride_dist ~ exponential( 1 );
  
  // {
    //   vector[2] z;
    //   vector[2] mu;
    //   
      //   // generate city-level random effects of frequency and distance
    //   for (i in 1:n_cities)  {
      //       z = [ log_ranef_rate_city[i], log_ranef_ride_dist_city[i] ]';  // log mean ride frequency and log mean ride distance for this city 
  //       // mu = [ lmu_ranef_rate, lmu_ranef_ride_dist ]'; // log population mean ride frequency and log population mean ride distance
      //       mu = [ lmu_ranef_rate, lmu_ranef_ride_dist ]'; // log population mean ride frequency and log population mean ride distance
  //       
  //       // sample the varaiables
  //       // z ~ multi_normal( mu, quad_form_diag(rho_ranef_city, sigma_ranef_city) );
  //   }
  // }
  // 
  // 
  // for (i in 1:n_cities_freq_no_dist ) {
  //   log_ranef_rate_city[i] ~ normal( lmu_ranef_rate, sigma_ranef_rate_city );
  //   // log_ranef_dist_city[i] ~ normal( lmu_ranef_dist, sigma_ranef_dist_city );
  //   
  //   // trip_distance_paired[ i ] ~ lognormal( log_ranef_ride_dist_city[ i ], sigma_obs_ride_dist / n_trips[ i ] );
  //   // sigma_obs_ride_dist);
  // 
  //   trip_frequency_only[ i ] ~ lognormal( log_ranef_rate_city[ i ], 0.01 );
  // }
  //   
  // for (i in 1:n_cities_dist_no_freq) {
  //   // log_ranef_rate_city[i] ~ normal( lmu_ranef_rate, sigma_ranef_rate_city );
  //   log_ranef_dist_city[i] ~ normal( lmu_ranef_dist, sigma_ranef_dist_city );
  //   
  //   trip_distance_only[ i ] ~ lognormal( log_ranef_dist_city[ i ], 0.01 );
  //   // sigma_obs_ride_dist);
  // 
  //   // trip_frequency_paired[ i ] ~ lognormal( log_ranef_rate_city[ i ], 0.01 );
  // }
  // 
  
  // for (i in 1:n_cities_freq_and_dist) {
  //   trip_distance_paired[ i ] ~ lognormal( log_ranef_ride_dist_city[ i ], sigma_obs_ride_dist / n_trips[ i ] );
  //   // sigma_obs_ride_dist);
  //   
  //   trip_frequency_paired[ i ] ~ lognormal( log_ranef_rate_city[ i ], 0.01 );
  // }
  // 
  // for (i in 1:n_cities_dist_no_freq) {
  //   int j = i + n_cities_freq_and_dist;
  //   
  //   trip_distance_only[ i ] ~ lognormal( log_ranef_ride_dist_city[ j ], sigma_obs_ride_dist / n_trips[ j ] );
  //   // sigma_obs_ride_dist);
  // }
  // 
  // 
  // for (i in 1:n_cities_freq_no_dist) {
  //   int j = i + n_cities_freq_and_dist + n_cities_dist_no_freq;
  //   
  //   trip_frequency_only[ i ] ~ lognormal( log_ranef_rate_city[ j ], 0.01 );
  // }

}
generated quantities {
  

// 
//   vector[n_cities_pred] pred_log_ranef_rate_city;
//   vector[n_cities_pred] pred_log_ranef_dist_city;
//   
//   // // calculate the log-likelihood of the mode-substitution data
//   // for ( i in 1:n_cities_pred ) {
//   //   // theta[1] = 0;
//   //   
//   //   for ( x in 2:K ){
//   //     // theta[x] = asc[x-1] + Bperson[ user_ID[i], x-1 ] + bdistance_log[ x-1 ] * log(ride_dist[ i ]);
//   //     theta[x] = asc[x-1] + Bperson[ user_ID[i], x-1 ] + bdistance_log[ x-1 ] * log(ride_dist[ i ]);
//   //   }
//   //   
//   //   log_lik[ n_bikeshare_users + n_rides + i ] = categorical_logit_lpmf( mode_sub_int[i] | theta );
//   // }
//   // 
//   
//   for (i in 1:n_cities_pred) {
//     
//     
//     // for (i in 1:n_cities_freq_no_dist ) {
//       pred_log_ranef_rate_city[i] = normal_rng( lmu_ranef_rate, sigma_ranef_rate_city );
//       // log_ranef_dist_city[i] ~ normal( lmu_ranef_dist, sigma_ranef_dist_city );
//       
//       // trip_distance_paired[ i ] ~ lognormal( log_ranef_ride_dist_city[ i ], sigma_obs_ride_dist / n_trips[ i ] );
//       // sigma_obs_ride_dist);
//   
//       // trip_frequency_only[ i ] ~ lognormal( log_ranef_rate_city[ i ], 0.01 );
//     // }
//       
//     // for (i in 1:n_cities_dist_no_freq) {
//       // log_ranef_rate_city[i] ~ normal( lmu_ranef_rate, sigma_ranef_rate_city );
//       pred_log_ranef_dist_city[i] = normal_rng( lmu_ranef_dist, sigma_ranef_dist_city );
//       
//       // trip_distance_only[ i ] ~ lognormal( log_ranef_dist_city[ i ], 0.01 );
//       // sigma_obs_ride_dist);
//   
//       // trip_frequency_paired[ i ] ~ lognormal( log_ranef_rate_city[ i ], 0.01 );
//     // }
//     
//     
//   }
  // 
  // // predictions
  // {
  //   vector[2] z;
  //   vector[2] mu;
  //   mu = [ lmu_ranef_rate, lmu_ranef_ride_dist ]';
    //   
      //   // predictions
    //   for ( i in 1:n_pred ) {
      //     
        //     // sample the predicted user's parameters for mean ride frequency and trip distance
  //     z = multi_normal_rng( mu, quad_form_diag(rho_ranef_user, sigma_ranef_user) );
  //     
  //     // sample an actual number of rides for this simulation of this user
  //     pred_rate[i] = exp( z[1] );
  //     pred_n_rides[ i ] = poisson_rng( pred_rate[i] );
  //     
  //     // 
  //     pred_log_ranef_ride_dist[i] = z[2];
  //     
  //     
  //     // simulate the rides for this user ( simulate pred_n_rides[i] )
  //     { 
  //       real pred_ride_dist;
  //       int pred_modesub;
  //       vector[K] pred_theta;
  //       vector[K-1] pred_Bperson;
  //       vector[K-1] z_pred;
  //       
  //       // These varaibles will be incremented as we run through the simulations
  //       pred_n_rides_carsub[i] = 0;
  //       pred_substituted_miles[i] = 0;
  //       
  //       // 
  //       for ( j in 1:(K-1) ) {
  //         z_pred[j] = normal_rng(0, 1);
  //       }
  //       pred_Bperson = (diag_pre_multiply(sigma_mode_sub, L_B) * z_pred) + Bperson_mean;
  //       
  //       for ( j in 1:pred_n_rides[i] ) {
  //         
  //         // simulate the distance of this ride
  //         pred_ride_dist = lognormal_rng( pred_log_ranef_ride_dist[ i ], sigma_obs_ride_dist );
  //         
  //         // calculate the mode-substitution model parameters, now that the user parameters and ride distance are sampled.
  //         pred_theta[1] = 0;
  //         for ( x in 2:K ) {
  //           pred_theta[x] = asc[x-1] + pred_Bperson[ x-1 ] + bdistance_log[x-1] * log(pred_ride_dist);
  //         }
  //         
  //         // simulate what mode this ride substitutes for
  //         pred_modesub = categorical_logit_rng( pred_theta );
  //         
  //         // The mode substitution for car_alone is 3, but stan's categorical predictions begin at one, not zero. So here, car substitution is indexed by 4.
      //         if (pred_modesub == 4) { 
        //           
          //           // we found a ride that substitutes for car travel, so increment the carsub and substituted_miles variables
        //           pred_n_rides_carsub[i] += 1;
        //           pred_substituted_miles[i] += pred_ride_dist;
        //         }
      //         
        //       }
    //     }
  //     
    //   }
// }
// 
  }
