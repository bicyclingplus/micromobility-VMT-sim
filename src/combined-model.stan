data{
  
  // Data for mode-substitution
  int<lower=1> N;           // number of rows
  int<lower=2> K;           // number of mode choices
  int<lower=2> S;           // number of respondents
  
  int mode_sub_int[N];               // Response variable (travel mode substitution):1=walk,2=ridehail,3=car alone,4=no trip,5=carpool,6= transit
  real distance_log[N];
  int ID[N];                    	 // ID labels for person
  
  // Data for VMT
  int n_hhold;
  int n_obs;
  
  int n_sampno[n_hhold]; // number of rows for each starting state
  real totdist[n_obs];
  int sampno[n_hhold];
  
  
  // Data for bike share ride frequency
  int n_bikeshare;
  int ride_freq[n_bikeshare];
  
  
  // Data for bikeshare trip_distance
  int n_trip_dist;
  real trip_dist[ n_trip_dist ];
  
}
parameters{
  
  // Parameters for mode substitution
  vector[K-1] asc;                   //alternative specific constants (base=bike)
  vector[K-1] bdistance_log;		 // parameter vector for log distance

  vector[S] Bperson_raw[K-1];        //unscaled person-level alternative specific constants
  vector<lower=0>[K-1] sigma;		 //person-level standard devs. for each mode choice
  
  
  // Parameters for VMT
  real logA;
  real logB;
  corr_matrix[2] Rho;
  vector<lower=0>[2] sigma_vmt_gamma;
  vector[n_hhold] logA_i;
  vector[n_hhold] logB_i;
  
  
  // Parameters for bikeshare usage
  real laa;
  real lbb;
  real<lower=0> rate;
  
  
  // Parameters for bikeshare trip distance
  real laa2;
  real lbb2;
  
}
transformed parameters{
  
  // scaled person-level alternative specific constants
  vector[S] Bperson[K-1];
  
  
  // Transformed parameters for VMT
  real A;
  real B;
  vector[n_hhold] a_i;
  vector[n_hhold] b_i;
  
  
  // Transformed parameters for bikeshare frequency
  real aa;
  real bb;
  
  
  // Transformed parameters for bikeshare trip distance
  real aa2;
  real bb2;
  
  
  // scaled person-level alternative specific constants
  for ( i in 1:(K-1)) {
    Bperson[i,] = sigma[i] * Bperson_raw[i,];  // scale them
  }
  
  
  // Transformed parameters for VMT
  A = exp( logA );
  B = exp( logB );
  a_i = exp( logA_i );
  b_i = exp( logB_i );
  
  
  // Transformed parameters for bikeshare frequency
  aa = exp( laa );
  bb = exp( lbb );
    
  
  // Transformed parameters for bikeshare trip distance
  aa2 = exp(laa2);
  bb2 = exp(lbb2);
  
}
model{
  //priors
  asc ~ normal( 0 , 1.5 );
  bdistance_log ~ normal( 0 , 1.5 );
  sigma ~ student_t(6,0,1.5);

  for ( j in 1:(K-1)){
    Bperson_raw[j,] ~ normal(0,1);
  }
  
  // Likelihood for mode substitution
  for ( i in 1:N ) {
    vector[K] theta;

    theta[1] = 0;			// base mode = bike, so set to 0.
    for ( x in 2:K ){
      theta[x] = asc[x-1] + Bperson[x-1,ID[i]] + bdistance_log[x-1]*distance_log[i];
    }
    mode_sub_int[i] ~ categorical_logit(theta);
  }
  
  
  
  // Likelihood for bikeshare use
  laa ~ normal( 0, 1 );
  lbb ~ normal( 0, 1 );
  rate ~ gamma( aa, bb );
  ride_freq ~ poisson( rate );
  
  
  // Likelihood for bikeshare trip distance
  laa2 ~ normal( 0, 1 );
  lbb2 ~ normal( 0, 1 );
  trip_dist ~ gamma( aa2, bb2 );
  
  
  
  // Likelihood for VMT
  sigma_vmt_gamma ~ exponential( 1 );
  Rho ~ lkj_corr( 2 );

  logA ~ normal( 0, 1 );
  logB ~ normal( 0, 1 );

  {
    vector[2] MU;
    vector[2] GAMMA_PARS[n_hhold];
    
    MU = [ logA, logB ]';
    for ( i in 1:n_hhold ) GAMMA_PARS[i] = [ logA_i[i], logB_i[i] ]';
    GAMMA_PARS ~ multi_normal( MU, quad_form_diag( Rho, sigma_vmt_gamma ) );
  }
  
  {
    int m;
    m = 1;
    
    for ( j in 1:n_hhold ) {
    
      for ( k in 1:n_sampno[j] ) {
        totdist[m] ~ gamma( a_i[j], b_i[j] );
        m = m+1;
      }
    }
  }
  
}
generated quantities {
  vector[N] log_lik;		// pointwise log density for predictive evaluation
  for ( i in 1:N ) {
    vector[K] theta;
    theta[1] = 0;
    for ( x in 2:K ){
      theta[x] = asc[x-1] + Bperson[x-1,ID[i]] + bdistance_log[x-1]*distance_log[i];
    }
    log_lik[i] = categorical_logit_lpmf(mode_sub_int[i]|theta);
  }
}

