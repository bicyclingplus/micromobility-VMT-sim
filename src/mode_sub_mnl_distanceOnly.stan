data{
  int<lower=1> N;           // number of rows
  int<lower=2> K;           // number of mode choices
  int<lower=2> S;           // number of respondents
  
  int mode_sub_int[N];               // Response variable (travel mode substitution):1=walk,2=ridehail,3=car alone,4=no trip,5=carpool,6= transit
  real distance_log[N];
  int ID[N];                    	 // ID labels for person
}
parameters{
  vector[K-1] asc;                   //alternative specific constants (base=bike)
  vector[K-1] bdistance_log;		 // parameter vector for log distance

  vector[S] Bperson_raw[K-1];        //unscaled person-level alternative specific constants
  vector<lower=0>[K-1] sigma;		 //person-level standard devs. for each mode choice
}
transformed parameters{
  vector[S] Bperson[K-1]; 			 // scaled person-level alternative specific constants
  
  for ( i in 1:(K-1)){
    Bperson[i,] = sigma[i] * Bperson_raw[i,];  // scale them
  }
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
