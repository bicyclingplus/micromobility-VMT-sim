# model the probability that a ride substitutes for car travel


sub_car_alone <- c( 0.1882,0.1433, 0.19, NA, 0.108, 0.13, 0.14, NA, 0.05, NA, 0.194, 0.24, NA, NA, 0.094, NA, 0.05, NA, 0.11,0.04, 0.04, 0.1337, 0.34, NA )
survey_sample_size <- c( 3444, 1088, NA, 4260, 12446, 1066, 292, 2084, 2256, 2640, 680, 2704, 1391, 124, 617, 3424, 5832, 44,7067, 883, 2814, 173, 38, 982 )
sub_data <- data.frame(sub_car_alone = as.integer(sub_car_alone * survey_sample_size), survey_sample_size = survey_sample_size)
sub_data = sub_data[ complete.cases(sub_data), ]


d_sub_list <- list()
d_sub_list[[ "sub_car_alone" ]] <- sub_data[[ "sub_car_alone" ]]
d_sub_list[[ "survey_sample_size" ]] <- sub_data[[ "survey_sample_size" ]]
d_sub_list[[ 'miles' ]] <- jump_cleaned[[ "distance" ]]

substitution_model_code <- " data{
      int sub_car_alone[15];
      int survey_sample_size[15];
      real miles[846];
    }
    parameters{
      real alpha;
      real sigma;
      
      real a;
      real b;
    }
    model{
      
      sigma ~ exponential( 1 );
      alpha ~ student_t( 1, 0, sigma );
      sub_car_alone ~ binomial_logit( survey_sample_size, alpha );
      
      a ~ normal(0, 1);
      b ~ normal(0, 1);
      miles ~ gamma( a, b );
    } "
substitution_model <- stan_model(model_code = substitution_model_code)

fit3 <- sampling(substitution_model, d_sub_list, cores=1)
draws3 <- extract.samples( fit3 )
