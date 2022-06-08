# once we have a model for trips and for substitution, we can combine them for a model of miles travelled

n_waves <- 3

# generate car trips and mileage ------------------------------------------
# generate car trips and mileage
i <- sample(1:4000, 1)

S <- matrix( c( draws[[ 'sigma' ]][i,1]^2, prod(draws[[ 'sigma' ]][i,]) * draws[[ "Rho" ]][i],
             prod(draws[[ 'sigma' ]][i,]) * draws[[ "Rho" ]][i], draws[[ 'sigma' ]][i,2]^2 ), 2, 2 )

fam_gam <- exp( rmvnorm( 1, mean = c( draws[[ "A" ]][i], draws[[ "B" ]][i] ), sigma = S) )

weekly_vmt <- rgamma(n_waves, fam_gam[[1]], fam_gam[[2]])


# generate bike trips and mileage -----------------------------------------
# generate bike trips and mileage

# divide by four because the modeled rates are for 28 days
rate <- rgamma(n_waves, draws[[ "aa" ]][i], draws[[ "bb" ]][i]) / 4

n_bike_trips <- rpois( n_waves, rate )




# determine how many bike miles substitute car miles ----------------------
# determine how many bike miles substitute car miles






# 