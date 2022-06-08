# This is a simulation experiment extracted from Richard McElreath's "Rethinking Statistics", Chapter 14.

## Set model parameters
a <- 3.5 # average morning wait time
b <- (-1) # average difference afternoon wait time
sigma_a <- 1 # std dev in intercepts
sigma_b <- 0.5 # std dev in slopes
rho <- (-0.7) # correlation between intercepts and slopes

Mu <- c(a, b)

# Set up the covariance matrix
cov_ab <- sigma_a * sigma_b * rho
Sigma <- matrix( c(sigma_a^2, cov_ab, cov_ab, sigma_b^2) , ncol=2 )

# alternate representation of the covariance matrix
sigmas <- c(sigma_a, sigma_b) # standard deviations
Rho <- matrix( c(1, rho, rho, 1) , nrow=2 ) # correlation matrix

# now matrix multiply to get covariance matrix
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

library(MASS)

# Simulate hidden variables
set.seed(5) # used to replicate example 
N_cafes <- 20
vary_effects <- mvrnorm( N_cafes , Mu , Sigma )

a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

plot( a_cafe , b_cafe , col=rangi2 ,
      xlab="intercepts (a_cafe)" , ylab="slopes (b_cafe)" )

# overlay population distribution
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines( ellipse(Sigma, centre=Mu, level=l), col=col.alpha("black", 0.2) )

# simulate data
set.seed(22)
N_visits <- 10
afternoon <- rep(0:1, N_visits * N_cafes / 2)
cafe_id <- rep( 1:N_cafes , each=N_visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id] * afternoon
sigma <- 0.5 # std dev within cafes
wait <- rnorm( N_visits * N_cafes , mu , sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )


# estimate model parameters
R <- rlkjcorr( 1e4 , K=2 , eta=2 )
dens( R[,1,2] , xlab="correlation" )


# create the Bayesian model
set.seed(867530)
m14.1 <- ulam(
  alist(
    wait ~ normal( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe, b_cafe)[cafe] ~ multi_normal( c(a,b) , Rho , sigma_cafe ),
    a ~ normal(5,2),
    b ~ normal(-1,0.5),
    sigma_cafe ~ exponential(1),
    sigma ~ exponential(1),
    Rho ~ lkj_corr(2)
  ), data=d , chains=4,)


# Extract results
post <- extract.samples(m14.1)
dens( post$Rho[,1,2] , xlim=c(-1,1) ) # posterior
R <- rlkjcorr( 1e4 , K=2 , eta=2 ) # prior
dens( R[,1,2] , add=TRUE , lty=2 )


# compute unpooled estimates directly from data
a1 <- sapply( 1:N_cafes, function(i) mean(wait[cafe_id==i & afternoon==0]) )
b1 <- sapply( 1:N_cafes, function(i) mean(wait[cafe_id==i & afternoon==1]) ) - a1

# extract posterior means of partially pooled estimates
post <- extract.samples(m14.1)
a2 <- apply( post$a_cafe , 2 , mean )
b2 <- apply( post$b_cafe , 2 , mean )

# plot both and connect with lines
plot( a1 , b1 , xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2 , ylim=c( min(b1)-0.1 , max(b1)+0.1 ) ,
      xlim=c( min(a1)-0.1 , max(a1)+0.1 ) )
points( a2 , b2 , pch=1 )
for ( i in 1:N_cafes ) lines( c(a1[i],a2[i]) , c(b1[i],b2[i]) )

# compute posterior mean bivariate Gaussian 
Mu_est <- c( mean(post$a) , mean(post$b) )
rho_est <- mean( post$Rho[, 1, 2] )
sa_est <- mean( post$sigma_cafe[, 1] )
sb_est <- mean( post$sigma_cafe[, 2] )
cov_ab <- sa_est * sb_est * rho_est
Sigma_est <- matrix( c(sa_est^2, cov_ab, cov_ab, sb_est^2), ncol=2 )

# draw contours
library(ellipse)
for ( l in c(0.1,0.3,0.5,0.8,0.99) )
  lines(ellipse(Sigma_est, centre=Mu_est, level=l), col=col.alpha("black",0.2))
