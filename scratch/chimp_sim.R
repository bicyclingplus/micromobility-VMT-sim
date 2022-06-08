library(rethinking)
data(chimpanzees)

d <- chimpanzees
d$block_id <- d$block
d$treatment <- 1L + d$prosoc_left + 2L*d$condition

dat <- list(
  L = d$pulled_left,
  tid = d$treatment,
  actor = d$actor,
  block_id = as.integer(d$block_id) )

set.seed(4387510)
m14.2 <- ulam(
  alist(
    L ~ dbinom(1,p),
    logit(p) <- g[tid] + alpha[actor,tid] + beta[block_id,tid],
    
    # adaptive priors
    vector[4]:alpha[actor] ~ multi_normal(0, Rho_actor, sigma_actor),
    vector[4]:beta[block_id] ~ multi_normal(0, Rho_block, sigma_block),

    # fixed priors
    g[tid] ~ dnorm(0,1),
    sigma_actor ~ dexp(1),
    Rho_actor ~ dlkjcorr(4),
    sigma_block ~ dexp(1),
    Rho_block ~ dlkjcorr(4)
  ),
  data=dat , chains=4 , cores=4
  
)



set.seed(4387510)
m14.3 <- ulam(
  alist(
    L ~ binomial(1,p),
    logit(p) <- g[tid] + alpha[actor,tid] + beta[block_id,tid],
    
    # adaptive priors - non-centered
    transpars> matrix[actor,4]:alpha <-
      compose_noncentered( sigma_actor , L_Rho_actor , z_actor ),
    
    transpars> matrix[block_id,4]:beta <-
      compose_noncentered( sigma_block , L_Rho_block , z_block ),
    
    matrix[4,actor]:z_actor ~ normal( 0 , 1 ),
    matrix[4,block_id]:z_block ~ normal( 0 , 1 ),
    
    # fixed priors
    g[tid] ~ normal(0,1),
    vector[4]:sigma_actor ~ dexp(1),
    
    cholesky_factor_corr[4]:L_Rho_actor ~ lkj_corr_cholesky( 2 ),
    
    vector[4]:sigma_block ~ dexp(1),
    
    cholesky_factor_corr[4]:L_Rho_block ~ lkj_corr_cholesky( 2 ),
    
    # compute ordinary correlation matrixes from Cholesky factors
    gq> matrix[4,4]:Rho_actor <<- Chol_to_Corr(L_Rho_actor),
    gq> matrix[4,4]:Rho_block <<- Chol_to_Corr(L_Rho_block)
  ),
  data=dat , chains=4 , cores=4 , log_lik=TRUE
)