# simulations for power calculations
require( "lme4" )

Model <- function( x = micromob ) {
  mod <- lmer( vehicle_miles ~ (1 | city) + (1 | subject), data = x, na.action = na.omit )
}


GetHyperparam<-function( x, b = NULL ) {
  ## Get the hyperparameters from the mixed effect model
  fe <- fixef(x)
  
  if ( is.null(b) )
    b <- fe[[ 2 ]] # use the data effect size if not supplied
  
  mu.a <- fe[[ 1 ]]
  vc <- VarCorr(x)
  sigma.y <- as.numeric( vc[5, 2] ) # Residual StdDev
  sigma.a <- as.numeric( vc[2, 2] ) # city StdDev
  sigma.g <- as.numeric( vc[4, 2] ) # city:subject StdDev
  hp <- c(b, mu.a, sigma.y, sigma.a, sigma.g)
  names(hp) <- c("b", "mu.a", "sigma.y", "sigma.a", "sigma.g")
  
  return( hp )
}



fakeModWithRestarts <- function( m.o, n = 100, ... ) { ## A Fake Model
  withCallingHandlers({
    i <- 0
    mod <- NULL
    while (i < n & is.null(mod)) {
      mod <- withRestarts({
        f <- fake(m.orig = m.o, transform = FALSE, ...)
        return(update(m.o, data = f))
      },
      rs = function(){
        i <<- i + 1
        return(NULL)
      })
    }
    if ( is.null(mod) )
      warning("ExceededIterations")
    return(mod)
  },
  error = function(e) {
    invokeRestart("rs")
  },
  warning = function(w) {
    if (w$message == "ExceededIterations")
      cat("\n", w$message, "\n")
    else
      invokeRestart("rs")
  })
}


## Calculate power for a particular sampling design 
dt.power <- function ( m, n.sims = 1000, alpha = 0.05, ... ) { 
  signif <- rep( NA, n.sims )
  
  for (i in 1:n.sims) {
    lme.power <- fakeModWithRestarts(m.o = m, ...)
    if(!is.null(lme.power))
      signif[[ i ]] <- summary(lme.power)$tTable[2, 5] < alpha
  }
  
  power <- mean(signif, na.rm = TRUE)
  return( power )
}


factoredDesign <- function(Elevs = seq(-0.5, 0.5, length.out = 21), Nlevs = seq(1, 4, by = 1), Jlevs = seq(4, 10, by = 2),
                           Klevs = c(10, 20, 50, 100, 200), ...) {
  ## Generates factored series of sampling designs for simulation ## of data that follow a particular model.
  ## Inputs:
  ## Elevs
  ## Nlevs
  ## Jlevs
  ## Klevs
  ## Results:
  ## Data frame with where columns are the factors and
  ## rows are the designs.
  # vector of effect sizes for the slope parameter.
  # vector of number of years to sample.
  # vector of number of cobblebars to sample.
  # vector of number of transects to sample.
  
  # Level lengths
  lE <- length( Elevs )
  lN <- length( Nlevs )
  lJ <- length( Jlevs )
  lK <- length( Klevs )
  
  # Generate repeated vectors for each factor
  n_effects <- rep( Elevs, each = lN * lJ * lK)
  n_waves <- rep( rep(Nlevs, each = lJ * lK), times = lE)
  n_cities <- rep( rep(Jlevs, each = lK), times = lE * lN)
  n_subj_each_city <- rep( Klevs, times = lE * lN * lJ)
  
  return( data.frame(n_effects, n_waves, n_cities, n_subj_each_city) )
}



fake <- function(n_waves = 2, n_cities = 6, n_subj_each_city = 5, b = NULL, m.orig = mod, transform = TRUE, ...) {
  ## Simulated Data for power analysis
  ## N = Number of waves
  ## J = Number of cities
  ## K = Number of subjects within cities
  wave <- rep(0:(n_waves - 1), each = n_cities * n_subj_each_city)
  city <- factor( rep( rep(1:n_cities, each = n_subj_each_city), times = n_waves))
  subject <- factor( rep(1:n_subj_each_city, times = n_waves * n_cities) )
  
  ## Simulated parameters
  hp <- GetHyperparam( x = m.orig )
  
  if(is.null(b))
    b <- hp[[ "b" ]]
  
  g <- rnorm(n_cities * n_subj_each_city, mean = 0, sd = hp[[ 'sigma.g' ]])
  a <- rnorm(n_cities * n_subj_each_city, mean = hp[[ 'mu.a' ]] + g, sd = hp[[ 'sigma.a' ]])
  
  ## Simulated responses
  eta <- rnorm(n_cities * n_subj_each_city * nwaves, mean = a + b * n_bike_trips, sd = hp[[ 'sigma.y' ]])
  if (transform) {
    if (m.orig$type == "normal") {
      y <- eta
      y[y > 1] <- 1 # Fix any boundary problems.
      y[y < 0] <- 0
    }
    else if (m.orig$type == "log") {
      y <- exp(eta)
      y[y > 1] <- 1
    }
    else if (m.orig$type == "logit")
      y <- exp(eta) / (1 + exp(eta))
  } else {
    y <- eta 
  }
  
  return( data.frame(vmt = y, wave, subject, city) )
}



powerAnalysis <- function( parallel = FALSE, ...) { ## Full Power Analysis
  ## Parallel
  if (parallel) {
    closeAllConnections()
    cl <- makeCluster(7, type = "SOCK")
    on.exit(closeAllConnections())
    clusterEvalQ(cl, source("cobblebars2.r"))
  }
  
  ## The simulations
  dat <- factoredDesign(...)
  if (parallel) {
    dat$power <- parRapply(cl, dat, function(x, ...) {
      dt.power( N = x[[ 2 ]], J = x[[ 3 ]], K = x[[ 4 ]], b = x[[ 1 ]], ... )
      }, ... )
  } else {
    dat$power <- apply(dat, 1, function(x, ...) {
      dt.power( N = x[[ 2 ]], J = x[[ 3 ]], K = x[[ 4 ]], b = x[[ 1 ]], ... )
    }, ... )
  }
  
  return( dat )
}




des <- factoredDesign()

design <- with( des[1, ], 
      expand.grid(wave = factor( letters[1:n_waves] ), 
                  city = factor( letters[1:n_cities] ), 
                  subj = factor( letters[1:n_subj_each_city] ) ))



sim_design <- function(n_waves, n_cities, n_subj_each_city, effect, sigma_wave, sigma_city, sigma_subject, sigma_obs) {
  
  # create the list of subjects
  wave_design <- data.frame( city = rep(factor( letters[1:n_cities] ), each = n_subj_each_city) )
  wave_design[[ "subject" ]] <- factor( paste0(wave_design[[ "city" ]], rep(1:n_subj_each_city, times = n_cities) ) )
  
  # create the overal design matrix
  design <- data.frame()
  
  # populate the design wave by wave
  wave <- 1
  while (wave <= n_waves) {
    wave_design[[ "wave" ]] <- paste0( wave_design[[ 'city' ]], wave)
    design <- rbind(design, wave_design)
    
    wave <- wave + 1
  }
  
  # wave ought to be a factor
  design[[ 'wave' ]] <- as.factor( design[[ 'wave' ]])

  # now simulate the data
  city_ranef <- rnorm( n_cities, 0, sigma_city)
  wave_ranef <- rnorm( length( levels( design[[ 'wave' ]])), 0, sigma_wave) # the random effect of wave is constant variance across all cities and waves, independent from city to city.
  subj_ranef <- rnorm( length( levels( design[[ 'subject' ]])), 0, sigma_subject)
  epsilon <- rnorm( nrow(design), 0, sigma_obs)
  
  res <- design
  res[[ 'obs' ]] <- city_ranef + wave_ranef + subj_ranef + epsilon
  
  res
}