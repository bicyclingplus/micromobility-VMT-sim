require("tidyverse")
require("rethinking")

trips <- read_csv("~/Downloads/psrc_06_full_survey/v_gpstrips.csv")
trips$date <- lubridate::as_date(trips$bktp_start_date)
trips$year <- lubridate::year(trips$date)
trips$week <- lubridate::week(trips$date)
trips$weekday <- weekdays(trips$date)

# households that begin with zero may also appear with the leading zeroes stripped.
# these ought to be combined.
trips$sampno <- gsub("^0+([^0][[:digit:]]*)$", "\\1", trips$sampno) # ifelse( substr(trips$sampno, 1, 1) == '0', , trips$sampno)

t2 <- trips %>% group_by(sampno, year, week) %>% summarize(totdist = sum(bktp_mt_total))

t3 <- t2
t3$sampno <- as.integer(t3$sampno)
t3 <- t3[ !is.na(t3$sampno), ]


# what is the variability in cross-sectional data?



metadata <- t3 %>% group_by(sampno) %>% summarize(n_sampno = n())

d_list <- list()
d_list[[ "sampno" ]] <- metadata[[ "sampno" ]]
d_list[[ "n_sampno" ]] <- metadata[[ "n_sampno" ]]
d_list[[ "totdist" ]] <- t3[[ "totdist" ]]

model <- ulam(
  alist(
    totdist ~ gamma( a_i, b_i ),
    c(a_i, b_i)[sampno] ~ multi_normal( c(A, B), rho, sigma_i ),
    
    tranpars> vector[1]:A <- exp( logA ),
    tranpars> vector[1]:B <- exp( logB ),
    
    logA ~ normal(0, 1),
    logB ~ normal(0, 1),
    

    
    sigma_i ~ exponential( 1 ),
    rho ~ lkj_corr( 2 )
    
    
    
  ),
  data = t2, chains = 4, cores = 4
)
