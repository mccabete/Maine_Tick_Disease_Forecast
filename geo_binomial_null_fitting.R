print("loading packages")
library(rjags, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(lazyeval, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(ggplot2, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(coda, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(dplyr, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")
library(rlang, lib.loc = "/share/pkg.7/r/3.6.0/install/lib64/R/library")

print("Reading in data")
tick_disease_big <- read.csv("/usr3/graduate/tmccabe/Maine_Tick_Disease_Forecast/town_level_data_lyme.csv", header = TRUE, stringsAsFactors = FALSE)
sebago <- read.csv("/usr3/graduate/tmccabe/Maine_Tick_Disease_Forecast/data_raw/Sebago_correction.csv", header = FALSE, stringsAsFactors = FALSE)
names(sebago) <- names(tick_disease_big)
tick_disease_big <- rbind(tick_disease_big, sebago)

tick_disease_big$Population <- gsub(pattern = ",", replacement = "", tick_disease_big$Population)

towns_in_cumberland <- c("Harpswell", "Freeport", "Brunswick", "Pownal", "Yarmouth", 
                         "Cumberland", "Standish", "Chebeague_Island", "Portland", "Falmouth", "South_Portland", 
                         "Cape_Elizabeth", "Long_Island", "Scarborough", "Harrison", "Gray", "North_Yarmouth", 
                         "Baldwin", "Bridgton", "Casco", "Raymond", "Naples", "Sebago", "Windham", "Gorham",
                         "Westbrook", "New_Gloucester") # Deleted "frye_island because NR
#unique(tick_disease_big$Number)

##towns_for_validation <- c("Bridgton","Sebago",  "Westbrook")

towns_to_run <- towns_in_cumberland #[!(towns_in_cumberland %in% towns_for_validation)]

## Correct names for spacing
print("correcting names in tick_disease")

tmp <- gsub(" ", "_", tick_disease_big$Location)
tmp <- as.data.frame(tmp)
tick_disease_big$Location <- tmp
tick_disease <- tick_disease_big[as.matrix(tick_disease_big$Location) %in% towns_in_cumberland, ]





## Get beta params
print("getting parameters for beta")
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

mu_true_pos <- 0.12
observed_pos_1 <- 0.0579 # Two Tier
observed_pos_2 <- 0.1189 # EIA/ IFA Stand-alone or First Tier
proportion_used_1 <- 297619/ (297619 + 287595)  # Reported number of tests for two-tierd / total 
proportion_used_2 <- 287595/ (297619 + 287595) # Reported number of tests for two-tierd / total 

weighted_obs_pos <- (0.0579 *proportion_used_1) + (0.1189*proportion_used_2)

detection_rate_mu <- weighted_obs_pos / mu_true_pos
detection_rate_min <-  weighted_obs_pos / 0.185
detection_rate_max <- weighted_obs_pos / 0.1

var_true_pos <- 0.11 # Something larger than min and max, but still constrained by them. 

beta_of_true_tick_rate <- estBetaParams(detection_rate_mu, var_true_pos)



### Function
print("loading model function")
fit_binomial_null <- function(tick_data, has_suppressed = TRUE, town_name, n.iter = 91000, data_list, init_detection_rate, init_data){
  
  tick_data <- dplyr::arrange(tick_data, by = tick_data$Year) # Make sure to be going in order of year
  
  
  less_than_six_indices <- which(tick_data$Number == "<6" )
  numeric_indices <- which(tick_data$Number != "<6")
  
  data_list$n <- length(tick_data$Year) 
  data_list$numeric_indices <- numeric_indices
  data_list$less_than_six_indices <- less_than_six_indices
  data_list$y <- tick_data$Number
  
  ## Check if has suppressed data. If yes, provide flow control
  if (rlang::is_empty(less_than_six_indices)){
    has_suppressed <- FALSE
  }
  
  if(has_suppressed){
    RandomWalk = "
    model{
    
    #### Data Model
    for(t in numeric_indices){
    
    y[t] ~ dbinom(detection_rate, x[t])
    }
    
    for(t in less_than_six_indices){   ## Maybe consider a population cutoff, and allow zeros
    y[t] ~ dbinom(detection_rate, x[t]) T(1,5)
    }
    
    #### Process Model
    for (i in 1:n){
    x[i] ~ dbinom(probability, pop[i])
    }
    
    ## Prior
    probability ~ dbeta(1,1) #uniform probability
    
    #### Priors
    #x[1] ~ dpois(x_ic) 
    detection_rate ~ dbeta(a_obs,b_obs) 
    
    }
    "
  }else{
    RandomWalk = "
    model{
    
    #### Data Model
    for(t in numeric_indices){
    y[t] ~ dbinom(detection_rate, x[t])
    }
    
    #### Process Model
    for (i in 1:n){
    x[i] ~ dbinom(probability, pop[i])
    }
    
    
    #### Priors
    #x[1] ~ dpois(x_ic)
    detection_rate ~ dbeta(a_obs, b_obs)
    probability ~ dbeta(1,1) #uniform probability
    }
    "
}
  
  ## loading in priors
  #data <- data_list
  
  ## setting  initial conditions
  nchain = 3
  init <- list()
  for(i in 1:nchain){
    init[[i]] <- list(detection_rate = init_detection_rate , x = init_data) 
  }
  
  
  j.model   <- jags.model (file = textConnection(RandomWalk),
                           data = data_list,
                           inits = init,
                           n.chains = 3)
  
  jags.out   <- coda.samples (model = j.model,
                              variable.names = c( "x", "detection_rate", "probability"),
                              n.iter = n.iter)
  
  GBR <- gelman.plot(jags.out)
  burnin <- GBR$last.iter[tail(which(apply(GBR$shrink[,,2]>1.05,1,any)),1)+1] ## Cut our burn-in. 
  
  if(is.na(burnin)){
    warning("GBR !< 1.05. Model may have failed to converge")
    jags.burn <- jags.out
    did_it_converge <- "convergence_failed_GBR_test" 
    
  }else{
    did_it_converge <- "convergence_passed_GBR_test"
    jags.burn <- window(jags.out,start=burnin, extend = FALSE)
  }
  date_stamp <- Sys.time()
  date_stamp <- format(date_stamp, "%Y%m%d")
  file_name <- paste("/Users/tess/Documents/work/Maine_Tick_Disease_Forecast/Jags_output/", date_stamp, ".", "binomial_null",".",town_name,".", did_it_converge, ".","JAGS_run.Rdata", sep = "")
  print(did_it_converge)
  print(effectiveSize(jags.burn))
  save(jags.burn, file = file_name )
  #return(jags.burn)
  }


### For loop
print("Looping over towns")
#towns_to_run <- c( "Bridgton" , "Sebago"   , "Westbrook", "Harrison", "Baldwin", "Chebeague_Island", "Long_Island","Pownal") # no Frye_Island because NA in data
need_mid_runs <- c( "Bridgton" , "Sebago"   , "Westbrook", "Harrison", "Baldwin") 
need_long_runs <- c("Chebeague_Island", "Long_Island", "Pownal")
for (i in seq_along(towns_to_run)){
  print("starting loop")
  n.iter = 101000
  
  if(towns_to_run[i] %in% need_mid_runs){
    n.iter = 951000
  }
  if(towns_to_run[i] %in% need_long_runs){
    n.iter = 10000000
  }
    
  town_name <- towns_to_run[i]
  tick_data <- tick_disease
  tick_disease_tmp <- tick_data[(tick_data$Location == town_name ),] 
  tick_disease_tmp <- dplyr::filter(tick_disease_tmp, tick_disease_tmp$Year >= 2008) 
  tick_disease_tmp$Number[tick_disease_tmp$Year > 2016] <- NA # Hold out three years for validation. 
  tick_disease_tmp <- dplyr::arrange(tick_disease_tmp, by = tick_disease_tmp$Year) # Make sure model is fitting through time
  
  
  
  tick_data <- tick_disease_tmp
  less_than_six_indices <- which(tick_data$Number == "<6" )
  numeric_indices <- which(tick_data$Number != "<6")
  
  init_data <- tick_data$Number
  if(NA %in% init_data){
    init_data[is.na(init_data)] <- init_data[9] # expected value is latest non-NA value
  }
  if("<6" %in% init_data ){
    init_data[init_data == "<6"] <- rep(3, length(init_data[init_data == "<6"]))
  }
  init_data <- as.numeric(init_data) 
  
  data_list <- list(less_than_six_indices = less_than_six_indices, numeric_indices =numeric_indices,  a_obs = beta_of_true_tick_rate$alpha, b_obs = beta_of_true_tick_rate$beta , 
                    y = tick_disease_tmp$Number, pop = as.numeric(tick_disease_tmp$Population))
  print(town_name)
  fit_binomial_null(tick_data, has_suppressed = TRUE, town_name, n.iter = n.iter, data_list, init_detection_rate = 0.9, init_data = init_data )
  print(paste(town_name, "Finished"))
}







