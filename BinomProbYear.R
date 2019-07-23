


town <- subset(tick_disease, Location == towns_in_cumberland[i])
town <- town[order(town$Year),,drop = FALSE]
town <- subset(town, Year >= 2008 & Year <= 2015)
town2model <- town$Location[1]

if(town2model == "Pownal"){
  town <- town[c(1,3,4,6,8,10,11,12),]
}

data.jags <- list()
# data.jags$z <- town$Rate
data.jags$y <- town$Number
data.jags$pop <- town$Population
# data.jags$x_ic <- 1 # town_x_ic$x_ic[town_x_ic$Location == town_name]
data.jags$a_obs <- 0.5 # beta_of_true_tick_rate$alpha
data.jags$b_obs <- 0.1 #beta_of_true_tick_rate$beta
data.jags$n <- length(town$Year) #+ 1
data.jags$numeric_indices <- which(town$Number != "<6" )
data.jags$less_than_six_indices <- which(town$Number == "<6" )
# data.jags$yearly_error <- mean_year_year_variability
data.jags$b0 <- as.vector(c(-5,0))      ## regression beta means
data.jags$Vb <- solve(diag(25,2))   ## regression beta precisions
data.jags$terra <- as.vector(aggrigate_data("Terra_LST", town2model)$year.mean)
data.jags$aqua <- as.vector(aggrigate_data("Aqua_LST", town2model)$year.mean)

if(length(data.jags$less_than_six_indices) > 0){
  data.jags$y[data.jags$less_than_six_indices] <- NA
  model = " model{
  
  for(t in numeric_indices){
    #### Data Model
    y[t] ~ dbinom(detection_rate, x[t])
    
    #### Process Model - probability a function of yearly temperature
    logit(probability[t]) <- beta[1] + beta[2]*lst[t]
    x[t] ~ dbinom(probability[t], pop[t])
  }
 
  for(t in less_than_six_indices){   
    #### Data Model
    y[t] ~ dbinom(detection_rate, x[t])
    
    #### Process Model - probability a function of yearly temperature
    logit(probability[t]) <- beta[1] + beta[2]*lst[t]
    x[t] ~ dbinom(probability[t], pop[t])
  }

  for(t in 1:n){
    #### EIV
    terra[t] ~ dnorm(lst[t], tau_terra)
    aqua[t] ~ dnorm(lst[t], tau_aqua)
    lst[t] ~ dunif(-2.003567, 1.440454)
  }
  
  #### Priors
  detection_rate ~ dbeta(a_obs,b_obs) 
  beta ~ dmnorm(b0, Vb)
  tau_terra ~ dgamma(0.1, 0.1)
  tau_aqua ~ dgamma(0.1, 0.1)
}"
  
  
} else {
  model = " model {
  
  for(t in numeric_indices){

    #### Data Model
    y[t] ~ dbinom(detection_rate, x[t])
    
    #### Process Model - probability a function of yearly temperature
    logit(probability[t]) <- beta[1] + beta[2]*lst[t]
    x[t] ~ dbinom(probability[t], pop[t])

    #### EIV
    terra[t] ~ dnorm(lst[t], tau_terra)
    aqua[t] ~ dnorm(lst[t], tau_aqua)
    lst[t] ~ dunif(-2.003567, 1.440454)
  }

  #### Priors
  detection_rate ~ dbeta(a_obs, b_obs)
  beta ~ dmnorm(b0, Vb)
  tau_terra ~ dgamma(0.1, 0.1)
  tau_aqua ~ dgamma(0.1, 0.1)
}"
}

n.iter <- 200000
iter2save <- 5000
thin <- round(n.iter / iter2save)
x.init <- round(as.numeric(data.jags$y)*runif(1, 1, 3))
variable.names <- c( "x", "detection_rate", "beta", "lst", "tau_terra", "tau_aqua")

inits <- function(){list(x = x.init,
                         detection_rate = runif(1, 0.87, 1),
                         beta = rnorm(2, c(-7, 0), 0.01))}

load.module("glm")
j.model   <- jags.model (file = textConnection(model),
                         data = data.jags,
                         inits = inits,
                         n.chains = 3)

jags.out   <- coda.samples (model = j.model,
                            variable.names = variable.names,
                            burnin = 50000,
                            thin = thin,
                            n.iter = n.iter)

# jags.out <- window(jags.out, 3500)

## split output
out <- list(params = NULL, predict = NULL, lst = NULL)
mfit <- as.matrix(jags.out, chains = TRUE)
pred.cols <- grep("x[", colnames(mfit), fixed = TRUE)
lst.cols <- grep("lst[", colnames(mfit), fixed = TRUE)
chain.col <- which(colnames(mfit) == "CHAIN")
out$predict <- ecoforecastR::mat2mcmc.list(mfit[, c(chain.col, pred.cols)])
out$lst <- ecoforecastR::mat2mcmc.list(mfit[, c(chain.col, lst.cols)])
out$params <- ecoforecastR::mat2mcmc.list(mfit[, -c(lst.cols, pred.cols)])

plot(out$params)

state <- as.matrix(out$predict)
state.ci <- apply(state, 2, quantile, c(0.025, 0.5, 0.975))

par(mfrow = c(1, 1))
plot(1:ncol(state.ci), state.ci[2,], ylim = range(state.ci))
ciEnvelope(1:ncol(state.ci), state.ci[1,], state.ci[3,], col = "lightblue")
lines(1:ncol(state.ci), state.ci[2,])


params <- as.matrix(out$params)
lst <- as.matrix(out$lst)
cases <- prob <- matrix(NA, nrow(town), nrow(params))
for(i in 1:nrow(town)){
  prob[i,] <- inv.logit(params[,"beta[1]"] + params[,"beta[2]"]*lst[,i])
  cases[i,] <- rbinom(nrow(params), as.numeric(data.jags$pop[i]), prob[i,])
}

cases.ci <- apply(prob, 1, quantile, c(0.025, 0.5, 0.975))
par(mfrow = c(1, 1))
plot(1:ncol(cases.ci), cases.ci[2,], ylim = range(cases.ci))
ciEnvelope(1:ncol(cases.ci), cases.ci[1,], cases.ci[3,], col = "lightblue")
lines(1:ncol(cases.ci), cases.ci[2,])
