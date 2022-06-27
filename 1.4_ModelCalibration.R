source("functions.R")

library(dplyr);library(tidyverse);library(parallel)

##### Get data
options_feb <- read.csv2("C:/Users/vikto/Onedrive/Skrivebord/Projects/PricingGermanPowerOptions/Data/monthly_options-feb.csv")
options_feb$X <- as.Date(options_feb$X, '%Y-%m-%d')

options_feb %>% select(X,X.3,X.6)

options_p1 <- options_feb %>% subset(as.Date(options_feb$X) == as.Date("2020-02-04")) 
options_p1$X.3 <- as.Date(options_p1$X.3, '%Y-%m-%d')

options_p1 <- options_p1 %>% subset(as.Date(options_p1$X.3) == as.Date("2020-03-01"))
options_p1 %>% count(X.6)

##### Model Calibration function setup

delta <- 1/11
#test_seq <- 1:40
T <-  length(test_seq) * delta
time_grid <- seq(0, T, delta)[-1]
F_0 <- 32.01
K=27:45  #27:45
N=10 # number of monte carlo sims
n <- length(K)
mu <- 0
MG_mu_start <- -delta0*(sqrt((alpha0^2-(beta0)^2))-sqrt(alpha0^2-(beta0+1)^2))

#initial parameters in optim
alpha0 <- 24
beta0 <- 0.75
delta0 <- 0.01745
mu0 <- 0.0005
lB     <- c(18,0.1,0.001,0.0001)
uB     <- c(30,3,0.5,0.1)
theta <- c(alpha0,beta0,delta0,mu0)

### real call prices

Truecall <- options_p1 %>% subset(options_p1$X.6 == "C") 
Truecall$X.7 <- gsub(',','.',Truecall$X.7)
Truecall$X.7 <- gsub(' EUR/MWh','',Truecall$X.7)
Actual_calls <- as.numeric(Truecall$X.7[26:50]) %>% rev() %>% replace(is.na(.), 0)

##### Option pricing function --------------------------------------------------------------


funcOP11 <- function(alpha, beta, ddelta, mu, T,
                     delta, N, K){
  NIG_price_path <- lapply(1:length(K), FUN = function(input){
    mapply(1:N, FUN = function(input){
      Sub_NIG_simulate(T=length(test_seq)*delta, delta = delta, alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)[-(length(test_seq)+1)]
    })})
  NIG_price_path <- lapply(NIG_price_path, function(input){
    F_0[1]*exp(input)
  })
  g=list()
  for (j in 1:length(K)) {
    g[[j]] <- Call_payoff(k=K[j],x=NIG_price_path[[j]])  
  }
  g <- lapply(g, rowMeans)
  g <- do.call(cbind, g) # for price paths
  g_last <- g[nrow(g),] # for optim model calibration
  return(g_last)
}

funcOP_path <- function(alpha, beta, ddelta, mu, T,
                        delta, N, K){
  NIG_price_path <- lapply(1:length(K), FUN = function(input){
    mapply(1:N, FUN = function(input){
      Sub_NIG_simulate(T=length(test_seq)*delta, delta = delta, alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)[-(length(test_seq)+1)]
    })})
  NIG_price_path <- lapply(NIG_price_path, function(input){
    F_0[1]*exp(input)
  })
  g=list()
  for (j in 1:length(K)) {
    g[[j]] <- Call_payoff(k=K[j],x=NIG_price_path[[j]])  
  }
  g <- lapply(g, rowMeans)
  g <- do.call(cbind, g) # for price paths
  #g_last <- g[nrow(g),] # for optim model calibration
  return(g)
}

MonteCarloCall <- mapply(funcOP11,
                         K = K,
                         N=N,
                         T=T,
                         delta=delta,
                         alpha = rep(theta[1],length(K)),
                         beta = rep(theta[2],length(K)),
                         ddelta = rep(theta[3],length(K)),
                         mu = rep(theta[4],length(K)))
return(sum(abs(MonteCarloCall[1:length(K)]-Actual_calls[1:length(K)])/n))

### Model Calibration --------------------------------------------------------------

#optModCalibrate <- function(theta,K,N,T,delta) {
funcCalibrate <- function(theta) {  
  MonteCarlo_Call <- mapply(funcOP11,
                            K = K,
                            N=N,
                            T=T,
                            delta=delta,
                            alpha = rep(theta[1],length(K)),
                            beta = rep(theta[2],length(K)),
                            ddelta = rep(theta[3],length(K)),
                            mu = rep(theta[4],length(K)))
  return(sum(abs(MonteCarlo_Call[1:length(K)]-Actual_calls[1:length(K)])/n))
}

alpha0 <- 24
beta0 <- 0.75
delta0 <- 0.01745
mu0 <- 0.0005

lB     <- c(18,0.1,0.001,0.0001)
uB     <- c(30,3,3,0.5)
theta <- c(alpha0,beta0,delta0,mu0)
n <- length(K)

N=10000
K=27:45

start <- Sys.time()
thetaOptim <- optim(theta, funcCalibrate, 
                    lower = lB, upper = uB, 
                    method = "L-BFGS-B", 
                    control = list(trace = TRUE, maxit = 500))
end <- Sys.time()
time <- end - start;print(time)

### time for number of sims
time_n_200 <- "4.551889 mins"
time_n_2000 <- "50.77658 mins"
time_n_10000 <- "2.760823 hours"

# Save output
output <- data.frame(alpha.fit = thetaOptim$par[1],
                     beta.fit = thetaOptim$par[2],
                     delta.fit = thetaOptim$par[3],
                     mu_fit = thetaOptim$par[4],
                     opt_val = thetaOptim$value[1])

MG_mu <- -output$delta.fit*(sqrt((output$alpha.fit^2-(output$beta.fit)^2))-sqrt(output$alpha.fit^2-(output$beta.fit+1)^2))

#remember F_0=32.01
F_0=32.01
K=27:45
alpha=23.99686
beta=0.761928
ddelta=0.01736279
mu= -0.0009145257

alpha=23.919666860  
beta=0.701220364  
ddelta=0.015762641
mu=-0.0007927573

start <- Sys.time()
CalCalls_paths <- funcOP_path(alpha = alpha, beta = beta, ddelta = ddelta, mu = mu, delta = delta, T = T, N = 10000, K = K)
end <- Sys.time()
time <- end - start;print(time)

CalCalls <- CalCalls_paths[nrow(CalCalls_paths),]
#saveRDS(CalCalls, file = "calibrate_prices.Rda")
pricesCalls <- readRDS(file="calibrate_prices.Rda")

AAE_calibrate <- sum(abs(CalCalls[1:19] - Actual_calls[1:19]))/length(Actual_calls[1:19])
RMSE_calibrate <- sqrt(sum((CalCalls[1:19] - Actual_calls[1:19])^2)/length(Actual_calls[1:19]))

CalibratedData <- data.frame(K,pricesCalls, Actual_calls[1:19])

CalibratedvsTrue <- ggplot(data = CalibratedData, mapping = aes(x=K))+
  geom_vline(aes(xintercept = 32.01, color = "F_0"), linetype="dashed", size=0.73)+
  geom_line(mapping = aes(y=CalCalls, color="Calibrated Calls"))+
  geom_line(mapping = aes(y=Actual_calls[1:19], color="Actual Calls"))+
  geom_point(mapping = aes(y=CalCalls,color="Calibrated Calls"))+
  geom_point(mapping = aes(y=Actual_calls[1:19], color="Actual Calls"))+
  scale_color_manual(labels = c("Actual Calls", "Calibrated Calls", "F_0"),
                     breaks = c("Actual Calls", "Calibrated Calls","F_0"), 
                     values = c("cyan3", "coral2","green3"),
                     name = "") +
  ylab("Call Price") + xlab("Strike K") +
  labs(color = "") + theme(legend.position = "bottom");print(CalibratedvsTrue)

ggsave("CalibratevsTrue2.png", plot = CalibratedvsTrue, dpi = 320)


pg <- ggplot_build(CalibratedvsTrue)
caldat <-  pg$data[[2]]

#############################################################################################
################################# Implied volatility.#######################################

source("C:/Users/vikto/OneDrive/Skrivebord/Speciale/R/Functions.R")

library(dplyr);library(tidyverse);library(parallel)

##### Get data
options_feb <- read.csv2("C:/Users/vikto/Onedrive/Skrivebord/Speciale/R/monthly_options-feb.csv")
options_feb$X <- as.Date(options_feb$X, '%Y-%m-%d')

options_p1 <- options_feb %>% subset(as.Date(options_feb$X) == as.Date("2020-02-04")) 
options_p1$X.3 <- as.Date(options_p1$X.3, '%Y-%m-%d')

options_p1 <- options_p1 %>% subset(as.Date(options_p1$X.3) == as.Date("2020-03-01"))
options_p1 %>% count(X.6)


### setup parameters
r=0
delta <- 1/11
test_seq <- test1$Price
T <-  length(test_seq) * delta
time_grid <- seq(0, T, delta)[-1]
F_0 <- 32.01
K=27  #27:45
N=100000 # number of monte carlo sims
n <- length(K)


ImpVol <- function(S,K,T,r,vol0 = 1, C_option){
  vol <- vol0
  fvol <- 1
  while(abs(fvol) >= 1e-10) {
    d1 <- (log(S/K)+(r+vol^2/2)*T)/(vol*sqrt(T))
    fvol <- BS_Call(S, K, r, T, sigma=vol) - C_option
    dfvol <- S*dnorm(d1)*sqrt(T)
    vol = vol - fvol/dfvol;vol
  }
  return(vol)
}

K=c(27:45)

IV <- c()
for (i in 1:17) {
  IV[i] <- ImpVol(S = F_0, K=K[i], T=T, r=0, vol0=1, C_option = Actual_calls[i])
}

NIG_IV <- c()
for (i in 1:17) {
  NIG_IV[i] <- ImpVol(S = F_0, K=K[i], T=T, r=0, vol0=1, C_option = CalCalls[i])
}


ImpvolData <- data.frame(K[1:17],IV,NIG_IV)

ImpVolPlot <- ggplot(data = ImpvolData, mapping = aes(x=K[1:17]))+
  geom_vline(aes(xintercept = 32.01,color = "F_0"), linetype="dashed", size=0.73)+
  geom_point(mapping = aes(y=IV,color="Implied volatility"), shape=1, size=2)+
  geom_line(mapping = aes(y=IV,color="Implied volatility"))+
  geom_line(mapping = aes(y=NIG_IV,color="NIG smile"))+
  geom_point(mapping = aes(y=NIG_IV,color="NIG smile"))+
  scale_color_manual(labels = c("Implied volatility", "NIG smile", "F_0"),
                     breaks = c("Implied volatility", "NIG smile","F_0"), 
                     values = c("red", "dodgerblue1","green3"),
                     name = "") +
  ylab("Implied volatility") + xlab("Strike K") +
  labs(color = "") + 
  theme(legend.position = "bottom");print(ImpVolPlot)

ggsave("ImpVolPlot.png", plot = ImpVolPlot, dpi = 320)



