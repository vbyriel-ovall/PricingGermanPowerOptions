##### Functions

# Black Scholes maximum likelihood

BS_MLE <- function(x_fit, delta, n_runs = 10){
  
  BS_lik <- function(param, X_fit, delta){
    sigma <- param[1]
    mu <- param[2]
    
    -sum(dnorm(x = X_fit, mean = (mu-(1/2)*sigma^2)*delta, sd = sigma*sqrt(delta), log = TRUE))
  }
  
  cores <- 7
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(), environment())
  
  all_runs <- parLapply(cluster, 1:n_runs, fun = function(ins){
    
    opt_single_run  <- try(optim(par = c(runif(1,1e-9,1-1e-9  ), runif(1,1e-9,1-1e-9  )),
                                 fn = BS_lik, X_fit = x_fit, delta = delta,
                                 method = "L-BFGS-B", lower = c(1e-9, -Inf), upper = c(1-1e-9, Inf)))
    
    if(class(opt_single_run) == "try-error"){
      data.frame(sigma = NA,
                 mu = NA,
                 opt_val = Inf)
    }else{
      data.frame(sigma = opt_single_run$par[1],
                 mu = opt_single_run$par[2],
                 opt_val = opt_single_run$value[1])
    }
  })
  
  stopCluster(cluster)
  
  all_runs <- do.call(rbind, all_runs)
  all_runs <- all_runs[order(all_runs$opt_val),]
  
  return(
    all_runs %>% slice(1)
  )
}

# Simulation --------------------------------------------------------------

#Inverse Gaussian distribution


IG=function(n,mu,lambda){
  X=numeric()
  for(i in 1:n){
    N=rnorm(1,0,1)
    Y=N^2
    X_1=mu[i] + (mu[i]^2*Y)/(2*lambda[i])-(mu[i])/(2*lambda[i])*sqrt(4*mu[i]*lambda[i]*Y+mu[i]^2*Y^2)
    U=runif(1,0,1)
    X[i]=ifelse(U<=mu[i]/(X_1+mu[i]),X_1,mu[i]^2/X_1)
  }
  return(X)
}

#Normal inverse gaussian distribution

Sub_NIG_simulate <- function(T, delta, ...){
  list2env(list(...), environment())
  t_grid <- seq(0, T, by = delta)
  mu <-  replicate(length(t_grid), ddelta^2)
  lambda <- replicate(length(t_grid), ddelta/sqrt(alpha^2-beta^2)) #creating a vector with same values of the desired length
  IGs <- IG(n=length(t_grid),mu=mu,lambda=lambda)
  N_sim <- rnorm(length(t_grid), 0, 1)
  
  delta_X <- N_sim * sqrt(IGs) + beta * IGs + mu * delta
  
  return(cumsum(delta_X))
}



# European Call option payoff function --------------------------------------

Euro_Call_payoff <- function(x,k){
  max(x - k, 0)
}

Euro_Put_payoff <- function(x,k){
  max(k - x, 0)
}


Call_payoff <- function(k,x){
  pmax(x - k, 0)
}

#Black Scholes Call price
BS_Call <- function(S, K, r, T, sigma) {
  d1  <-  (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  d2  <-  d1 - sigma*sqrt(T)
  S * pnorm(d1)  - K*exp(-r*T)*pnorm(d2)
}

#Moment generating function for NIG 
NIG_MGF=function(alpha,beta,ddelta, mu,h,t){
  exp(ddelta*t*(sqrt(alpha^2-(beta+h)^2)-sqrt(alpha^2-(beta+h+1)^2)) + mu)
}

##################################################################################
########### Har fjernet +1 i sqrt(alpha^2-(beta+h+1)^2))
#####################################################################################

# Option pricing ------------------------------------------------------------

esscher_parameter <- function(beta){
  -beta*(1/2)
}

Radon_Nik_Esscher <-  function(x_sim,h, delta, alpha, beta, ddelta, mu){
  return(
    exp(h*x_sim) / NIG_MGF(alpha = alpha, beta = beta,  mu = mu,
                           ddelta = ddelta, h = h, t = delta)
  )
}

option_pUT <- function(alpha, beta, ddelta, mu, T,
                       delta, h, M, K, r, to_mean = TRUE, first_price = FALSE, ...){
  
  list2env(list(...), environment())
  cores <- 7
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(), environment())
  
  NIG_price_path <- parLapply(cluster, 1:M, fun = function(runs){
    Sub_NIG_simulate(T = T, delta = delta, alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)
  })
  stopCluster(cluster)
  
  NIG_price_path <- do.call(cbind, NIG_price_path)
  NIG_price_path <- NIG_price_path[-nrow(NIG_price_path),]   #removing last row
  
  if(first_price == TRUE){
    NIG_price_path <- NIG_price_path[nrow(NIG_price_path),] # using last row
    R_N_E <- Radon_Nik_Esscher(x_sim = NIG_price_path, h = h, delta = delta,
                               alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)
    NIG_price_path <-  F_0[1]*exp(NIG_price_path)
    return(lapply(1:length(NIG_price_path), FUN = function(input){
      Euro_Put_payoff(
        x = NIG_price_path[input], k = K) * R_N_E[input]
    }) %>% do.call(what = rbind))
  }else{
    NIG_price_path <- apply(NIG_price_path, 2, function(input){rev(input)})
    
    R_N_E <- Radon_Nik_Esscher(x_sim = NIG_price_path, h = h, delta = delta,
                               alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)
    NIG_price_path <-  F_0*exp(NIG_price_path)
    
    NIG_price_path <- apply(NIG_price_path, c(1,2),
                            FUN = function(input){
                              Euro_Put_payoff(
                                x = input, k = K)
                            }) * R_N_E
  }
  if(to_mean == TRUE){
    pay_off <- apply(NIG_price_path, MARGIN = 1, FUN = function(input){
      mean(input)
    })
  }else{
    return(NIG_price_path)
  }
  
  return(pay_off)
}


## Call
option_pricing_path <- function(alpha, beta, ddelta, mu, T,
                                delta, h, M, K, r, to_mean = TRUE, first_price = FALSE, ...){
  
  list2env(list(...), environment())
  cores <- 7
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(), environment())
  
  NIG_price_path <- parLapply(cluster, 1:M, fun = function(runs){
    Sub_NIG_simulate(T = T, delta = delta, alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)
  })
  stopCluster(cluster)
  
  NIG_price_path <- do.call(cbind, NIG_price_path)
  NIG_price_path <- NIG_price_path[-nrow(NIG_price_path),]   #removing last row
  
  if(first_price == TRUE){
    NIG_price_path <- NIG_price_path[nrow(NIG_price_path),] # using last row
    R_N_E <- Radon_Nik_Esscher(x_sim = NIG_price_path, h = h, delta = delta,
                               alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)
    NIG_price_path <-  F_0[1]*exp(NIG_price_path)
    return(lapply(1:length(NIG_price_path), FUN = function(input){
      Euro_Call_payoff(
        x = NIG_price_path[input], k = K) * R_N_E[input]
    }) %>% do.call(what = rbind))
  }else{
    NIG_price_path <- apply(NIG_price_path, 2, function(input){rev(input)})
    
    R_N_E <- Radon_Nik_Esscher(x_sim = NIG_price_path, h = h, delta = delta,
                               alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)
    NIG_price_path <-  F_0*exp(NIG_price_path)
    
    NIG_price_path <- apply(NIG_price_path, c(1,2),
                            FUN = function(input){
                              Euro_Call_payoff(
                                x = input, k = K)
                            }) * R_N_E
  }
  if(to_mean == TRUE){
    pay_off <- apply(NIG_price_path, MARGIN = 1, FUN = function(input){
      mean(input)
    })
  }else{
    return(NIG_price_path)
  }
  
  return(pay_off)
}

#### No esscher pricing call

ny_pricing_path <- function(alpha, beta, ddelta, mu, T,
                            delta, M, K, r, to_mean = TRUE, first_price = FALSE, ...){
  
  list2env(list(...), environment())
  cores <- 7
  cluster <- makeCluster(cores)
  clusterExport(cluster, ls(), environment())
  
  NIG_price_path <- parLapply(cluster, 1:M, fun = function(runs){
    Sub_NIG_simulate(T = T, delta = delta, alpha = alpha, beta = beta, ddelta = ddelta, mu = mu)
  })
  stopCluster(cluster)
  
  NIG_price_path <- do.call(cbind, NIG_price_path)
  NIG_price_path <- NIG_price_path[-nrow(NIG_price_path),]   #removing last row
  
  if(first_price == TRUE){
    NIG_price_path <- NIG_price_path[nrow(NIG_price_path),] # using last row
    NIG_price_path <-  F_0[1]*exp(NIG_price_path)
    return(lapply(1:length(NIG_price_path), FUN = function(input){
      Euro_Call_payoff(
        x = NIG_price_path[input], k = K)
    }) %>% do.call(what = rbind))
  }else{
    NIG_price_path <- apply(NIG_price_path, 2, function(input){rev(input)})
    NIG_price_path <-  F_0*exp(NIG_price_path)
    NIG_price_path <- apply(NIG_price_path, c(1,2),
                            FUN = function(input){
                              Euro_Call_payoff(
                                x = input, k = K)
                            }) 
  }
  if(to_mean == TRUE){
    pay_off <- apply(NIG_price_path, MARGIN = 1, FUN = function(input){
      mean(input)
    })
  }else{
    return(NIG_price_path)
  }
  
  return(pay_off)
}


### Hedging --------------------------------------------------------------------

#Finding delta

BS_delta=function(S, K, r, T, sigma){
  d1 <- (log(S/K) + (r + sigma^2/2)*T) / (sigma*sqrt(T))
  delta <- pnorm(d1)
  return(delta)
}

finite.differences <- function(x, y) {
  if (length(x) != length(y)) {
    stop('x and y vectors must have equal length')
  }
  
  n <- length(x)
  
  # Initialize a vector of length n to enter the derivative approximations
  fdx <- vector(length = n)
  
  # Iterate through the values using the forward differencing method
  for (i in 2:n) {
    fdx[i-1] <- (y[i-1] - y[i]) / (x[i-1] - x[i])
  }
  
  # For the last value, since we are unable to perform the forward differencing method 
  # as only the first n values are known, we use the backward differencing approach
  # instead. Note this will essentially give the same value as the last iteration 
  # in the forward differencing method, but it is used as an approximation as we 
  # don't have any more information
  fdx[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])
  
  return(fdx)
}

backward_finite_diff <- function(S_1, S_0, C_1, C_0){
  delta=(C_1 - C_0) / (S_1 - S_0)
  return(ifelse(abs(S_1 - S_0) < 0.0001,0,delta))
}


delta_hedge <- function(Price_seq, delta_seq,initial_price){
  
  hedge_df <- data.frame(deltas = delta_seq,
                         prices = Price_seq,
                         bank = 0,
                         Position_val = 0)
  
  
  for(i in 1:nrow(hedge_df)){
    if(i == 1){
      hedge_df$bank[1] <- initial_price - (hedge_df$deltas[1]*hedge_df$prices[1])
      hedge_df$Position_val[1] <- hedge_df$deltas[1]*hedge_df$prices[1]
      hedge_df$portfolio_val[1] <- hedge_df$bank[1] +  hedge_df$Position_val[1]
    }else{
      hedge_df$bank[i] <-  hedge_df$bank[i-1] - (hedge_df$deltas[i] - hedge_df$deltas[i-1])*hedge_df$prices[i]
      hedge_df$Position_val[i] <- hedge_df$deltas[i]*hedge_df$prices[i]
      hedge_df$portfolio_val[i] <- hedge_df$bank[i] +  hedge_df$Position_val[i]
    }
  }
  
  return(hedge_df)
}


############ QQ_plot #############
QQ_plot_base <- function(x, y){
  order_x <- x[order(x)]
  order_y <- y[order(y)]
  ggplot() + geom_point(aes(x = order_x, y = order_y)) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1) + 
    xlab("Actual log return") + ylab("Simulated log returns")
}


