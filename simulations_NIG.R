source("C:/Users/vikto/OneDrive/Skrivebord/Speciale/R/Functions.R")

Period1 <- train %>% subset(as.Date(Date) >= as.Date("2018-01-02") &
                              as.Date(Date) < as.Date("2019-04-08"))


###--Simulation - The One in project -------------------------------###############################

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

mu=0
alpha=0.4204767468
beta=0.0005465471 
ddelta=0.0269269065

mu1 <- replicate(length(t_grid), ddelta^2)
lambda1 <- replicate(length(t_grid), ddelta/sqrt(alpha^2-beta^2))

IG(n=length(t_grid),mu=mu,lambda=lambda1)

delta=1/11
t_grid <- seq(0, T, by = delta)
mu <- diff(t_grid)

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
# Sub_NIG_sim(T= length(test_seq) * delta,
#                         delta=delta, sigma=0.004017535,theta=0.000117011,kappa=0.1079041,
#                         mu_nig = 0)[-(length(test_seq)+1)]
Sub_NIG_simulate(T=length(test_seq)*delta,delta=delta, alpha=alpha,beta=beta,ddelta=ddelta,mu=0)

N=10
## Simulating N NIG price paths - each of length of test_seq=30
NIG_price_path <- lapply(1:N, FUN = function(inpunt){
  Sub_NIG_simulate(T=length(test_seq[1:30])*delta,delta=delta, alpha=alpha,beta=beta,ddelta=ddelta,mu=0)[-(length(test_seq[1:30])+1)]
  # NIG_price_path <- do.call(cbind, NIG_price_path)
  #  NIG_price_path <-  S_0[1]*exp(NIG_price_path)
})

F_0=fp$Price

NIG_price_path <- do.call(cbind, NIG_price_path)
NIG_price_path <-  F_0[1]*exp(NIG_price_path)

#### test af moment generating function for Radon Nikodym derivative. ###################################################
NIG_MGF(alpha = alpha,beta = beta, ddelta = ddelta, mu = 0, h=h, t=delta)


# MGF=function(alpha,beta,ddelta, mu,h,t){
#   exp(ddelta*t*(sqrt(alpha^2-(beta+h)^2)-sqrt(abs((1+alpha)^2-(beta+h+1)^2))) + mu)
# }
# MGF(alpha = alpha, beta = beta, ddelta = ddelta, mu = 0, h=h, t=delta)
# Radon_Nik <-  function(x_sim,h, delta, alpha, beta, ddelta, mu){
#   return(
#     exp(h*x_sim) / MGF(alpha = alpha, beta = beta,  mu = mu,
#                            ddelta = ddelta, h = h, t = delta)
#   )
# }

te <- Radon_Nik_Esscher(NIG_price_path, h=h, delta = delta, alpha = alpha, beta = beta, ddelta = ddelta, mu=0)
########################################################################################################################


### Plotting simulated NIG paths ####----------------------------------------
F0 <- rep(41.47,10)
Path <- rbind(rep(41.47,10),NIG_price_path)
data <- data.frame(x=0:30,
                   y1=Path[,1],
                   y2=Path[,2],
                   y3=Path[,3],
                   y4=Path[,4],
                   y5=Path[,5],
                   y6=Path[,6],
                   y7=Path[,7],
                   y8=Path[,8],
                   y9=Path[,9],
                   y10=Path[,10])

data_ggp <- data.frame(x = data$x,                            # Reshape data frame
                       y = c(data$y1, data$y2, data$y3,data$y4,data$y5,data$y6,data$y7,data$y8,data$y9,data$y10),
                       group = c(rep("y1", nrow(data)),
                                 rep("y2", nrow(data)),
                                 rep("y3", nrow(data)),
                                 rep("y4", nrow(data)),
                                 rep("y5", nrow(data)),
                                 rep("y6", nrow(data)),
                                 rep("y7", nrow(data)),
                                 rep("y8", nrow(data)),
                                 rep("y9", nrow(data)),
                                 rep("y10",nrow(data))))

ggp <- ggplot(data_ggp, aes(x, y, col = group)) +             # Create ggplot2 plot
  geom_line()+
  ylab("") + xlab("")+ theme(legend.position = "none")
ggp      
ggsave("Sim_paths.png", ggp, device = "png", dpi = 320)

