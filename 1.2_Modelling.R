####### Normal inverse Gaussian fitting #######

source("C:/Users/vikto/OneDrive/Skrivebord/Speciale/R/Functions.R")
#C:\Users\vikto\OneDrive\Skrivebord\Uni\Hedging-With-Normal-Inverse-Gaussian-process-master\Hedging-With-Normal-Inverse-Gaussian-process-master\Code_Library
source("C:/Users/vikto/OneDrive/Skrivebord/Uni/Hedging-With-Normal-Inverse-Gaussian-process-master/Hedging-With-Normal-Inverse-Gaussian-process-master/Code_Library/functions.R")

############# Training sets ###############		


Period1 <- train %>% subset(as.Date(Date) >= as.Date("2017-06-01") &
                              as.Date(Date) < as.Date("2020-02-03"))


Price_seq <- Period1$Price
P1_retu <- diff(log(Price_seq), lag=1)

############ MODEL FITTING ##################

fb_nigFit <- nigFit(P1_retu)

alpha=24.0737539230  
beta=0.7494001284
ddelta=0.0174526725     
mu=-0.0004870304 

dens <- density(P1_retu)

fb_nigdens <- dnig(dens$x, alpha = alpha, beta = beta, delta = ddelta, mu = 0) 
density_data <- data.frame(dens$x,fb_nigdens)

NI_E <- ggplot() +
  geom_density(data=as.data.frame(P1_retu),aes(x=P1_retu, colour = "Empirical")) +
  geom_line(data=density_data,aes(x=dens$x, y = fb_nigdens, colour = "NIG")) +
  scale_colour_manual("", 
                      breaks = c("Empirical", "NIG"),
                      values = c("red", "blue")) +
  xlab("Log returns") +
  scale_y_continuous("Density", limits = c(0,24));print(NI_E)

ggsave("EmpvsNIG_dens_P1.png", NI_E, dpi = 320)

### QQ plot of the NIG model fit

probs <- seq(1e-3, 1-1e-3, length = 678)
quantiles <- ghyp::qghyp(probs, P1nigfit)

quantiles <- qnig(probs, alpha = alpha, beta = beta, delta = ddelta, mu=0)

NIG_qq_plot_period1 <- QQ_plot_base(x = diff(log(Period1$Price)), y = quantiles) + 
  ylab("NIG log returns");print(NIG_qq_plot_period1)+
  xlim(-0.15,0.15)+ylim(-0.17,0.17)


#Fit Black-Scholes for comparison. 
# delta = (T-t0)/N
delta <- 1/11

n <- length(Price_seq)
t <-  length(Price_seq) * delta

bs_fit_period1 <- BS_MLE(P1_retu, delta = delta)
mu <- bs_fit_period1$mu  #0.00497969
sigma <- bs_fit_period1$sigma #0.09

set.seed(123)
BS_prices <- sde::sde.sim(model = "BS" ,X0=Price_seq[1], theta = c(mu,sigma), N=678)

BS_qq_plot_period1 <- QQ_plot_base(x = diff(log(Price_seq)), y = diff(log(BS_prices))) + ylab("BS log returns");print(BS_qq_plot_period1)



QQPLOTS1 <- grid.arrange(BS_qq_plot_period1, NIG_qq_plot_period1, nrow = 1)

ggsave("QQ1.png", plot = QQPLOTS1, dpi = 320)

