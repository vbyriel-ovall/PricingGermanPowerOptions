### European Call Option 1 - Delta Hedging

source("C:/Users/vikto/OneDrive/Skrivebord/Speciale/R/Functions.R")

test_seq <- test1$Price #F(t)_1 length=40
Datetime_seq <- test1$Date 

delta <- 1/11
T <-  length(test_seq) * delta
alpha=24.0737539230  
beta=0.7494001284
ddelta=0.0174526725     
mu=-0.0004870304 
h <- esscher_parameter(beta = 0.7494001284)
time_grid <- seq(0, T, delta)[-1]


#NIG call prices ---------------------------------------------------------
K=27
{
  NIG_Call_Prices <- 0
  M <- 100
  pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
  for(i in 1:M){ #1 hour 7 min
    NIG_Call_Prices_single <- option_pricing_path(
      alpha = 24.0737539230, beta = 0.7494001284,
      ddelta = 0.0174526725, mu = 0, T = T, delta = delta, h = h,
      K = K, M = 1000, r = 0,
      Sub_NIG_simulate = Sub_NIG_simulate, IG = IG, 
      Euro_Call_payoff = Euro_Call_payoff, F_0 = F_0, to_mean = FALSE)
    
    NIG_Call_Prices_single <- apply(NIG_Call_Prices_single, MARGIN = 1, FUN = function(input){
      sum(input)/(M*1000)
    })
    
    NIG_Call_Prices <- NIG_Call_Prices + NIG_Call_Prices_single
    pbapply::setTimerProgressBar(pb,i)
  }
  
  NIG_Call_Prices
}


#NIG PUT prices ---------------------------------------------------------

K=51
{
  NIG_Put_Prices <- 0
  M <- 100
  pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
  for(i in 1:M){ #1 hour 7 min
    NIG_Put_Prices_single <- option_pUT(
      alpha = alpha, beta = beta,
      ddelta = ddelta, mu = 0, T = T, delta = delta, h = h,
      K = K, M = 1000, r = 0,
      Sub_NIG_simulate = Sub_NIG_simulate, IG = IG, 
      Euro_Put_payoff = Euro_Put_payoff, F_0 = F_0, to_mean = FALSE)
    
    NIG_Put_Prices_single <- apply(NIG_Put_Prices_single, MARGIN = 1, FUN = function(input){
      sum(input)/(M*1000)
    })
    
    NIG_Put_Prices <- NIG_Put_Prices + NIG_Put_Prices_single
    pbapply::setTimerProgressBar(pb,i)
  }
  
  NIG_Put_Prices
}


################################ CALL-PUT PLOTs #################################################

K_strikes <- c(27:51)
zero <- rep(0,15)
Calls <- c(5.018204,4.01860,3.015773,2.021091,1.030470,0.223992,0.013569,0.000842,0.000051, 0.000003,zero)
Puts <- c(0,0,0.00002,0.000384,0.009917,0.201886,0.995666,1.980352,2.980035,3.977223,4.980341,5.979849,6.979862,7.975231,8.981275,9.976889,10.979730,11.978210,12.978760,13.97673,14.98077,15.977442,16.98126,17.97958,18.97861)

Truecall <- options_p1 %>% subset(options_p1$X.6 == "C") 
Truecall$X.7 <- gsub(',','.',Truecall$X.7)
Truecall$X.7 <- gsub(' EUR/MWh','',Truecall$X.7)
Actual_calls <- as.numeric(Truecall$X.7[26:50]) %>% rev() %>% replace(is.na(.), 0)

Trueput <- options_p1 %>% subset(options_p1$X.6 == "P") 
Trueput$X.7 <- gsub(',','.',Trueput$X.7)
Trueput$X.7 <- gsub(' EUR/MWh','',Trueput$X.7)
Actual_puts <- as.numeric(Trueput$X.7[26:50]) %>% rev() 


Call_simprice <- data.frame(K_strikes,Calls)
Put_simprice <- data.frame(K_strikes,Puts,Actual_puts)
All_puts <- data.frame(K_strikes,Puts, Actual_puts)
All_calls <- data.frame(K_strikes,Calls, Actual_calls)

#AAE + RMSE:
AAE_esscher <- sum(abs(Calls[1:19] - Actual_calls[1:19]))/length(Actual_calls[1:19])
RMSE_esscher <- sqrt(sum((Calls[1:19] - Actual_calls[1:19])^2)/length(Actual_calls[1:19]))

ca <- ggplot(data = All_calls, mapping = aes(x=K_strikes))+
  geom_vline(aes(xintercept = 32.01,color = "F_0"), linetype="dashed", size=0.73)+
  geom_line(mapping = aes(y=Calls, color="Sim Calls"))+
  geom_line(mapping = aes(y=Actual_calls, color="Actual Calls"))+
  geom_point(mapping = aes(y=Calls,color="Sim Calls"))+
  geom_point(mapping = aes(y=Actual_calls, color="Actual Calls"))+
  scale_color_manual(labels = c("Actual Calls", "Sim Calls", "F_0"),
                     breaks = c("Actual Calls", "Sim Calls","F_0"), 
                     values = c("cyan3", "coral2","green3"),
                     name = "") +
  ylab("Call Price") + xlab("Strike K") +
  labs(color = "") + theme(legend.position = "bottom");print(ca)

ggsave("Esschercalloption.png", plot = ca, dpi = 320)

pu <- ggplot(data = All_puts, mapping = aes(x=K_strikes))+
  geom_vline(aes(xintercept = 32.01,color = "F_0"), linetype="dashed", size=0.73)+
  geom_line(mapping = aes(y=Puts, color="Sim Puts"))+
  geom_line(mapping = aes(y=Actual_puts, color="Actual Puts"))+
  geom_point(mapping = aes(y=Puts,color="Sim Puts"))+
  geom_point(mapping = aes(y=Actual_puts, color="Actual Puts"))+ 
  scale_color_manual(labels = c("Actual Puts", "Sim Puts", "F_0"),
                     breaks = c("Actual Puts", "Sim Puts","F_0"), 
                     values = c("cyan3", "coral2","green3"),
                     name = "") +
  ylab("Put Price") + xlab("Strike K") +
  labs(color = "") + 
  theme(legend.position = "bottom");print(pu)

OptionPlot <- grid.arrange(ca, pu, nrow = 1)

ggsave("Sim_Option_plot.png", plot = OptionPlot, dpi = 320)

#### Percentage of Mispricing
mis_call <- ((Calls/Actual_calls)-1)*100
mis_put <- ((Puts/Actual_puts)-1)*100

AAE <- sum(abs(Puts-Actual_puts)/length(Actual_puts))

ARPE <- (1/length(Actual_puts))*sum(abs(Puts-Actual_puts)/Actual_puts)

RMSE <- sqrt(sum(abs(Puts-Actual_puts)/length(Actual_puts)))
