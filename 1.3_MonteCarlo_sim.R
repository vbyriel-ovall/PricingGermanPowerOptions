# Option pricing

source("C:/Users/vikto/OneDrive/Skrivebord/Speciale/R/Functions.R")

############# Training sets ###############

Period1 <- train %>% subset(as.Date(Date) >= as.Date("2017-06-01") &
                              as.Date(Date) < as.Date("2020-02-03"))


Price_seq <- Period1$Price
P1_retu <- diff(log(Price_seq), lag=1)

############ Test set Option 1 #################

test1 <- train %>% subset(as.Date(Date) >= as.Date("2020-02-03") &
                            as.Date(Date) < as.Date("2020-03-31"))

test_seq <- test1$Price #F1/S1 length=40
Datetime_seq <- test1$Date 


#############################################################

# Call option price with NIG

delta <- 1/11
T <-  length(test_seq) * delta
#K <- seq(27,51,1)
K=27
F_0 <- fp$Price

time_grid <- seq(0, T, delta)[-1]

mu=-0.0004859075
alpha=0.4204767468
beta=0.0005465471 
ddelta=0.0269269065
h <- esscher_parameter(beta = beta)

whole_M <- c()
M <- 100
pb <-  pbapply::timerProgressBar(min = 0, max = M, initial = 0)
for(i in 1:M){ #30 min
  call_prices_NIG <- lapply(X = 1000,
                            FUN = function(input){
                              option_pricing_path(
                                alpha = alpha, beta = beta,
                                ddelta = ddelta, mu = 0, T = T, delta = delta, h = h,
                                K = K, M = input, r = 0,
                                Sub_NIG_simulate = Sub_NIG_simulate, IG = IG, 
                                Euro_Call_payoff = Euro_Call_payoff, F_0 = test_seq,
                                to_mean = FALSE,
                                first_price = TRUE)
                            })
  call_prices_NIG <- call_prices_NIG %>% unlist()
  whole_M <- c(whole_M, call_prices_NIG )
  pbapply::setTimerProgressBar(pb,i)
}
call_prices_NIG <- whole_M
call_prices_NIG <-  cumsum(call_prices_NIG)/seq_along(call_prices_NIG)
length(call_prices_NIG)


#save(call_prices_NIG, file = "C:/Users/vikto/OneDrive/Skrivebord/Speciale/R/Call_price_NIG_10000.Rdata", compress = "xz")
#load("C:/Users/vikto/OneDrive/Skrivebord/Speciale/R/Call_price_NIG_10000.Rdata")




Call_price_NIG_M_1 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG), y = call_prices_NIG)) + 
  ylab("Call Price") + xlab("N") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG)),
                     labels = c(1, "100000"));print(Call_price_NIG_M_1)

Call_price_NIG_M_2 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG[1000:100000]), y = call_prices_NIG[1000:100000])) + 
  ylab("Call Price") + xlab("N") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG[1000:100000])),
                     labels = c(1000, "100000"));print(Call_price_NIG_M_2)

Call_price_NIG_M_3 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG[10000:100000]), y = call_prices_NIG[10000:100000])) + 
  ylab("Call Price") + xlab("N") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG[10000:100000])),
                     labels = c(10000, "100000"));print(Call_price_NIG_M_3)


Call_price_NIG_M_4 <- ggplot() + 
  geom_line(aes(x = 1:length(call_prices_NIG[50000:100000]), y = call_prices_NIG[50000:100000])) + 
  ylab("Call Price") + xlab("N") +
  scale_x_continuous(breaks = c(0, length(call_prices_NIG[50000:100000])),
                     labels = c(50000, "100000"));print(Call_price_NIG_M_4)

Call_price_NIG_M <- grid.arrange(Call_price_NIG_M_1, Call_price_NIG_M_2, 
                                 Call_price_NIG_M_3, Call_price_NIG_M_4)
ggsave("Call_price_NIG_M.png", Call_price_NIG_M, device = "png", dpi = 320)
