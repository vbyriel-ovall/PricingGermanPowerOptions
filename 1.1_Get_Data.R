library(ggplot2)
library(dplyr)

## Getting data and setup

## "./Data//SPY.csv

Data2017 <- read.csv2("./Data//2017PowerFutures.csv") 
Data2018 <- read.csv("./Data//2018PowerFuture.csv")
Data2019 <- read.csv("./Data/2019PowerFuture.csv")
Data2020 <- read.csv("./Data/2020PowerFuture.csv")

Dat2017 <- Data2017[,c(1,11)]
Dat2018 <- Data2018[,c(1,11)]
Dat2019 <- Data2019[,c(1,7)]
Dat2020 <- Data2020[,c(1,7)]

Ft17 <- Dat2017[seq(2,nrow(Data2017),10),]
Ft18 <- Dat2018[seq(2,nrow(Data2018),10),]
Ft19 <- Dat2019[seq(2,nrow(Data2019),10),]
Ft20 <- Dat2020[seq(2,nrow(Data2020),10),]

Ft17 <- Ft17[rev(rownames(Ft17)),]
Ft18 <- Ft18[rev(rownames(Ft18)),]
Ft19 <- Ft19[rev(rownames(Ft19)),]
Ft20 <- Ft20[rev(rownames(Ft20)),]


Ft17_t <- data.frame('Date'=Ft17$Trading.Day,'Price'=Ft17$Settlement.Price)
Ft18 <- data.frame('Date'=Ft18$Trading.Day,'Price'=Ft18$Settlement.Price)
Ft19 <- data.frame('Date'=Ft19$Trading.Day,'Price'=Ft19$Settlement.Price)
Ft20 <- data.frame('Date'=Ft20$Trading.Day,'Price'=Ft20$Settlement.Price)

Ft17_t$Date <- format(as.Date(Ft17_t$Date, format = "%d-%m-%Y"), '%m/%d/%Y')

train <- rbind(Ft17_t,Ft18,Ft19,Ft20)
test <- Ft20
train$Date <- as.Date(train$Date, '%m/%d/%Y')

# ################ PLOT ###############
{
# price_plot <- train %>% 
#   ggplot(aes(x=Date,y=Price))+
#   geom_line()+
#   labs(x="", y="");print(price_plot)
# 
# ggsave("Price_Plot.png", plot=price_plot, dpi = 320)

############ Seasonality ################
# ggAcf(as.ts(train$Price), lag.max = 365)
# 
# acf(train$Price, lag.max = 100)


############# Log return ##############
# prices <- train$Price
# log_returns <- diff(log(prices), lag=1)
# 
# plot(log_returns)
# ggAcf(as.ts(log_returns), lag.max = 100)
# 
# hist(log_returns,breaks = 100)
# qqnorm(log_returns)
# qqline(log_returns)
}

### Option data
options_feb <- read.csv2("./Data//monthly_options-feb.csv")
options_feb$X <- as.Date(options_feb$X, '%Y-%m-%d')


options_p1 <- options_feb %>% subset(as.Date(options_feb$X) == as.Date("2020-02-04")) 
options_p1$X.3 <- as.Date(options_p1$X.3, '%Y-%m-%d')

options_p1 <- options_p1 %>% subset(as.Date(options_p1$X.3) == as.Date("2020-03-01"))

options_p1 %>% count(X.6)

fp <- train %>% subset(as.Date(Date) == as.Date("2020-02-04")) 



############ Test set Option 1 #################

test1 <- train %>% subset(as.Date(Date) >= as.Date("2020-02-04") &
                            as.Date(Date) < as.Date("2020-03-31"))

test_seq <- test1$Price #F1/S1 length=40
Datetime_seq <- test1$Date 
