library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)

f <- list(
  family = "Arial",
  size = 18,
  color = "#7f7f7f"
)

#Loads the company stock using ticker

start_date <- as.Date("2009-12-31")
end_date <- as.Date("2019-10-16")
start_date
end_date

lapply(start_date, class)
lapply(end_date, class)



getSymbols("AMZN",from=start_date,to=end_date)
getSymbols("FB",from=start_date,to=end_date)
getSymbols("TSLA",from=start_date,to=end_date)
getSymbols("AAPL",from=start_date,to=end_date)
getSymbols("GOOGL",from=start_date,to=end_date)

#Stock returns in log

AMZN_log_returns<-AMZN%>%Ad()%>%dailyReturn(type='log')
FB_log_returns<-FB%>%Ad()%>%dailyReturn(type='log')
TSLA_log_returns<-TSLA%>%Ad()%>%dailyReturn(type='log')
AAPL_log_returns<-AAPL%>%Ad()%>%dailyReturn(type='log')
GOOGL_log_returns<-GOOGL%>%Ad()%>%dailyReturn(type='log')

#Mean of log stock returns 

AMZN_mean_log<-mean(AMZN_log_returns)
FB_mean_log<-mean(FB_log_returns)
TSLA_mean_log<-mean(TSLA_log_returns)
AAPL_mean_log<-mean(AAPL_log_returns)
GOOGL_mean_log<-mean(GOOGL_log_returns)

#round it to 4 decimal places

mean_log<-c(AMZN_mean_log,FB_mean_log,TSLA_mean_log,AAPL_mean_log,GOOGL_mean_log)
mean_log<-round(mean_log,4)

#standard deviation of log stock returns

AMZN_sd_log<-sd(AMZN_log_returns)
FB_sd_log<-sd(FB_log_returns)
TSLA_sd_log<-sd(TSLA_log_returns)
AAPL_sd_log<-sd(AAPL_log_returns)
GOOGL_sd_Log<-sd(GOOGL_log_returns)

#round it to 4 decimal places 

sd_log<-c(AMZN_sd_log,FB_sd_log,TSLA_sd_log,AAPL_sd_log,GOOGL_sd_Log)
sd_log<-round(sd_log,4)

#create data frame

graphic1<-data.frame(rbind(c("AMZN",AMZN_mean_log,AMZN_sd_log),c("FB",FB_mean_log,FB_sd_log),c("TSLA",TSLA_mean_log,TSLA_sd_log),c("AAPL",AAPL_mean_log,AAPL_sd_log),c("GOOGL",GOOGL_mean_log,GOOGL_sd_Log)),stringsAsFactors = FALSE)


graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("AMZN","FB","TSLA","AAPL","GOOGL")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")

# Data frame contains the 4 companies with each company's average log return and standard deviation.

# Used plotly to create a visualization of each stock's risk v reward. 
# Risk: standard deviation of log returns
# Reward: mean of log returns

xlab<-list(title="Reward", titlefont=f)
ylab<-list(title="Risk", titlefont=f)

plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","blue","red","grey","green")))%>%layout(title="Risk vs. Reward",xaxis=xlab,yaxis=ylab)


#Use R to observe a stock's performance
#chart components: bollinger bands, % bollinger change, volume, moving average convergence divergence

AMZN%>%Ad()%>%chartSeries(name = 'Amazone (AMZN)')
AMZN%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2018/2019', name = 'Amazone (AMZN)')

FB%>%Ad()%>%chartSeries(name = 'FaceBook (FB)')
FB%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2018/2019', name = 'FaceBook (FB)')

TSLA%>%Ad()%>%chartSeries(name = 'Tesla (TSLA)')
TSLA%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2018/2019', name = 'Tesla (TSLA)')

AAPL%>%Ad()%>%chartSeries(name = 'Apple (AAPL)')
AAPL%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2018/2019', name = 'Apple (APPL)')

GOOGL%>%Ad()%>%chartSeries(name = 'Google (GOOGL)')
GOOGL%>%chartSeries(TA='addBBands();addBBands(draw="p");addVo();addMACD()',subset='2018/2019', name = 'Google (GOOGL)')

#Average stock daily return


probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)

AMZN_dist<-AMZN_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AMZN_mean<-mean(AMZN_log_returns,na.rm=TRUE)
AMZN_sd<-sd(AMZN_log_returns,na.rm=TRUE)

AMZN_mean%>%exp() # 1.001046

FB_dist<-FB_log_returns%>%quantile(probs=probs,na.rm=TRUE)
FB_mean<-mean(FB_log_returns,na.rm=TRUE)
FB_sd<-sd(FB_log_returns,na.rm=TRUE)

FB_mean%>%exp() # 1.000857

TSLA_dist<-TSLA_log_returns%>%quantile(probs=probs,na.rm=TRUE)
TSLA_mean<-mean(TSLA_log_returns,na.rm=TRUE)
TSLA_sd<-sd(TSLA_log_returns,na.rm=TRUE)

TSLA_mean%>%exp() # 1.001017

AAPL_dist<-AAPL_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AAPL_mean<-mean(AAPL_log_returns,na.rm=TRUE)
AAPL_sd<-sd(AAPL_log_returns,na.rm=TRUE)

AAPL_mean%>%exp() # 1.00089

GOOGL_dist<-GOOGL_log_returns%>%quantile(probs=probs,na.rm=TRUE)
GOOGL_mean<-mean(GOOGL_log_returns,na.rm=TRUE)
GOOGL_sd<-sd(GOOGL_log_returns,na.rm=TRUE)

GOOGL_mean%>%exp() # 1.000563



# Checking the correlation of 4 stocks: tesla, facebook, google, amazon

library(PerformanceAnalytics)
data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))),diff(log(Cl(TSLA))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))
chart.Correlation(data)

# random walk: Rooted in past performance is not an indicator of future results. Price fluctuations can not be predicted with accuracy


mu<-AMZN_mean_log
sig<-AMZN_sd_log
testsim<-rep(NA,1000)

#generate random daily exponent increase rate using AMZN's mean and sd log returns

#one year 252 trading days, simulate for 4 years 
# 4*252 trading days

price<-rep(NA,252*4)

#most recent price
price[1]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])

#start simulating prices

for(i in 2:length(testsim)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)

random_data%>%ggplot(aes(Day,Price))+geom_line()+labs(title="Amazon (AMZN) price simulation for 4 years")+theme_bw()


# monte carlo simulation: incredibly useful forecasting tool to predict outcomes of events with many random variables


N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])

for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(AMZN$AMZN.Adjusted[length(AMZN$AMZN.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)

final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as.tibble(final_mat)
colnames(final_mat)<-name

dim(final_mat) #1008 501

final_mat%>%
  gather("Simulation","Price",2:501)%>%
  ggplot(aes(x=Day,y=Price,Group=Simulation))+
  geom_line(alpha=0.2)+
  labs(title="Amazon Stock (AMZN): 500 Monte Carlo Simulations for 4 Years")+
  theme_bw()

#is it likely? Check the confidence interval

final_mat[500,-1]%>%
  as.numeric()%>%
  quantile(probs=probs)

