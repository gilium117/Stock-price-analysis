list_of_symbols <- c("MSFT","AMZN","GE","WMT","AAPL","GS","MRK","VZ")

all_price <- Null
i <- 1
for (tkr in list_of_symbols) 
  {
  tkr_prices <- get.hist.quote(instrument = tkr, start = "2008-01-01",
                               end = "2018-08-01", quote = "AdjClose",
                               provider = "yahoo", origin = "1970-01-01",
                               compression = "m", retclass = "zoo")
  if (i == 1) 
    {
    all_price = tkr_prices
  }
  else
  {
    all_price = merge(all_price,tkr_prices)
  }
  i = i + 1
}



#Assign colum names

colnames(all_price)=list_of_symbols
head(all_price)

###compute continous compounded returns [log(1+R) = log(p(t)/p(t-1))]
#the diff function helps to compute the differences in the log prices

all_cont_comp_returns <- diff(log(all_price))
colnames(all_cont_comp_returns) <- paste0(rep("ccRet",length(list_of_symbols)),list_of_symbols)
head(all_cont_comp_returns)

?paste0
## store the data in time series format
all_price_xts <- all_price
all_cont_comp_returns_xts <- all_cont_comp_returns

## add month and year as separate colums
all_price$year_price <- year(index(all_price))
all_price$month_price <- month(index(all_price))


rownames(all_price) <- index(all_price)
head(all_price)
row.names(all_price)
colnames(all_price)


year(all_price)
library(lubridate)

#### do thw month and year to the returns data as well
all_cont_comp_returns$year_price <- year(index(all_cont_comp_returns))
all_cont_comp_returns$month_price <- month(index(all_cont_comp_returns))

##convert the data into normal data frame for analysis
all_price <- data.table(as.data.frame(coredata(all_price)))
return_analysis <- as.data.frame(coredata(all_cont_comp_returns))
str(return_analysis)

##Convert the data into long format
all_price_lf <- data.table(melt(all_price, id.vars = c("year_price", "month_price")))
return_analysis_lf <- data.table(melt(return_analysis, id.vars = c("year_price", "month_price")))

##set the names for the column
setnames(all_price_lf, c("year_price","month_price","stock","price"))
setnames(return_analysis_lf, c("year_price","month_price","stock","cc_return"))

##############Compute aggregates to use in graphs##############

##Compute aggregates for the price
##by year

price_mean_by_year <- all_price_lf[,list(avg_price =
                                           mean(price),
                                         median_price = median(price),
                                         sd_price = sd(price)),
                                   by = list(year_price)]


#By year and given stock
price_mean_by_year_stock <- all_price_lf[,list(avg_price = mean(price),
                                               median_price = median(price),
                                               sd_price = sd(price)),
                                         by = list(year_price,stock)]


### By year and month
price_mean_by_year_month <- all_price_lf[,list(avg_price =
                                                 mean(price),
                                               median_price = median(price),
                                               sd_price = sd(price)),
                                         by = list(year_price,month_price)]


###Now similarly compute aggregates for Returns
#By Year
return_mean_by_year <- return_analysis_lf[,list(
  avg_cc_return = mean(cc_return),
  med_cc_return = median(cc_return),
  sd_cc_return = sd(cc_return)),
  by = list(year_price)]

#By year and given stock
return_mean_by_year_stock <- return_analysis_lf[,list(
  avg_cc_return = mean(cc_return),
  med_cc_return = median(cc_return),
  sd_cc_return = sd(cc_return)),
  by = list(year_price,stock)]

#By year and month
return_mean_by_year_month <- return_analysis_lf[,list(
  avg_cc_return = mean(cc_return),
  med_cc_return = median(cc_return),
  sd_cc_return = sd(cc_return)),
  by = list(year_price,month_price)]
############

### Create and summarize price data - Leverage price data #################

#How is the average stock price moving over the years

ggplot(price_mean_by_year_stock, aes(x = as.factor(year_price),y = avg_price, color = stock, 
                                     group = stock)) + geom_line() +
labs(x = "Year", y = "Average Price", title = "Average Prices of Stocks over Years")

###show the variability of stock across months

ggplot(all_price_lf, aes(x = as.factor(month_price), y = price, color = stock, 
                         group = stock)) + geom_line() + facet_wrap( ~ year_price, scales = "free_y")+
labs(x = "Month", y = "Price", title = "Price Distribution for every year by month")

###Drop the year and month dimension and see what happens

ggplot(all_price_lf, aes(x = as.factor(stock), y = price, group = stock)) + geom_boxplot() + 
  labs(x = "stock", y = "price", title = "Price Distribution of every stock across all years")
  
###Consider only amazon stock and draw a regression line
ggplot(all_price_lf [stock == "AMZN"], aes(x = as.factor(month_price), y = price, color = stock, group = stock))+
  geom_point(shape = 1) + geom_smooth(method = lm) + facet_wrap( ~ year_price, scales = "free_y")+
  labs(x = "Month", y = "Price", title = "Price Distribution for every year by month")
  
  
############ Create and summarize Data: Leverage returns ################
# Let us see how the distribution of continous compound returns across all stocks

ggplot(return_analysis_lf, aes(x = as.factor(year_price), y = cc_return)) + geom_boxplot() +
  labs(x = "Year", y = "Cont Comp Returns", title = "Returns across all stocks")

# Let us add the average on every boxplot
# Flavor 1 - use the underlying dataset
# Flavor 2 - uses the aggregates that are available in a separate data frame and the lines connect dot*

ggplot(return_analysis_lf, aes(x = as.factor(year_price), y = cc_return), group = year_price) + geom_boxplot() +
  stat_summary(fun.y = "mean", colour = "red", geom = "point", size = 5) +
labs(x = "Year", y = "Cont Comp Returns", title = "Returns across all stocks")


ggplot(return_analysis_lf, aes(x = as.factor(year_price), y = cc_return), group = year_price) + geom_boxplot() +
  geom_line(data = return_mean_by_year, aes(y = avg_cc_return, colour = "red", group = 1, size = 1)) +
  geom_point(data = return_mean_by_year, aes(y = avg_cc_return, colour = "blue", group = 1, size = 1)) +
  labs(x = "Year", y = "Cont Comp Returns", title = "Returns across all stocks") +
  theme(legend.position = "none")


# Now lets see if are able to draw some trends
# Notice the use of facet_wrap below

ggplot(return_analysis_lf, aes(x = as.factor(year_price), y = cc_return)) + geom_boxplot() +
  stat_summary(fun.y = "mean", colour = "red", geom = "point", size = 2) +
  facet_wrap(~ month_price, scales = "free_y") + 
  labs(x = "Year", y = "Cont Comp Returns", title = "Returns across all stocks by month") +
  theme(axis.text.x = element_text(angle = 90))

# Below chart provides a comparison of the average monthly return
#The size of the spot is the size of the returns

ggplot(return_analysis_lf, aes(x = as.factor(year_price), y = as.factor(month_price))) +
  geom_point(data = return_mean_by_year_month, aes(size = avg_cc_return)) + 
  scale_size(range = c(1,10)) +
  labs(x = "Year", y = "Month", title = "Comparison of returns") +
  theme(axis.text.x = element_text(angle = 90))


###### Look at distribution of returns for every stock
ggplot(return_analysis_lf, aes(x = as.factor(year_price), y = cc_return)) + geom_boxplot() +
  stat_summary(fun.y = "mean", colour = "red", geom = "point", size = 2) +
  facet_wrap(~ stock, scales = "free_y") + 
  labs(x = "Year", y = "Cont Comp Returns", title = "Returns all years by stock") +
  theme(axis.text.x = element_text(angle = 90))



ggplot(return_analysis_lf, aes(x = as.factor(stock), y = cc_return)) + geom_boxplot() +
  stat_summary(fun.y = "mean", colour = "red", geom = "point", size = 2) +
  facet_wrap(~ year_price, scales = "free_y") + 
  labs(x = "Year", y = "Cont Comp Returns", title = "Returns all years by stock") +
  theme(axis.text.x = element_text(angle = 90))


############################################################
###     Histograms

# Add some lines to show the standar deviation calculated above
# Notice how the distributionof log returns is trending towards the normal curve

ggplot(return_analysis_lf, aes(x = cc_return)) + geom_histogram() +
  geom_vline(data = return_mean_by_year, aes(xintercept = avg_cc_return, color = "red")) +
  geom_vline(data = return_mean_by_year, aes(xintercept = avg_cc_return + sd_cc_return, color = "blue")) +
  geom_vline(data = return_mean_by_year, aes(xintercept = avg_cc_return - sd_cc_return, color = "blue")) +
  facet_wrap(~ year_price, scales = "free_y") +
  labs(x = "Year", y = "Frequency", title = "Histograms of yearly returns showing mean +/- st_dev") +
  theme(axis.text = element_text(angle = 90))


#### Generate some correlation plots

cor(all_price[,-c(9,10),with = F])
pairs(all_price[,-c(9,10),with = F])

#### performanceanalytics package ########## requires time series object
### Generate performance related matrices
## plot the range of prices using boxplot

chart.Boxplot(all_price_xts, legend.loc = "bottom", main= "Price Distribution")


# Compute the wealth index
chart.CumReturns(diff(all_price_xts)/lag.xts(all_price_xts, k=1),
                 legend.loc = "topleft", wealth.index = T,
                 main = "Future Value of $1 invested")

### Plot the range of return using box plot
# notice the noise in the data and the outliers

chart.Boxplot(all_cont_comp_returns_xts, legend.loc = "bottom", main = "Return Distribution")




















  
  
  