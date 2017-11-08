library(tidyquant)

symbols<-c("GOOG","IBM")

full_df<-NULL #Initialize empty dataset

for (symbol in symbols){

  # pull financials
financial<-tq_get(symbol,get="financial",from="2001-01-01")

# pull prices
stock_price<-tq_get(symbol,get="stock.prices")

# add name
stock_price$name<-symbol

#split day into year, month day

stock_price$year<-year(stock_price$date)
stock_price$month<-month(stock_price$date)
stock_price$day<-day(stock_price$date)

#Convert to average for the month, for merge with quarterly data
stock_price<-stock_price%>%group_by(name,year,month)%>%
  summarize_at(c("open","high","low","close","volume"),mean,na.rm=TRUE)

# Get income statements (IS) others are Balance Sheets (bs) and cash flows (CF)
fin_IS<-financial %>%
  filter(type == "IS") %>%
  select(quarter)%>%
  unnest()

fin_BS<-financial%>%
  filter(type=="BS") %>%
  select(quarter) %>%
  unnest()

fin_CF<-financial%>%
  filter(type=="CF") %>%
  select(quarter) %>%
  unnest()

# combine all financials
fin<-rbind(fin_IS,fin_BS,fin_CF)

#add name of stock
fin$name<-symbol
# Get year for merge
fin$year<-year(fin$date)
# get month for merge
fin$month<-month(fin$date)

#quarterly dataset from financials
q_df<-left_join(fin,stock_price,by=c("name","year","month"))

#Drop any missing
q_df<-q_df%>%filter(!is.na(value))

#Don't need group
q_df<-q_df%>%select(-group)
#Combine with full data frame
full_df<-rbind(full_df,q_df)

}

## Use spread to create new column for all financial chars
full_df2<-full_df%>%
  spread(category,value)

# linear model

reg1<-lm(close~`Total Revenue`,data=full_df2);summary(reg1)
         