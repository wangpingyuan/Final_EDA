# Intuition for this part
# Already did room property, neighbour hood, relationship to price
# Now want to see relationship with rating scores. See below for details

library(ggplot2)
library(dplyr)
library(vcd)
library(viridis)
listing2=read.csv('/Users/van/Desktop/EDA_Project/listings-2.csv')
listing_all=read.csv('/Users/van/Desktop/EDA_Project/listings.csv')
listing=merge(listing2,listing_all,by="id")

#1 Price_Level VS Neighbourhood Filled by Average Rating
#only price from 50-150
data_50_150=listing[listing$price.x<=150,]
data_50_150=data_50_150[data_50_150$price.x>=50,]
data_50_150$price_level=trunc((data_50_150$price.x-50)/10)
a <- group_by(data_50_150,price_level,neighbourhood_group)
b <- summarise(a,mean=mean(review_scores_rating,na.rm=T))
ggplot(b,aes(x=price_level,y=neighbourhood_group,fill=mean))+
  geom_tile()+
  scale_fill_viridis('Average Rating Scores')+
  labs(title="Price_Level VS Neighbourhood Filled by Average Rating",x ="Price Level", y = "Neightbourhood Group")

#2 host_response_time versus average rating scores
#change empty entry to 'N/A'
listing[,41] <- sub("^$", "N/A", listing[,41])
a <- group_by(listing,host_response_time)
b <- summarise(a,mean=mean(review_scores_rating,na.rm=T))

ggplot(data=b, aes(x=host_response_time, y=mean-90)) +
  geom_bar(colour="black", stat="identity")+
  labs(title="Bar Plot for Host Response Time versus Average Rating - 90",x ="Host Response Time", y = "Rating Score")


#3 Cancellation versus rating scores

#only one with long_term, and it's NA, remove this category
a <- group_by(listing,cancellation_policy)
b <- summarise(a,mean=mean(review_scores_rating,na.rm=T))
b=b[-2,]

ggplot(data=b, aes(x=cancellation_policy, y=mean-90)) +
  geom_bar(colour="black", stat="identity")+
  labs(title="Bar Plot for Cancellation Policy versus Average Rating - 90",x ="Cancellation", y = "Rating Score")


#4 host_Response rate versus review_scores_Rating
listing$host_response_rate=as.numeric(sub("%", "", listing$host_response_rate))
a=c("host_response_rate","review_scores_rating")
b=listing[,a]
b <- na.omit(b)
ggplot(data=b, aes(x=scale(host_response_rate), y=scale(review_scores_rating))) +
  geom_point(colour="black")

#5 price of renting versus review_scores_rating
a=c("price.x","review_scores_rating")
b=data_50_150[,a]
b <- na.omit(b)
ggplot(data=b, aes(x=price.x, y=review_scores_rating)) +
  geom_point(colour="black",alpha=.2)

# price changes as date changes
calender=read.csv('/Users/van/Desktop/EDA_Project/calendar.csv')
name=c("date",'price')
calender=calender[name]
calender <- na.omit(calender)
calender$price <- as.numeric(gsub('[$,]', '', calender$price))

a <- group_by(calender,date)
b <- summarise(a,mean=mean(price,na.rm=T))

library(dygraphs)
library(xts)

b$date=xts(as.factor(b$date))
b$date <- as.Date(as.character(b$date),format="%Y-%m-%d")
b <- xts(b$mean,b$date)
dygraph(b)




