'''
1. Size of data is 800 mb, but lots of text data that we may not use
2. Airbnb is the hottest company now, we want to see analyze the data to provide 
customers a view of how to choose host (region, time, room type, etc) based on different 
criterials, reviews(maybe sentiment analysis to see if positive/negative) or price.
  a. Neighbours versus price
  b. Rooms property(number of roon, size, number of baths, etc) versus price
  c. Numebr of reviews (another response variable), when people lived in then they could write review (assume)
  d. Draw map to see price/ distribution
3. This first draft only used listing2 data

'''


library(ggplot2)
library(dplyr)
library(vcd)
library(viridis)
listing2=read.csv('/Users/van/Desktop/EDA_Project/Data/listings-2.csv')
summary(listing2)
#Take a look at histogram for neighbourhood in each group
ggplot(data=listing2,aes(neighbourhood_group))+
  geom_histogram(stat="count")

#Take a look at most freq housings in Brooklyn and Manh
#Too many neightbours, even though we cannot directly see which neighbourhood has higher counts and which one has lowers counts
#But it can tell us the distribution is very imblanced, thus we could assume that using MAPS to represents the counts for each neigh
#would be valuable 
Brooklyn=listing2[listing2$neighbourhood_group=='Brooklyn',]
Manhattan=listing2[listing2$neighbourhood_group=='Manhattan',]
ggplot(data=Brooklyn,aes(neighbourhood))+
  geom_histogram(stat='count')
ggplot(data=Manhattan,aes(neighbourhood))+
  geom_histogram(stat='count')



#Take a look at mean for each neighourhood group
listing2_neigh_group=group_by(listing2,neighbourhood_group)
data_q1=summarise(listing2_neigh_group,mean=mean(price))
ggplot(data=data_q1, aes(x=neighbourhood_group,y=mean))+
  geom_bar(stat='identity')


'''
Take a look at price, very imbalanced
For further analysis, 
step 1. seperate the data to a: symmetric part, 2: extreme part
step 2, analyze each part and see which regions has the extreme high price and figure out why
'''
ggplot(data=listing2,aes(price))+
  geom_density()

#Seperate data manually based on visualziation

data_low_price=listing2[listing2$price<=500,]
data_high_price=listing2[listing2$price>500,]

ggplot(data=data_low_price,aes(price))+
  geom_histogram(binwidth = 10)
ggplot(data=data_high_price,aes(price))+
  geom_histogram(binwidth = 20)
#9999 might be missing value need to figure it out

#take a look at low price versus region
#recode price as a factor variable, 1 is lowest, etc, for first analysis, just pick 50-150 ten levels
data_50_150=listing2[listing2$price<=150,]
data_50_150=data_50_150[data_50_150$price>=50,]
data_50_150$price_level=trunc((data_50_150$price-50)/10)

vcd::mosaic(neighbourhood_group ~ price_level, data = data_50_150,direction = c("v", "h"), 
            main=("Mosaic Plot for Neighborhood Group vs Price Level"),gp = gpar(fill = c("light yellow","grey60")), spacing =
              spacing_highlighting)


a <- group_by(data_50_150,price_level,neighbourhood_group)
b <- summarise(a,mean=mean(reviews_per_month,na.rm=T))
ggplot(b,aes(x=price_level,y=neighbourhood_group,fill=mean))+
  geom_tile()+
  scale_fill_viridis('Average Reviews')+
  labs(title="Price_Level VS Neighbourhood filled by average review",x ="Price Level", y = "Neightbourhood Group")

#Room Type
summary(listing2$room_type)
#room_type price, a little surprised that Private room and shared room has very similar average price
data_room=group_by(listing2,room_type)
room_sum=summarise(data_room,mean=mean(price))
ggplot(data=room_sum, aes(x=room_type,y=mean))+
  geom_bar(stat='identity')

#Nothing surprise :(
data_room_neigh=group_by(listing2,neighbourhood_group,room_type)
a <- group_by(data_room_neigh,neighbourhood_group,room_type)
b <- summarise(a,mean=mean(price,na.rm=T))
ggplot(b,aes(x=neighbourhood_group,y=room_type,fill=mean))+
  geom_tile()+
  scale_fill_viridis('Average Price')+
  labs(title="Neighbourhood VS Room Type filled by average price",x ="neighbourhood_group", y = "Room Type")



#minimum_nights versus price, only look at short term renting, less than 30 days
#imbalanced data, thus the line may not be reliable
data_min=listing2[listing2$minimum_nights<=30,]
a=group_by(data_min,minimum_nights)
b <- summarise(a,mean=mean(price,na.rm=T))
ggplot(data=b,aes(minimum_nights,mean))+
  geom_line()
ggplot(data=a,aes(minimum_nights))+
  geom_histogram()


#minimum_night and price_level filled by average reviews
data_min_night=group_by(data_50_150,minimum_nights,price_level)
a <- group_by(data_min_night,minimum_nights,price_level)
b <- summarise(a,mean=mean(reviews_per_month,na.rm=T))
ggplot(b,aes(x=minimum_nights,y=price_level,fill=mean))+
  geom_tile()+
  scale_fill_viridis('Average Review')+
  labs(title="Min Nights VS Price Level filled by average Reviews",x ="Min Nights", y = "Price Level")










