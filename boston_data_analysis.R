#Set working directory
setwd("C:/Users/dnehls/Documents/data_analysis_teaching_presentation")

#Load data from csv file
boston <- read.table(
  file = "tomslee_airbnb_boston_1429_2017-07-10.csv",
  sep = ",",
  header = TRUE,
  fill = TRUE
)

#Univariate statistics for qualitative variables

#what is the most popular room type in Boston?
table(boston$room_type)
plot(
  x = boston$room_type, 
  ylim = c(0, 3100),
  main = "Room Type Frequency in Boston"
)

#what is the room distribution across each neighborhood?
table(boston$neighborhood)
plot(
  boston$neighborhood, 
  ylim = c(0, 500), 
  las = 3,
  ylab = "Room Quantity",
  main="Room Quantity by Neighborhood"
)


#Univariate statistics for quantitative variables

#How many reviews were submitted in Boston?
sum(boston$reviews)

#Bivariate statistics for both a quantitative and qualitative variable

#per neighborhood?
tapply(boston$reviews, boston$neighborhood, FUN = sum)
barplot(
  tapply(boston$reviews, boston$neighborhood, FUN = sum),
  las = 2,
  ylim = c(0, 14000),
  main = "Review Quantity by Neighborhood"
)

#Sort and Subset operations on datasets

#Find the most active airbnb neighborhoods

#look at the top five neighborhoods by room amount
neighborhoods_by_rooms <- 
head(
  sort(
    table(boston$neighborhood), 
    decreasing = TRUE
  ), 
  n = 5
)

#look at the top five neighborhoods by review amount
neighborhoods_by_reviews <- 
head(
  sort(
    tapply(boston$reviews, boston$neighborhood, FUN = sum), 
    decreasing = TRUE
  ), 
  n =5
)

#Set operations

#combine these lists to get the most "active" neighborhoods
active_neighborhoods <- 
  intersect(names(neighborhoods_by_reviews), names(neighborhoods_by_rooms))

#inspect the active neighborhoods
active_neighborhoods

#univariate statistics of quantitative variable

#look at review result distribution for Boston
summary(boston$overall_satisfaction[boston$reviews > 0])
sd(boston$overall_satisfaction[boston$reviews > 0])
which.max(table(boston$overall_satisfaction[boston$reviews > 0]))
#the median is slightly more than the mean, which means the shape of distribution 
#of values will be negatively skewed, that is left skewed.
#sd is large relative to the 5-point scale. large spread.
#from 1st and 3rd quantile, we can see that the middle 50% of 
#reviews fall between 4.5-5.0 range.

#visualization of the review distribution
hist(
  x = boston$overall_satisfaction[boston$reviews > 0],
  breaks = 20,
  xlab = "Review Results (out of 5.0)",
  main = "Histogram of Review Results in Boston",
  ylim = c(0, 2000)
)

#review distribution for active neighborhoods
par(mfrow = c(2, 2))
for(nb in active_neighborhoods) {
  print(nb)
  print(
    paste("total reviews submitted: ", 
              sum(boston$reviews[boston$neighborhood == nb])
    )
  )
  print(summary(boston$overall_satisfaction[boston$reviews > 0 
                                      & boston$neighborhood==nb]))
  hist(
    x = boston$overall_satisfaction[boston$reviews > 0 
                                    & boston$neighborhood==nb],
    ylim = c(0, 200),
    xlab = "Review Results",
    main = paste(nb, " Review Results")
  )
}

#compare review result distribution btwn boston and a neighborhood
par(mfrow = c(1, 2))
hist(
  x = boston$overall_satisfaction[boston$reviews > 0],
  breaks = 20,
  xlab = "Review Results (out of 5.0)",
  main = "Review Results in Boston",
  ylim = c(0, 2000)
)
hist(
  x = boston$overall_satisfaction[boston$reviews > 0
                                  & boston$neighborhood=="Jamaica Plain"],
  ylim = c(0, 200),
  xlab = "Review Results",
  main = "Jamaica Plain Review Results"
)


#look at the price distributions for best reviewed neighborhoods
summary(boston$price[boston$neighborhood=="Jamaica Plain" & 
                     boston$room_type=="Entire home/apt"])
sd(boston$price[boston$neighborhood=="Jamaica Plain"& 
                     boston$room_type=="Entire home/apt"])

summary(boston$price[boston$neighborhood=="South End" & 
                     boston$room_type=="Entire home/apt"])
sd(boston$price[boston$neighborhood=="South End"& 
                     boston$room_type=="Entire home/apt"])

#Bivariate statistics for two quantitative variables

#correlation between price and review results in these neighborhoods?
cor(boston$price[boston$reviews > 0 & 
                   boston$neighborhood=="Jamaica Plain" &
                   boston$room_type=="Entire home/apt"],
    boston$overall_satisfaction
    [boston$reviews > 0 & 
        boston$neighborhood=="Jamaica Plain" &
        boston$room_type=="Entire home/apt"])

cor(boston$price[boston$reviews > 0 & 
                   boston$neighborhood=="South End" &
                   boston$room_type=="Entire home/apt"],
    boston$overall_satisfaction
    [boston$reviews > 0 & 
        boston$neighborhood=="South End" &
        boston$room_type=="Entire home/apt"])
#result close to 0 means no correlation. 

#price of Entire home/apt across the best reviewed neighborhoods
par(mfrow = c(1, 2))
hist(
  x=boston$price[boston$room_type=="Entire home/apt" &
                   boston$neighborhood=="South End"],
  xlab = "price",
  breaks = 20,
  main = "South End Entire home/apt",
  ylim = c(0, 100)
)

hist(
  x=boston$price[boston$room_type=="Entire home/apt" & 
                   boston$neighborhood=="Jamaica Plain"],
  xlab = "price",
  breaks = 20,
  main = "Jamaica Plain Entire home/apt",
  ylim = c(0, 60)
)

max(boston$price[boston$room_type=="Entire home/apt" & 
                   boston$neighborhood=="Jamaica Plain"])

max(boston$price[boston$neighborhood=="South End" & 
                   boston$room_type=="Entire home/apt"])
