#' Author: Ted Kwartler
#' Data: 9-2-2020
#' Student:
#' Assignment: EDA, Functions, visuals & mapping
#' Instructions: Complete the scaffolded code for Canvas.
 
## Set the working directory (HINT: it should be your wk2_homework folder)
setwd('/Users/jogardn/git/Harvard_DataMining_Business_Student/HW/HW I')

## Load the libraries, maps ggplot2, ggthemes
library(maps)
library(ggplot2)
library(ggthemes)


## Exercises
# 1. Read in diamonds.csv data and call it 'df', you will need to change the path in quotes to your local machine to use read.csv
df <-read.csv('/Users/jogardn/git/Harvard_DataMining_Business_Student/HW/HW I/diamonds.csv')

# 2. Examine the first 10 rows of data
head(df, 10)

# 3. What is the first value for the 'color' column when looking at head()? 
# Answer: E

# 4. Create a new data frame called 'diamonds' by sorting the 'df' object by price and decreasing is FALSE
diamonds <- df[sort(df$price, decreasing=F),]

# 5. Examine the last 6 rows by calling 'tail()' on the 'diamonds' data frame.  What is the most expensive diamond in the set?
tail(diamonds)
# Answer: 7710

# 6. Copy and paste the results of the 'summary()' stats for the 'caret' attribute below.  You can use either $ or the index to get the vector
summary(diamonds$carat)

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.23    0.71    0.80    0.81    0.98    2.72 


# Introducing additional functions to try out, don't worry these are straight forward:
?max
?min
?range

# 7. What is the maximum value for the "depth" attribute?
max(diamonds$depth)
# Answer: 71.6

# 8. What is the minimum value for the "table" attribute?
min(diamonds$table)
# Answer: 43

# 9. What is the range of values for the "price" column?
range(diamonds$price)
# Answer: 405 7710

# 10. Find the 347th diamond in the data set using row indexing.  Copy paste the single row below.
diamonds[347,]
#      carat   cut color clarity depth table price    x    y    z
#411.1  0.34 Ideal     I     VS1  61.8    55   555 4.48 4.52 2.78

# 11. Create a barplot of the diamonds' cut column.  Using jpeg() as the function create a file "barplot.jpg" then create the bar plot so it is saved to disk.
jpeg("/Users/jogardn/git/Harvard_DataMining_Business_Student/personalFiles/barplot.jpg")
barplot(table(diamonds$cut), main="Diamond Cut", xlab="Cut", ylab="Frequency")
dev.off()


# 12. Create a ggplot scatterplot of points with the following aesthetics:
# color = clarity
# x axis = carat
# y axis = price
# point size size = 0.25
# theme = theme_economist_white()
# legend = none

ggplot(data=diamonds, aes(x=carat, y=price, color=clarity)) + geom_point(size=0.25) + theme_economist_white() + theme(legend.position="none")


# 13. Examine the price distribution by creating a ggplot geom_histogram() instead of a scatter plot layer.  Use the code scaffold below with the following parameters:
# data = diamonds
# type = geom_histogram
# x = price (we are only examining a single attribute here)
# bin width = 100

ggplot(data= diamonds) + geom_histogram(aes(x=price), binwidth=100)


#14. What is the class() of the carat vector?  HINT: apply class() as a function to the carat column using $ or index number
class(diamonds$carat)

"numeric"

#15. What is the class of the color vector?
class(diamonds$color)

"character"

#16. Read in the WesternCellTowers.csv cell towers as westTowers
westTowers <- read.csv("/Users/jogardn/git/Harvard_DataMining_Business_Student/HW/HW I/WesternCellTowers.csv")

#17. Using map() create a state map of 'oregon', 'utah', 'nevada', 'california' & add points() with westtowers$lon,westtowers$lat, col='blue'
map("state", regions=c("oregon", "utah", "nevada", "california"), fill=TRUE, col="lightblue")
points(westTowers$lon, westTowers$lat, col="blue")

#18. Load the county map data called counties (HINT: with map_data)
counties <- map_data('county')

#19. Load the state data called state 
allStates <- map_data('state')

#20. Subset counties and allStates into the objects below; add the last states, california & nevada to the search index
westernCounties <- counties[
  counties$region %in% c("oregon", "utah", "california", "nevada"),
]
westernStates   <- allStates[
  allStates$region %in% c("oregon","utah", "california", "nevada") ,
]

#21. Create a ggplot map of the cell phone towers in the 4 western states.  Refer to the lesson's example code.
ggplot() +
  geom_polygon(data = westernStates, aes(x = long, y = lat, group = group),
               fill = "lightblue", color = "white") +
  geom_point(data = westTowers, aes(x = lon, y = lat), 
             color = "red", size = 2) +
  coord_fixed(1.3) +
  labs(title = "Cell Phone Towers in Western States")

# End
