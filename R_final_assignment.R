#1.How many records are in the dataset?
diversity <- read.csv("cave_vertebrate_biodiversity_data.csv")
nrow(diversity)
#Ans : 1302

#2.What is the year of the earliest survey, and what is the year of the latest survey in this dataset?
library(tidyverse)
cave <- read_csv("cave_vertebrate_biodiversity_data.csv")
cave2 <- mutate(cave,Date=as.Date(Date,format="%m/%d/%Y"))
min(cave2$Date)
max(cave2$Date)
#Ans : 2004-04-06, 2014-12-30

#3.What is the class of the associated object for this dataset? Note that the class of the object may vary depending on how you loaded the dataset into R.
class(diversity)
#Ans : data frame

#4.What is the species richness of all vertebrates during the entire study?
length(unique(diversity$Species))
#Ans : 73

#5.How many classes of vertebrates are represented in the entire dataset?
length(unique(diversity$Class))
#Ans : 6

#6.What is the species richness of mammals (class Mammalia) were observed during the entire study?
mammalia <- filter(diversity, Class=="Mammalia")
length(unique(mammalia$Species))
#Ans : 16

#7.What is the species richness of salamanders (order Caudata) were observed in 2006?
salamanders <- filter(cave2, Order=="Caudata")
salamanders_2006 <- filter(salamanders, Date>="2006-01-01"&Date<="2006-12-31")
length(unique(salamanders_2006$Species))
#Ans : 10

#8.Which county in Tennessee had the greatest species richness during the entire study?
TN <- filter(cave2, State=="TN")
TN_county <- group_by(TN, County) %>% 
  summarize(count = n_distinct(Species))
print(TN_county, n=38)
top_n(TN_county, 1)
#Ans : Marion    33

#9.How many caves were surveyed from Alabama during the study?
AL <- filter(diversity, State=="AL")
length(unique(AL$Cave))
#Ans : 28

#10.How many caves was the Southern Cavefish (Typhlichthys subterraneus) found in? What was the mean number (and standard deviation) of individuals observed during a single survey for this species?
cavefish <- filter(diversity, Species=="Typhlichthys subterraneus")
length(unique(cavefish$Cave))
cavefish$No_observed <- as.numeric(as.character(cavefish$No_observed))
mean(cavefish$No_observed)
sd(cavefish$No_observed)
#Ans : caves 53, mean 9.97, stdev 16.43

#11.Create a bar plot depicting the 10 counties with the greatest vertebrate species richness. Be sure to appropriately label all axes.
county <- group_by(cave2, County) %>% 
  summarize(count = n_distinct(Species))
print(county, n=44)
county_max <- top_n(county, 10)
#Ans :  Cannon, Coffee, DeKalb, Franklin, Grundy, Jackson, Marion, Marshall, Montgomery, Warren 
library(ggplot2)
ggplot(county_max, aes(County,count)) + geom_bar(stat = "identity") + ylab("Species richness") +ggtitle("Counties with greatest species richness")

#12.Create a plot showing the change in species richness detected by year over the duration of the study.
library(lubridate)
cave_by_year <- mutate(cave2, Year=as.integer(year(Date)))
year_grp <- group_by(cave_by_year, Year) %>% 
  summarize(count = n_distinct(Species))
print(year_grp, n=9)
#Ans : 2004 24, 2005 37, 2006 36, 2007 28, 2008 32, 2009 6, 2012 18, 2013 41, 2014 26
ggplot(year_grp, aes(Year,count)) + geom_line() + geom_point() + ylab("Species richness") +ggtitle("Change in species richness")

#13.Load the BCI dataset and determine the number of rows and columns of this dataset
library(vegan)
data(BCI)
nrow(BCI)
ncol(BCI)
#Ans : row 50, col 225

#14.What is the total number of individuals of the tree Poulsenia armata in the 50-hectare plot?
sum(BCI$Poulsenia.armata)
#Ans : 755

#15.Using built-in functions in vegan, what is the mean and median Simpson’s Index for a hectare on Barro Colorado Island?
simp <- diversity(BCI, "simpson")
mean(simp)
median(simp)
#Ans: mean 0.959, median 0.9657

#16.Using built-in functions in vegan, what is the range and mean Shannon’s Index for a hectare on Barro Colorado Island?
shan <- diversity(BCI, "shannon")
range(shan)
mean(shan)
#Ans : range 2.642-4.077, mean 3.821

#17.Generate a species-area curve for BCI dataset. Be sure to label your axes. Hint: look into the specaccum function.
sp1 <- specaccum(BCI)
plot(sp1,ci.type="polygon", col="blue", lwd=2, ci.lty=0, ci.col="lightblue", ylab = "Species richness")
