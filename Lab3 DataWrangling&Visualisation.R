install.packages( "dplyr" )
library(dplyr)

download.file("https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv", destfile = "C:/Users/aldan/Desktop/Y2S2/ps0002/msleep_ggplot2.csv")
df <- read.csv("C:/Users/aldan/Desktop/Y2S2/ps0002/msleep_ggplot2.csv", header=TRUE)

#df without name, genus, bodywt
sleep1 <-select(df, "vore":"brainwt")

#selecting only omni, carni, herbi (task 1)
sleep1 <- sleep1[sleep1$vore %in% c('omni', 'carni', 'herbi'),]
head(sleep1)

#creating rem% column (task 2)
rem_percentage <- (sleep1$sleep_rem/sleep1$sleep_total)*100
sleep1 <- cbind(sleep1, rem_percentage)
head(sleep1)

#summary statistics. number of mammals, avg brain weight, avg rem sleep% over vore groups (task3)
orderedVores <- sleep1 %>% group_by(vore)
summStats <- summarise(orderedVores, count=n(), avgBrainWt=mean(brainwt,na.rm=TRUE), avgRemPerc=mean(rem_percentage, na.rm=TRUE))
summStats
#In summary for
# 1) Carnivores: 19 of them with average brain weight of 0.793, and average rem sleep percentage of 20.7
# 2) Herbivores: 32 of them with average brain weight of 0.622, with average rem sleep percentage of 14.9
# 3) Omnivores: 20 of them with average brain weight of 0.146, with average rem sleep percentage of 17.9
# Herbivores has the highest number population, followed by omnivores, then carnivores.
# Carnivores has the highest average brain weight, followed by herbivores then significantly dropping in omnivores.
# Carnivores has the highest average rem sleep percentage, followed by omnivores, then herbivores

install.packages("ggplot2")
library(ggplot2)

#data visualisation of rem_percentage with boxplot
ggplot(sleep1, aes(x=vore, y=rem_percentage)) + geom_boxplot()
# Carnivores has the highest median of rem_percentage, followed by omnivores then herbivores. This means most carnivores are able to sleep deeply
# and herbivores have trouble sleeping deeply.
# Carnivores has the largest stretch of whiskers followed by omnivores then herbivores. This means carnivores has a wide range of rem percentages
# while the spread of rem percentages for herbivores are closer to one another.

#analysing relationship between rem sleep percentage and brain weight
ggplot(sleep1, aes(x=brainwt, y=rem_percentage))+geom_point(color = "red")+xlim(0,0.5) + geom_smooth(method="lm")
#large upper bounds (above 1) for brain weight allows outliers to skew geom_smooth upwards, which isn't an accurate representation of the trend of the data.
#0.5 as an upper bound seems to provide a good spread of the scatter plot and shows us that as brain weight increases, the rem percentage decreases

