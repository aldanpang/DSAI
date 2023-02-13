library(dplyr)
library(ggplot2)
library(psych)

#1 Creating dataframe with summary statistics
msleep = read.csv("msleep_ggplot2.csv")
sleep_stats = summarise(msleep, count=n(), mean=mean(sleep_total, na.rm=TRUE), sd=sd(sleep_total, na.rm=TRUE), range=max(sleep_total, na.rm=TRUE)-min(sleep_total, na.rm=TRUE)) 

#2 Visualising mean, sd, ranges
msleep%>%group_by(vore)%>%
ggplot(aes(x=vore, y=sleep_total)) + geom_boxplot()

#3 Creating histograms and density plots showing distrubution of total sleep time and rem sleep time
ggplot(msleep, aes(sleep_total))+geom_histogram(bins=20, na.rm=TRUE)
ggplot(msleep, aes(sleep_rem))+geom_histogram(bins=20,na.rm=TRUE)

#3 Density plots
ggplot(msleep, aes(sleep_total))+geom_density()
ggplot(msleep, aes(sleep_rem))+geom_density()

#4 for mammals w brain weight <0.1kg, compare relationship between rem_ratio=sleep_rem/sleep_total
msleep %>% filter(brainwt<0.1) %>%
  mutate(rem_ratio=sleep_rem/sleep_total) %>%
  ggplot(aes(x=brainwt, y=rem_ratio))+geom_point()

