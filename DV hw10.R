setwd("C:/Users/avman/OneDrive/Desktop/Time Series/TSF excel files")
suic = read.csv("DV suic data.csv")

sum(is.na(suic))

suic = suic[,-9]
sum(is.na(suic[,9]))

mean(suic$suicides_no, na.rm = TRUE)
median(suic$suicides_no) 
range(suic$suicides_no)  
quantile(suic$suicides_no)

newdata = subset(suic , suicides_no <1000 & year >1999 & ï..country != c("United States", " Russian Federation"))

boxplot(suicides_no ~ year , data=suic, xlab="Years", ylab="Suicides")
boxplot(sum(suicides_no) ~ year , data=newb1, xlab="Years", ylab="Suicides")

par(las=2)
bp = boxplot(suicides_no ~ ï..country , data=suic, xlab="Countries", ylab="Suicides", show.names = F)
axis(1, at=seq(length(bp$names)),
     labels=bp$names,
     cex.axis=0.50)

par(las=2)
bp1 = boxplot(suicides_no ~ ï..country , data=newdata, xlab="Countries", ylab="Suicides", show.names = F)
axis(1, at=seq(length(bp$names)),
     labels=bp$names,
     cex.axis=0.5)

names(suic)

hist(suic$suicides_no)

library(dplyr)
?group_by

bycountry1 = group_by(suic, ï..country)
newb2=summarize(bycountry1, sum(suicides_no), sum(population))
newb3 = as.data.frame(newb2)
plot(newb3$`sum(population)`,newb3$`sum(suicides_no)`)


bycountry = group_by(newdata, ï..country)
newb=summarize(bycountry, sum(suicides_no), sum(population), sum(suicides.100k.pop))
class(newb)
newb1 = as.data.frame(newb)
plot(newb1$`sum(population)`,newb1$`sum(suicides_no)`, xlab = "Population", ylab = "Suicides", main = "Realtion of Suicides Vs Population")

cor(suic[,5:6])


max(newb1$`sum(suicides.100k.pop)`)
match(7660.64,newb1$`sum(suicides.100k.pop)`)
newb1$ï..country[52]

bysex = group_by(newdata, sex)
newb5=summarize(bysex, mean(suicides_no), mean(suicides.100k.pop))
newb6 = as.data.frame(newb5)
barplot(newb6$`mean(suicides.100k.pop)`, main = "Suicides per 100k population by gender", xlab = "Gender", ylab = "Suicides per 100k")

hist(newb1$`sum(suicides_no)`, xlab = "Total suicides", ylab = "No. of countries", main = "Barplot of no. of countries by total suicides")
boxplot(newb1$`sum(suicides_no)`, main = "Boxplot for Total Suicides by Countries")
