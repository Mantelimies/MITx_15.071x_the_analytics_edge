### First Assignment ###
rm (list = ls(all=T))

mvt = read.csv("mvtWeek1.csv")
str(mvt)

max(mvt$ID)
min(mvt$Beat)

str(mvt)
summary(mvt)

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
str(DateConvert)
summary(DateConvert)


# add months and weekdays

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

# check time related mins and maxs
table(mvt$Month)
min(table(mvt$Month))

table(mvt$Weekday)
max(table(mvt$Weekday))

a <- table(mvt$Month, mvt$Arrest)
a[which.max(a[,2]),]


# start creating plots

hist(mvt$Date, breaks=100)
boxplot(mvt$Date, mvt$Arrest)
install.packages("ggplot")


#2001 arrest proportion
table(mvt$Year, mvt$Arrest)[1,2]/sum(table(mvt$Year, mvt$Arrest)[1,])


#2007 arrest proportion
prop.table(table(mvt$Arrest[mvt$Year == "2007"]))

#2012 arrest proportion
prop.table(table(mvt$Arrest[mvt$Year == "2012"]))

#Popular locations
sort(table(mvt$LocationDescription), decreasing = T)


#Top5 locations
Top5 = subset(mvt, LocationDescription == "STREET" | LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription == "ALLEY"| LocationDescription == "GAS STATION" | LocationDescription == "DRIVEWAY - RESIDENTIAL")
str(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
str(top5)

max(prop.table(table(Top5$LocationDescription, Top5$Arrest))[,2])

#Most thefts on which weekday
which.max(table(Top5$LocationDescription, Top5$Weekday)["GAS STATION",])

#Least thefts on driveways
which.min(table(Top5$LocationDescription, Top5$Weekday)["DRIVEWAY - RESIDENTIAL",])


### Second Assignment ###

