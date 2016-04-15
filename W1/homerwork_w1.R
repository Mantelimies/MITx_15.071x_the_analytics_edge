data = read.csv("mvtWeek1.csv")
str(data)

max(data$ID)
min(data$Beat)

str(data)
summary(data)

DateConvert = as.Date(strptime(data$Date, "%m/%d/%y %H:%M"))
str(DateConvert)
summary(DateConvert)
