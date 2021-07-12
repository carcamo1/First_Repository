install.packages("readxl")
library(readxl)
nfl <- read_excel(file.choose("NFL Combine Data.xlsx"))
#Had trouble subsetting the vertical leap column so I renamed it
names(nfl)[names(nfl) =="Vert Leap (in)"] <- "vert_leap"
#Remove all na's in vertical leap
nfl1 <- subset(nfl, subset = vert_leap > 0)
#subset so we are only looking at the five desired positions
nfl1 <- subset(nfl1, subset = POS == 'RB' | POS== 'WR'| POS == 'TE'| POS == 'FS' | POS== 'SS')
nfl1[1231,1] = 2005
#Summary stats for the position variable
length(nfl1$POS)
table(nfl1$POS)
prop.table(table(nfl1$POS))
#summary stats for year
summary(nfl1$Year)
sd(nfl1$Year)
#Height
summary(nfl1$`Height (in)`)
sd(nfl1$`Height (in)`)
#Weight
summary(nfl1$`Weight (lbs)`)
sd(nfl1$`Weight (lbs)`)
#Vertical Leap
summary(nfl1$vert_leap)
sd(nfl1$vert_leap)
#Multiple Linear Regression
reg<-lm(vert_leap ~ `Height (in)`+`Weight (lbs)`  + Year + POS, data=nfl1)
summary(reg)
#stargazer table
install.packages("stargazer")
library(stargazer)
stargazer(reg, type='html', out='regression.html')
