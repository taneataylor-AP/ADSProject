library(tabulizer)
library(pdftools)
library(glue)
library(dplyr)
library(magrittr)
library(ggthemes)
library(tidyverse)
library(shiny)
library(miniUI)
library(ggplot2)
library(reshape)
library(readxl)
library(selectr)
library(XML)
library(xml2)
library(rvest)
library(taRifx)
require(gdata)

# Geographical Data
geoDataSite <- "https://census.gov/content/dam/Census/library/working-papers/2002/demo/POP-twps0056.pdf"
# data we need is on pages 30-33
## command below was used for data scraping but only needed once in order to 
## get the coordinates for the area needed.
# f <- locate_areas("https://census.gov/content/dam/Census/library/working-papers/2002/demo/POP-twps0056.pdf", pages = 30:33)
tab <- extract_tables(geoDataSite, output = "data.frame",
                                 pages = c(30,31,32,33), 
                                 area = list(c(51,42,395,294), #northeast
                                             c(49,40,393,290), #NorthCentral
                                             c(53,42,401,292), #south
                                             c(53,38,355,294)),#west
                                 guess = FALSE)

names(tab)[1] <- 'Northeast Region'
names(tab)[2] <- 'NorthCentral Region'
names(tab)[3] <- 'South Region'
names(tab)[4] <- 'West Region'

#deleting irrelevant columns
NERegion <- tab$`Northeast Region`$X <- NULL
NERegion <- tab$`Northeast Region`$White <- NULL
NERegion <- tab$`Northeast Region`[-c(3,5,7,8,12), ]

NCRegion <- tab$`NorthCentral Region`$X <- NULL
NCRegion <- tab$`NorthCentral Region`$White <- NULL
NCRegion <- tab$`NorthCentral Region`[-c(3,5,7,8,12), ]

SRegion <- tab$`South Region`$X <- NULL
SRegion <- tab$`South Region`$White <- NULL
SRegion <- tab$`South Region`[-c(3,5,7,8,12), ]

WRegion <- tab$`West Region`$X <- NULL
WRegion <- tab$`West Region`$White <- NULL
WRegion <- tab$`West Region`[-c(3,5,7,8,13), ]

# 1910 - 1940 Migration
# plot with a line graph with dif lines for each region
NEFirstMigratation <- NERegion[10:7, ]
NCFirstMigration <- NCRegion[10:7, ]
SFirstMigration <- SRegion[10:7, ]
WFirstMigration <- WRegion[11:8, ]
# merge data frames 
temp <- merge(NEFirstMigratation, NCFirstMigration, by = 'Census.year.Total.population', all=TRUE)
temp2 <- merge(SFirstMigration, WFirstMigration, by = 'Census.year.Total.population', all=TRUE)                    
temp$Census.year.Total.population[4] <- "1940 ............................" #original table had subscript next to 1940 so change it to only "1940"
temp2$Census.year.Total.population[4] <- "1940 ............................"
temp2$Census.year.Total.population[5] <- "1940 ............................"
temp2 <- temp2 %>% 
  group_by(Census.year.Total.population) %>% 
  summarise_all(na.omit)
FirstMigration <- merge(temp, temp2, by = 'Census.year.Total.population', all=TRUE)
#clean up
names(FirstMigration)[1] <- 'Year'
names(FirstMigration)[2] <- 'Northeast'
names(FirstMigration)[3] <- 'NorthCentral'
names(FirstMigration)[4] <- 'South'
names(FirstMigration)[5] <- 'West'
#plot
FirstMigration <- FirstMigration[order(FirstMigration$Year), ]
Molten <- melt(FirstMigration, id.vars = 'Year')
ggplot(Molten, aes(x=Year, y = value, colour = variable)) + geom_point() + geom_line()

## Summary Stats
FirstMigration$Year <- as.numeric(gsub(' ...........................','',FirstMigration$Year))
FirstMigration$Northeast <- as.numeric(gsub(' ','',FirstMigration$Northeast))
FirstMigration$`NorthCentral` <- as.numeric(gsub(' ','',FirstMigration$`NorthCentral`))
FirstMigration$West <- as.numeric(gsub(' ','',FirstMigration$West))
FirstMigNoS <- data.frame(FirstMigration$Northeast,FirstMigration$`NorthCentral`,FirstMigration$West)
FirstMigration$South <- as.numeric(gsub(' ','',FirstMigration$South))

## a. Mean
# FirstMigration %>% group_by(Year) %>% summarise(mean(Northeast))
totalMean <- FirstMigration%>%
  group_by(Year)%>% 
  summarise(Mean=mean(FirstMigration))
totalMean <- colMeans(FirstMigration)
totalMean <- totalMean[-c(1)]
print(totalMean)

## b. Mode
totalMode <- names(table(FirstMigration))[table(FirstMigration)==max(table(FirstMigration))]
print(totalMode)

## c. Median
NEMedian <- sort(FirstMigration$Northeast)
NCMedian <-sort(FirstMigration$`NorthCentral`)
SMedian <- sort(FirstMigration$South)
WMedian <- sort(FirstMigration$West)
NEMedian <- median(NEMedian)
NCMedian <- median(NCMedian)
SMedian <- median(SMedian)
WMedian <- median(WMedian)

MedianbyRegion <- data.frame(NEMedian, NCMedian, WMedian, SMedian)
print(MedianbyRegion)
### Correlation
firstCor <- cor(FirstMigration)
print(firstCor)

#### Linear Regression
linReg <- lm(Year ~ Northeast + NorthCentral + South + West, data = FirstMigration)
print(linReg)

# 1940 - 1970 Migration
NEScndMigratation <- NERegion[7:4, ]
NCScndMigration <- NCRegion[7:4, ]
SScndMigration <- SRegion[7:4, ]
WScndMigration <- WRegion[8:4, ]

SScndMigration$Census.year.Total.population[1] <- "1940 ............................"
WScndMigration <- WScndMigration[-c(3), ] #omit second census from 1960
WScndMigration$Census.year.Total.population[1] <- "1940 ............................"
WScndMigration$Census.year.Total.population[3] <- "1960 ............................"
# merge data frames 
temp3 <- merge(NEScndMigratation, NCScndMigration, by = 'Census.year.Total.population', all=TRUE)
temp3$Census.year.Total.population[1] <- "1940 ............................"
temp4 <- merge(SScndMigration, WScndMigration, by = 'Census.year.Total.population', all=TRUE)        
SecondMigration <- merge(temp3, temp4, by = 'Census.year.Total.population', all=TRUE)
#clean up
names(SecondMigration)[1] <- 'Year'
names(SecondMigration)[2] <- 'Northeast'
names(SecondMigration)[3] <- 'NorthCentral'
names(SecondMigration)[4] <- 'South'
names(SecondMigration)[5] <- 'West'
#plot
SecondMigration <- SecondMigration[order(SecondMigration$Year), ]
Molten2 <- melt(SecondMigration, id.vars = 'Year')
ggplot(Molten2, aes(x=Year, y = value, colour = variable)) +  geom_point() + geom_line()

## Summary Stats
SecondMigration$Year <- as.numeric(gsub(' ...........................','',SecondMigration$Year))
SecondMigration$Northeast <- as.numeric(gsub(' ','',SecondMigration$Northeast))
SecondMigration$`NorthCentral` <- as.numeric(gsub(' ','',SecondMigration$`NorthCentral`))
SecondMigration$West <- as.numeric(gsub(' ','',SecondMigration$West))
SecondMigNoS <- data.frame(SecondMigration$Northeast,SecondMigration$`NorthCentral`,SecondMigration$West)
SecondMigration$South <- as.numeric(gsub(' ','',SecondMigration$South))

## a. Mean
totalMean2 <- SecondMigration%>%
  group_by(Year)%>% 
  summarise(Mean=mean(SecondMigration))
totalMean2 <- colMeans(SecondMigration)
totalMean2 <- totalMean2[-c(1)]
print(totalMean2)
## b. Mode
totalMode2 <- names(table(SecondMigration))[table(SecondMigration)==max(table(SecondMigration))]
print(totalMode2)

## c. Median
NEMedian2 <- sort(SecondMigration$Northeast)
NCMedian2 <-sort(SecondMigration$`NorthCentral`)
SMedian2 <- sort(SecondMigration$South)
WMedian2 <- sort(SecondMigration$West)
NEMedian2 <- median(NEMedian2)
NCMedian2 <- median(NCMedian2)
SMedian2 <- median(SMedian2)
WMedian2 <- median(WMedian2)

MedianbyRegion2 <- data.frame(NEMedian2, NCMedian2, WMedian2, SMedian2)
print(MedianbyRegion2)
### Correlation
secondCor <- cor(SecondMigration)
print(secondCor)

#### Linear Regression
linReg2nd <- lm(Year ~ Northeast + NorthCentral + South + West, data = SecondMigration)
print(linReg2nd)

# 1970 - 1990/Today's Migration
NEThrdMigratation <- NERegion[4:2, ]
NCThrdMigration <- NCRegion[4:2, ]
SThrdMigration <- SRegion[4:2, ]
WThrdMigration <- WRegion[4:2, ]
#merge
temp5 <- merge(NEThrdMigratation, NCThrdMigration, by = 'Census.year.Total.population', all=TRUE)
temp6 <- merge(SThrdMigration, WThrdMigration, by = 'Census.year.Total.population', all=TRUE)        
ThirdMigration <- merge(temp5, temp6, by = 'Census.year.Total.population', all=TRUE)
#clean up
names(ThirdMigration)[1] <- 'Year'
names(ThirdMigration)[2] <- 'Northeast'
names(ThirdMigration)[3] <- 'NorthCentral'
names(ThirdMigration)[4] <- 'South'
names(ThirdMigration)[5] <- 'West'

#plot
ThirdMigration <- ThirdMigration[order(ThirdMigration$Year), ]
Molten3 <- melt(ThirdMigration, id.vars = 'Year')
ggplot(Molten3, aes(x=Year, y = value, colour = variable)) + geom_line()

## Summary Stats
ThirdMigration$Year <- as.numeric(gsub(' ...........................','',ThirdMigration$Year))
ThirdMigration$Northeast <- as.numeric(gsub(' ','',ThirdMigration$Northeast))
ThirdMigration$`NorthCentral` <- as.numeric(gsub(' ','',ThirdMigration$`NorthCentral`))
ThirdMigration$West <- as.numeric(gsub(' ','',ThirdMigration$West))
ThirdMigNoS <- data.frame(ThirdMigration$Northeast,ThirdMigration$`NorthCentral`,ThirdMigration$West)
ThirdMigration$South <- as.numeric(gsub(' ','',ThirdMigration$South))

## a. Mean
totalMean3 <- ThirdMigration%>%
  group_by(Year)%>% 
  summarise(Mean=mean(ThirdMigration))
totalMean3 <- colMeans(ThirdMigration)
totalMean3 <- totalMean3[-c(1)]
print(totalMean3)

## b. Mode
totalMode3 <- names(table(SecondMigration))[table(SecondMigration)==max(table(SecondMigration))]
print(totalMode3)

## c. Median
NEMedian3 <- sort(SecondMigration$Northeast)
NCMedian3 <-sort(SecondMigration$`NorthCentral`)
SMedian3 <- sort(SecondMigration$South)
WMedian3 <- sort(SecondMigration$West)
NEMedian3 <- median(NEMedian3)
NCMedian3 <- median(NCMedian3)
SMedian3 <- median(SMedian3)
WMedian3 <- median(WMedian3)

MedianbyRegion3 <- data.frame(NEMedian3, NCMedian3, WMedian3, SMedian3)
print(MedianbyRegion3)
### Correlation
thirdCor <- cor(ThirdMigration)
print(thirdCor)

#### Linear Regression
linReg3rd <- lm(Year ~ Northeast + NorthCentral + South + West, data = ThirdMigration)
print(linReg3rd)

# Economic Data
# 1910-1940 Migration
# Mean Annual Earnings of Wage and Salary Workers 1879-1939
firstData <- 'http://www.csun.edu/~hfeco002/black%20white%20income%20gap.pdf'
firstData <- extract_tables(firstData, output = "character",
                      pages = c(15:15), 
                      area = list(c(168, 271, 184, 307)),
                      guess = FALSE)
firstData <- destring(firstData)
#f <- locate_areas(firstData, pages = 15:15)

txt <- extract_tables('/cloud/project/AA20thCen.pdf')
txt <- txt[4]
MaleData <- cbind(txt[[1]][2,2]) %>% data.frame(stringsAsFactors = FALSE)
FemaleData <- cbind(txt[[1]][2,5]) %>% data.frame(stringsAsFactors = FALSE)
combinedData <- rbind(MaleData[1,1],FemaleData[1,1])
combinedData <- melt(list(MaleData, FemaleData))
combinedData <- destring(combinedData[1:2,1])
combinedData <- sum(combinedData[1], combinedData[2])
FirstMgEco <- matrix(c(firstData, combinedData))
colnames(FirstMgEco) <- c('Black')
rownames(FirstMgEco) <- c('1879', '1939')
FirstMgEco <- as.table(FirstMgEco)

#plot
FirstMgEco <- as.data.frame(FirstMgEco)
FirstMgEco <- FirstMgEco[,-c(2)]
plot(FirstMgEco)

## Summary Stats
FirstMgEco$Var1 <- as.numeric(gsub(' ','',FirstMgEco$Var1))
FirstMgEco$Freq <- as.numeric(gsub(' ','',FirstMgEco$Freq))

## a. Mean
totalMean4 <- FirstMgEco%>%
  group_by(Var1)%>% 
  summarise(Mean=mean(FirstMgEco))
totalMean4 <- colMeans(FirstMgEco)
totalMean4 <- totalMean4[-c(1)]
print(totalMean4)

## b. Mode
totalMode4 <- names(table(FirstMgEco))[table(FirstMgEco)==max(table(FirstMgEco))]
print(totalMode4)

## c. Median
FirEcoMed <- sort(FirstMgEco$Freq)
FirEcoMed <- median(FirEcoMed)
print(FirEcoMed)

### Correlation
fourthCor <- cor(FirstMgEco)
print(fourthCor)

#### Linear Regression
linReg4th <- lm(Var1 ~ Freq, data = FirstMgEco)
print(linReg4th)

#1940-1970
#f <- locate_areas('/cloud/project/p04-converted.pdf', pages = 7)
df <- extract_tables('/cloud/project/p04-converted.pdf', #from census.gov
                     output = "matrix",
                     pages = 7,
                     area = list(c(15, -3, 361, 483)),
                     guess = FALSE)

df <- df[[1]]
df[23,1] <- '1949'
df[20,1] <- '1952'
df[11,1] <- '1961'
df[10,1] <- '1962'
df[7,1] <- '1965'
df[6,1] <- '1966'
df[5,1] <- '1967'
df[1,1] <- '1971'
df[24,2] <- '0'
df[23,2] <- '0'
df[22,2] <- '0'
df[21,2] <- '0'
df[20,2] <- '0'
df <- type.convert(df)
# df[24,1] df[2,1]
df <- data.frame('Year' = df[24:2,1], 'Number with income' = df[24:2,2],
                 'Median-Current dollars' = df[24:2,3], 'Median-2018 dollars' = df[24:2,4],
                 'Mean-Current dollars' = df[24:2,5], 'Mean-2018 dollars' = df[24:2,6])
#df <- as.data.frame(df)

#plot
df <- df[order(df$Year), ]
Molten4 <- melt(df, id.vars = 'Year')
ggplot(Molten4, aes(x=Year, y = value, colour = variable)) + geom_point() + geom_line()

## Summary Stats

df$Year <- as.numeric(gsub(' ',',',df$Year))
df$Number.with.income <- extract_numeric(df$Number.with.income)
df$Median.Current.dollars <- extract_numeric(df$Median.Current.dollars)
df$Median.2018.dollars <- extract_numeric(df$Median.2018.dollars)
df$Mean.Current.dollars <- extract_numeric(df$Mean.Current.dollars)
df$Mean.2018.dollars <- extract_numeric(df$Mean.2018.dollars)

## a. Mean
totalMean5 <- df%>%
  group_by(Year)%>% 
  summarise(Mean=mean(df))
totalMean5 <- colMeans(df)
totalMean5 <- totalMean5[-c(1)]

print(totalMean5)
## b. Mode
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
modeNWI <- getmode(df$Number.with.income)
modeMCD <- getmode(df$Median.Current.dollars)
modeM2D <- getmode(df$Median.2018.dollars)
modeMeanCD <- getmode(df$Mean.Current.dollars)
modeMean2D <- getmode(df$Mean.2018.dollars)
mode2nd <- c(modeNWI, modeMCD,modeM2D, modeMeanCD, modeMean2D)
print(mode2nd)

## c. Median
ecoNWIMed <- sort(df$Number.with.income)
econMCDMed <- sort(df$Median.Current.dollars)
econMI2DMed <- sort(df$Median.2018.dollars)
econMeanCDMed <- sort(df$Mean.Current.dollars)
econMeanI2DMed <- sort(df$Mean.2018.dollars)

ecoNWIMed <- median(ecoNWIMed)
econMCDMed <- median(econMCDMed)
econMI2DMed <- median(econMI2DMed)
econMeanCDMed <- median(econMeanCDMed)
econMeanI2DMed <- median(econMeanI2DMed)

ecoMedian2nd <- data.frame(ecoNWIMed,econMCDMed,econMI2DMed,econMeanCDMed,econMeanI2DMed)

print(ecoMedian2nd)

### Correlation
ecoCor2nd <- cor(df)
print(ecoCor2nd)

#### Linear Regression
eco2ndLR <- lm(Year ~ Number.with.income + Median.Current.dollars + Median.2018.dollars + Mean.Current.dollars + Mean.2018.dollars, data = df)
print(eco2ndLR)

#Todays Migration (1978-2000)
#f <- locate_areas('/cloud/project/p04-converted.pdf', pages = 6)
df2 <- extract_tables('/cloud/project/p04-converted.pdf', 
                     output = "matrix",
                     pages = 6,
                     area = list(c(359, 2, 717, 480)),
                     guess = FALSE)

df2 <- df2[[1]]
df2 <- type.convert(df2)
# df2[24,1] df2[2,1]
df2 <- data.frame('Year' = df2[24:2,1], 'Number with income' = df2[24:2,2],
                 'Median-Current dollars' = df2[24:2,3], 'Median-2018 dollars' = df2[24:2,4],
                 'Mean-Current dollars' = df2[24:2,5], 'Mean-2018 dollars' = df2[24:2,6])
#plot
df2$Year <- as.numeric(gsub(' ',',',df2$Year))
df2$Number.with.income <- extract_numeric(df2$Number.with.income)
df2$Median.Current.dollars <- extract_numeric(df2$Median.Current.dollars)
df2$Median.2018.dollars <- extract_numeric(df2$Median.2018.dollars)
df2$Mean.Current.dollars <- extract_numeric(df2$Mean.Current.dollars)
df2$Mean.2018.dollars <- extract_numeric(df2$Mean.2018.dollars)

df2 <- df2[order(df2$Year), ]
Molten4 <- melt(df2, id.vars = 'Year')
ggplot(Molten4, aes(x=Year, y = value, colour = variable)) + geom_point() + geom_line()

## Summary Stats
## a. Mean
totalMean6 <- df2%>%
  group_by(Year)%>% 
  summarise(Mean=mean(df2))
totalMean6 <- colMeans(df2)
totalMean6 <- totalMean6[-c(1)]

print(totalMean6)
## b. Mode
getmode <- function(x) {
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}
mode3NWI <- getmode(df2$Number.with.income)
mode3MCD <- getmode(df2$Median.Current.dollars)
mode3M2D <- getmode(df2$Median.2018.dollars)
mode3MeanCD <- getmode(df2$Mean.Current.dollars)
mode3Mean2D <- getmode(df2$Mean.2018.dollars)
mode3rd <- c(mode3NWI, mode3MCD,mode3M2D, mode3MeanCD, mode3Mean2D)
print(mode3rd)

## c. Median
econ3NWIMed <- sort(df2$Number.with.income)
econ3MCDMed <- sort(df2$Median.Current.dollars)
econ3MI2DMed <- sort(df2$Median.2018.dollars)
econ3MeanCDMed <- sort(df2$Mean.Current.dollars)
econ3MeanI2DMed <- sort(df2$Mean.2018.dollars)

econ3NWIMed <- median(econ3NWIMed)
econ3MCDMed <- median(econ3MCDMed)
econ3MI2DMed <- median(econ3MI2DMed)
econ3MeanCDMed <- median(econ3MeanCDMed)
econ3MeanI2DMed <- median(econ3MeanI2DMed)

econMedian3rd <- data.frame(econ3NWIMed,econ3MCDMed,econ3MI2DMed,econ3MeanCDMed,econ3MeanI2DMed)

print(econMedian3rd)

### Correlation
econCor3rd <- cor(df2)
print(econCor3rd)

#### Linear Regression
econ3rdLR <- lm(Year ~ Number.with.income + Median.Current.dollars + Median.2018.dollars + Mean.Current.dollars + Mean.2018.dollars, data = df2)
print(econ3rdLR)






