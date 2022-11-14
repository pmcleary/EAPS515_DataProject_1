library(tidyverse)
library(maps)
library(lattice)
##----Section 1.3------
##----Question #1-------
attach("ALHURDAT.RDATA")
search()
objects(2)
##----Question #2------
str(AL)
summary(AL)
##---Question #3-------
factor(AL$Key)
factor(AL$Name)
factor(AL$Record)
factor(AL$Status)
##----Section 1.4-----
#### Extract the Year characters from DateTime, then convert to numeric.
#### This step has been performed for ALHURDAT.RDATA, so you don't have to.
AL$Year <- as.numeric(format(as.Date(AL$DateTime), "%Y"))  #This loads AL into the Global Environment
#### Extract a subset for Katrina (2005)
Kt <- AL %>% filter(Year == 2005 & Name == "KATRINA")
##----Question #4------
str(Kt)
summary(Kt)
##----Question #5------
#find "ET" Stauts in AL 
AL %>% filter(Status == "ET") #filter from dplyr extracts columns that meet a condition 
# Discusion
# Hurricane Harvey from 1993 has status "ET"
# Based on the Latitude of the storm it appears to be an extratropical storm which is normally the EX 
# status.  ET may be a typo with someone intending it to mean extratropical.

##----Section 2.1------
##----Section 2.1.1----
##----Question #6------
#extract Hurricane Sandy, 2012
Sd <- AL %>% filter(Year == 2012 & Name == "SANDY")
# Fill the last data points with color black
# Fill the first data points with color red
nstep <- dim(Sd)[1]
splom(~Sd[,c(6:9)],
      varnames = c("Lat\n(deg)",
                   "Lon\n(deg)",
                   "Wind\n(kt)",
                   "Pressure\n(mb)"),
      type="b",
      main="Sandy (2012)",
      panel = function(x,y,...){
        panel.splom(x,y,...)
        panel.xyplot(x[nstep],y[nstep], pch = 16, #nstep = 45, i.e last plot 
                     col="black")
        panel.xyplot(x[1],y[1], pch = 16, #x[1] = 1, i.e first plot
                     col="red")
      }
)

##----Question #7-----
#subset wind and pressure for the year 2012
# below filters rows by year == 2012 first then selects the columns Wind and Pressure 
wind_press <- AL %>% filter(Year == 2012) %>% select(Wind, Pressure) #select from dplyr extracts columns (variables)
#Create a bivariate scatterplot of wind vs pressure
xyplot(Wind ~ Pressure,
       data = wind_press,
       aspect = 1,
       xlab="minimum sea-level pressure (mb)",
       ylab="maximum 10-m wind (kt)",
       main="2012 All Storms",
       sub="Scatterplot of Pressure vs Wind")

##----Question #8------
#Box and Whisker plot for wind and pressure in 2012, order plots by median wind speed
#create a dataframe with Name, Wind, and Pressure
name_wind_press_2012 <- AL %>% filter(Year == 2012) %>% select(Name,Wind, Pressure) 
#find the median wind speed aggregated by Name
#create a new vector ordered by wind speed
windspd_order <- with(name_wind_press_2012, reorder(Name , Wind, median , na.rm=T))
#create a bwplot with the new ordered vector and the original wind speed data
bwplot(windspd_order ~ name_wind_press_2012$Wind,
            aspect = 1,
            NA.RM=TRUE,
            xlab="Maximum Sustained 10-m Wind (kts)")
# Discussion
# Sandy is the most famous Hurricane of 2012 and did in fact have the highest median wind speed.

##------Question #9---------
# Create stripplot for Year 2012
stripplot(Name ~ Wind,
          data = AL,
          subset = Year == 2012,
          jitter = TRUE,
          xlab="Maximum Sustained 10-m Wind (kts)")
# Discussion
# Jittering more clearly displays the plots because without jittering the points are actually
# overlapped but it is difficult to discern this. Jittering introduces a small amount of noise to 
# avoid this overlap.

##-----Question #10-----------
status.mean <- tapply(AL$Wind, AL$Status, mean, na.rm = TRUE)
dotplot( names(status.mean)~status.mean,
          aspect = 1,
          main = "1851--2016",
          ylab = "Status",
          xlab = "Wind (kts)" )
# Discussion
# To correct TD not plotting in the exampleAdded na.rm = TRUE 
# to the mean function within tapply of status.mean.
# 
# ##-----Question #11----------
statusP.mean <- tapply(
   subset(AL,Year>=1990)$Pressure,
   subset(AL,Year>=1990 )$Status, mean,na.rm = TRUE)
dotplot(names(statusP.mean)~statusP.mean,
          aspect = 1,
          main = "1990 -- 2018",
          ylab = "Status",
          xlab = "Pressure (mb)"
)
 
##------Section 2.1.6--------
##------Question #12---------
qq(Status ~ Wind,
    data = subset(AL, Year==2005 ),
    subset = Status=="HU" | Status=="TS",
    aspect = 1
)
# Discussion
# The two data sets do not have a similar distribution and the HU status 
# has much higher values than the TS status.

##------Section 2.4-----------
##------Question #13----------
## Multi panel plot with xyplot
# Create the base map of the continent outlines
mp <- map("world", ylim=c(20,60), xlim=c(-100, 0),plot = FALSE, fill=FALSE)
panel.map <- function(x,y,...)
{panel.xyplot(mp$x, mp$y,pch=".",col="gray",alpha=0.25)
  panel.xyplot(x,y,...)
}
trellis.device(pdf,file="Q13_Cleary_Paul.pdf")
xyplot(Lat~Lon | as.factor(Name),
       groups = as.factor(Name),
       data = subset(AL, Year==2005),
       type = "b",
       lty = 1:7,
       pch = 1:7,
       col = 1:7,
       scales = list(y=list(tick.number=8)),
       main = "Atlantic Storms in 2005",
       xlab=expression(paste("Longitude (", {}^o,"E)")),
       ylab=expression(paste("Latitude (", {}^o,"N)")),
       ylim=c(10,60), xlim=c(-100, 0),
       panel=panel.map 
)
dev.off()

##--------Question #14---------
# first create a df of all storms in 2017 
storms2017 <- AL %>% filter(Year == 2017)
# find the median wind speed aggregated by Name then create a new vector 
# ordered by wind speed make the second input to reorder (Wind) negative to 
# order the medians by decreasing value
windspd_order2017 <- with(storms2017, reorder(Name , -Wind, median , na.rm=T ))
trellis.device(pdf,file="Q14_Cleary_Paul.pdf")
bwplot(windspd_order2017 ~ storms2017$Wind,
       aspect = 1,
       NA.RM=TRUE,
       xlab="Maximum Sustained 10-m Wind (kts)",
       main = "2017 Storms by decreasing median")  
dev.off()

##---------Question #15----------
# Subset the AL df to only have storms from 1980 and later
q15df <- AL %>% filter(Year >= 1980)
# Replace the erroneous status "ET" with "EX"
q15df$Status <- recode(q15df$Status, ET = "EX")
# use aggregate to calculate the median wind speed per status for each year
q15dfmedian <-  aggregate(Wind~Status + Year, q15df, median)
q15dfmedian$Year <- as.factor(q15dfmedian$Year)
trellis.device(pdf,file="Q15_Cleary_Paul.pdf")
dotplot(reorder(Status, Wind) ~ Wind| Year,
        data = q15dfmedian,
        groups = as.factor(Status),
        aspect = 1,
        as.table = TRUE,
        #par.strip.text=list(cex=.8),
        auto.key = list(columns = 5),
        main = "Median Winds Speed by Storm Status from 1980-1918",
        ylab = "Storm Status",
        xlab = "Median Wind (kts)" )
dev.off()
# I was unable to order the panels accourding to the median of the 'HU' status but 
# this plot method is helpful to visualize how each year differs from the next.  
# It can also be helpful to visualize change over an extended period of time. 