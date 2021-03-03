

DateTime<-seq(as.POSIXct("2020/02/25"), as.POSIXct("2020/07/01"), "hour")

moon<-as.data.frame(DateTime)

library(lubridate)
tz(moon$DateTime)
moon$DateTime<-force_tz(moon$DateTime, tzone="UTC")
class(moon$DateTime)
#using general qik location at the moment
moon$lat<-67.553020
moon$lon<- -64.03978


library(oce)
library(tidyverse)

#the altitude of the moon, expressed as degrees above (positive) or below (negative) the horizon
moon$moonAlt<-moonAngle(moon$DateTime, moon$lon, moon$lat)$altitude

#the decimal proportion (0 to 1) of the moon's surface illuminated, 
#0=new moon, 0.25=first quart, 0.5=full, 0.34=last quart, this is how it's defined in the package
moon$moonIllum<-moonAngle(moon$DateTime, moon$lon, moon$lat)$illuminatedFraction


#interesting idea, def may need to be played with a bit
#moon phase can be categorized into "Dark Moon", "Mid Moon", and "Full Moon" based upon a combination of MoonIlluminatedFrac and MoonAlt (i.e., angle of the moon above or below the horizon). 
#Full Moon periods were defined as those times when MoonIlluminatedFrac was ??? 0.9 and MoonAlt was > 0 (i.e., the moon was above the horizon). 
#Dark Moon periods were defined as those times when MoonIlluminatedFrac was < 0.1 or MoonAlt was < 0 (i.e., moon was below the horizon). 
#identify all time periods where lunar illumination was low or absent, whether this was the result of moon phase or of the moon being below the horizon. 
#All time periods where MoonIlluminatedFrac was ??? 0.1 but <0.9 and Moon Angle was > 0 were termed Mid Moon. 
#This category accounted for periods with intermediate lunar illumination. (leonard et al 2020


#sun angle, into the categories of astronomical twilight(???18° < sun angle < ???12°), 
#day (sun angle > ???12°; nautical dawn)
#and night (sun angle < ???18°). #I think this is based on solar declination
moon$sunAlt<-sunAngle(moon$DateTime, moon$lon, moon$lat)$altitude
moon$sunDeclin<-sunAngle(moon$DateTime, moon$lon, moon$lat)$declination

library(lunar)
moon$phase_lunar<-lunar::lunar.phase(as.Date(moon$DateTime),name=8)
