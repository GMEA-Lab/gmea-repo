#This code looks at the data from the SIMBA ice chain that was deployed near Qik during Feb-June 2020 and
#attempts to determine the air/snow and water/ice interfaces


library(tidyverse)
library(lubridate) #makes it easier to deal with date/times
#get data from github
#Simba<-read_csv(url("https://raw.githubusercontent.com/jchawarski/ITO_Qik2020/main/SIMBA/uit0201td_2020-11-04_14-21-24.csv"))
#or get from project folder

Simba<-read_csv("SimbaData_UIT0201.csv")
str(Simba)
#Change to usable date time formate. 
#According to manual the timer period interval is set in UTC so I'll assume send time is also utc
Simba$SendTime2<-dmy_hm(Simba$SendTime,tz="UTC")

#just see what's happening here
Simba_date1<-Simba %>% group_by(SendTime2) %>% summarise(tot=n()) # basically 1 or 2 transmissions per sendtime   
Simba_date2<-Simba %>% mutate(Date2=as_date(SendTime2)) %>% group_by(Date2) %>% summarise(tot=n()) #about 6 per day. earlier days have less

#only interested in dates from 25th Feb and on
Simba2<-Simba %>% filter(SendTime2 > "2020-02-24 18:00:00")

#Lets look at airtemp first
unique(Simba2$msgtype) #10,11,14
#for now going to filter for only message type=10 because that's the unheated norm temps
#message type 11=Temperature change at time HST1.
#message type 14=Temperature change at time HST1+HST2+HST3+HST4. Heater off.
AirTemp<-Simba2 %>% filter(msgtype==10) %>% select(AirTemp, SendTime2)
hist(Simba2$AirTemp) #just quickly looking I don't really see any outliers
summary(AirTemp$AirTemp)
Hot<-AirTemp %>% filter(AirTemp>0)
ggplot(AirTemp, aes(SendTime2, AirTemp)) + 
  geom_line(color="dodgerblue3", size=1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date Time (UTC)")+ylab("Air Temperature (C)")+
  labs(title="Simba Air Temperature Readings 25Feb-02June 2020")

write.csv(AirTemp, "R_Output/Simba_AirTemp.csv", row.names = F)
#looks about like it should, I think

#look at rest of sensors
Simba_long<-Simba2 %>% filter(msgtype==10) %>% select(SendTime2, 12:251)
Simba_long<-Simba_long %>% pivot_longer(
                                        cols=2:241,
                                        names_to="Measurement",
                                        values_to="Temp")

hist(Simba_long$Temp) #some -90s values I think
summary(Simba_long$Temp) #NAs=720, max=10.4375, min=-97.750, mean=-6.429, median= -1.938
#sigh....
Simba_NA<-Simba_long %>% filter(is.na(Temp)) %>% group_by(SendTime2) %>% summarise(tot=n())
#NAs in 2020-03-15, 2020-04-02, 2020-05-01, 2020-05-14....also for some reason there's extra data on the 14th at 00:00:00
#March 15th had a double at 1800 so theres still data for that time period

#sigh...well those nums don't really seem right....
Simba_cold<-Simba_long %>% filter(Temp< -90) #all on the second of June which is when it stopped recording data and prob malfunctioned bc it was in an melt pond
Simba_Hot<-Simba_long %>% filter(Temp>3) #since air temp max=2.8, seems to be the end of april through june some of the sensors are quite warm

#the first and second of june didn't have all the transmission anyways so I'm just going to remove them for now
#sigh...what to do....
#going to make a new datset 
Simba_long2<-Simba2 %>% filter(msgtype==10) %>% select(SendTime2, 12:251)
#so really cold temps to -30 through T241 on feb 25th and 26th but not 27th, which makes me think it was out of the water until the 27th
Simba_long2<- Simba_long2 %>% filter(SendTime2 > "2020-02-26 18:00:00" & SendTime2< "2020-05-31 21:00:00")
summary(Simba_long2$SendTime2)
#first going to get rid of those duplicated rows on the 14th
Simba_long2<-Simba_long2[!duplicated(Simba_long2),] #gets rid of the 2 duplicates
#And the March 15th has some weird duplicate as well for 18:00:00, one row has full set of data one row has NAs
Simba_long2<-Simba_long2 %>% filter(!(is.na(T0) & date(SendTime2)=="2020-03-15 18:00:00"))


Simba_long2<-Simba_long2 %>% pivot_longer(
  cols=2:241,
  names_to="Measurement",
  values_to="Temp")
class(Simba_long2)
Simba_long2<-as.data.frame(Simba_long2)

#now what to do about the missing data...
#the zoo package can interpolate and/or extrapolate missing data
library(zoo)
#make numeric measurement variable
Simba_long2$Measurement2<-as.numeric(str_remove(Simba_long2$Measurement, "[T]"))
##
Simba_long3<- Simba_long2 %>% mutate(Date=date(SendTime2))%>% group_by(Date, Measurement2)%>% arrange(SendTime2, .by_group=T) %>%
              mutate(Temp2=na.spline(Temp, na.rm=F)) 
#na.spline can deal with leading NAs and bc hourly temperature has a daily cycle to it typically
approxtest<-Simba_long3 %>% filter(date(SendTime2)=="2020-04-02") #so I think that worked



Simba_long3<-as.data.frame(Simba_long3)

#see if we can get this plotted
ggplot(Simba_long3, aes(SendTime2, Measurement2, fill= Temp2)) + 
  geom_tile()+
  scale_fill_distiller()+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_hline(yintercept=78, linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Temperature Readings 26Feb-31May2020", fill="Temp (C)")


#warm days >3C started on april 30th 

#diff color pallet maybe
ggplot(Simba_long3, aes(SendTime2, Measurement2, fill= Temp2)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red","violetred2"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_hline(yintercept=78, linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Temperature Readings 26Feb-31May2020", fill="Temp (C)")



#not sure smoothing will help with patterns but...
summ<-Simba_long3 %>% ungroup() %>%
  summarise(mean_T=mean(Temp2,na.rm=T), sd_T=sd(Temp2,na.rm=T), tot=n())%>%
  mutate(se_T=sd_T/(sqrt(tot))) 
mean(Simba_long3$Temp2)+(1.96*0.03) #-5.89
mean(Simba_long3$Temp2)-(1.96*.03) #-6.007

Simba_long3$Temp3<-with(Simba_long3, ifelse(Temp2>7.9, 7.9,
                                     ifelse(Temp2< -20, -20, Simba_long3$Temp2)))




ggplot(Simba_long3, aes(SendTime2, Measurement2, fill= Temp3)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_hline(yintercept=78, linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Smoothed Temperature Readings 26Feb-31May2020", fill="Temp (C)")

Temp_means<-Simba_long3 %>% group_by(Measurement2) %>% summarise(avg=mean(Temp2), Tempsd=sd(Temp2),tot=n(), Max_T=max(Temp2), Min_T=min(Temp2)) %>%
            mutate(se=Tempsd/(sqrt(tot)))%>%
            mutate(L95=avg-(1.96*se), U95=avg+(1.96*se))

Simba_long4<-merge(Simba_long3, Temp_means, by="Measurement2")
Simba_long4$Temp4<-with(Simba_long4, ifelse(Temp2>U95, U95,
                                     ifelse(Temp2<L95, L95, Simba_long4$Temp2)))


ggplot(Simba_long4, aes(SendTime2, Measurement2, fill= Temp4)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_hline(yintercept=78, linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Smoothed2 Temperature Readings 26Feb-31May2020", fill="Temp (C)")


#what about plotting the air temp on top
#going to make a new datset 
Simba_AirWater<-Simba2 %>% filter(msgtype==10) %>% select(SendTime2, 12:252)
#so really cold temps to -30 through T241 on feb 25th and 26th but not 27th, which makes me think it was out of the water until the 27th
Simba_AirWater<- Simba_AirWater %>% filter(SendTime2 > "2020-02-25 18:00:00" & SendTime2< "2020-05-31 21:00:00")
Simba_AirWater<-Simba_AirWater[!duplicated(Simba_AirWater),] #gets rid of the 2 duplicates
#And the March 15th has some weird duplicate as well for 18:00:00, one row has full set of data one row has NAs
Simba_AirWater<-Simba_AirWater %>% filter(!(is.na(T0) & date(SendTime2)=="2020-03-15 18:00:00"))

Simba_AirWater<-Simba_AirWater %>% pivot_longer(
  cols=2:242,
  names_to="Measurement",
  values_to="Temp")

Simba_AirWater<-as.data.frame(Simba_AirWater)

#now what to do about the missing data...
#the zoo package can interpolate missing data
Simba_AirWater<- Simba_AirWater %>% mutate(Date=date(SendTime2))%>% group_by(Date, Measurement)%>%
  mutate(Temp2=zoo::na.spline(Temp, na.rm=F))


Simba_AirWater<-as.data.frame(Simba_AirWater)
Simba_AirWater$Measurement2<-as.numeric(str_remove(Simba_AirWater$Measurement, "[T]"))

Simba_AirWater$Measurement2<-with(Simba_AirWater, ifelse(is.na(Simba_AirWater$Measurement2), -1, Simba_AirWater$Measurement2))
Simba_AirWater$Measurement<-as.factor(Simba_AirWater$Measurement)
Simba_AirWater$Measurement<-reorder(Simba_AirWater$Measurement, Simba_AirWater$Measurement2)
class(Simba_AirWater)
Simba_AirWater<-as.data.frame(Simba_AirWater)

ggplot(Simba_AirWater, aes(SendTime2, Measurement, fill= Temp2)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red","violetred2"))+
  scale_y_discrete(limits = rev(levels(Simba_AirWater$Measurement)), breaks=c("AirTemp","T25","T50","T75","T100","T125","T150","T175","T200","T225"))+
  scale_x_datetime(expand=c(0,0))+
  geom_hline(aes(yintercept = "T78"), linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Temperature Readings 26Feb-31May2020", fill="Temp (C)")




###look at heat sensor temps at 120s. Rise of temp at 120s of heating is ~2C in air and ~0.2C in ice and water
Simba_heat<-Simba2 %>% filter(msgtype==14) %>% select(SendTime2, 12:251)
Simba_heat<- Simba_heat %>% filter(SendTime2 > "2020-02-25 18:00:00" & SendTime2< "2020-05-31 21:00:00")

Simba_heat<-Simba_heat %>% pivot_longer(
  cols=2:241,
  names_to="Measurement",
  values_to="Temp_14")

Simba_heat<-as.data.frame(Simba_heat)

Simba_heat$Measurement2<-as.numeric(str_remove(Simba_heat$Measurement, "[T]"))

#Simba_heat2<-Simba_heat %>% group_by(SendTime2) %>% arrange(SendTime2,Measurement2) %>% 
#            mutate(Diff=Temp - lag(Temp, default = Temp[1]))


ggplot(Simba_heat, aes(SendTime2, Measurement2, fill= Temp_14)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_hline(yintercept=78, linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
 labs(title="Simba Heat Sensor Readings 120s 26Feb-31May2020", fill="Temp (C)",
      xlab="Date",ylab="Measurement")

###look at heat sensor temps at 120s and 60s Rise of temp at 120s of heating is ~2C in air and ~0.2C in ice and water
Simba_heat2<-Simba2 %>% filter(msgtype==11) %>% select(SendTime2, 12:251)
Simba_heat2<- Simba_heat2 %>% filter(SendTime2 > "2020-02-25 18:00:00" & SendTime2< "2020-05-31 21:00:00")

Simba_heat2<-Simba_heat2 %>% pivot_longer(
  cols=2:241,
  names_to="Measurement",
  values_to="Temp_11")
Simba_heat<-as.data.frame(Simba_heat)


Simba_heat3<-merge(Simba_heat, Simba_heat2, by=c("SendTime2","Measurement"))

Simba_heat3$Ratio<-(Simba_heat3$Temp_14/Simba_heat3$Temp_11)
#well that looks like crap
ggplot(Simba_heat3, aes(SendTime2, Measurement2, fill= Ratio)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_hline(yintercept=78, linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Heat Sensor Readings 120-60s 26Feb-31May2020", fill="Temp (C)")




Simba_AirWater2<-Simba_AirWater%>%  group_by(SendTime2) %>% 
                 mutate(Diff=abs(Temp2 - lead(Temp2, order_by = Measurement2)))
Simba_AirWater2<-as.data.frame(Simba_AirWater2)  
ggplot(Simba_AirWater2, aes(SendTime2, Measurement2, fill= Diff)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_hline(yintercept=78, linetype="dashed", color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Readings Diff 26Feb-31May2020", fill="Temp (C)")


Simba_heat5<-Simba2 %>% filter(msgtype==14) %>% select(SendTime2, 12:252)
Simba_heat5<- Simba_heat5 %>% filter(SendTime2 > "2020-02-26 18:00:00" & SendTime2< "2020-05-31 21:00:00")

Simba_heat5<-Simba_heat5 %>% pivot_longer(
  cols=2:242,
  names_to="Measurement",
  values_to="Temp_14")

Simba_heat5<-as.data.frame(Simba_heat5)
Simba_heat5$Date<-date(Simba_heat5$SendTime2)
Simba_heat5<-Simba_heat5 %>% select(-(SendTime2))

Simba_AirWater3<-Simba_AirWater2 %>% mutate(Date=date(SendTime2)) %>%
  left_join(Simba_heat5) %>% filter(SendTime2 > "2020-02-26 18:00:00" & SendTime2< "2020-05-31 21:00:00")

#Liao et al 2018 calculated the mode of the 30 bottom sensors to define the freezing temp, the last sensor with this temp (from bottom up) is the ice bottom

#there is no function for mode in R for some reason so found this online
Mode = function(x){
  ta = table(x)
  tam = max(ta)
  if (all(ta == tam))
    mod = NA
  else
    if(is.numeric(x))
      mod = as.numeric(names(ta)[ta == tam])
  else
    mod = names(ta)[ta == tam]
  return(mod)
}
Mode(c(6,7,9,9,9,10,12,13,13))
BotTemp_Mode<-Simba_AirWater3 %>% filter(Measurement2>208) %>% group_by(SendTime2) %>% summarize(BotTemp_Mode=Mode(Temp2))
BotTemp_Mode2<-BotTemp_Mode %>% group_by(SendTime2) %>% summarise(tot=n())
##Hmmm sometimes mode is 2 numbers maybe it would be better to do a min max type thing?
BotTemps<-Simba_AirWater3 %>% filter(Measurement2>208) %>% group_by(SendTime2) %>% summarize(Min_BotTemp=min(Temp2), Max_BotTemp=max(Temp2))

Simba_AirWater3<-left_join(Simba_AirWater3, BotTemps)

prac<-head(Simba_AirWater3, 5000)

#liao et al 
library(zoo)
prac2<-prac %>% group_by(SendTime2) %>% arrange(SendTime2,Measurement2) %>%
                 mutate(medium=case_when(Measurement2==0|Measurement2==-1~"Air",
                               Diff<0.4375 & Temp_14>1.5~"Air"),
                        medium=ifelse(is.na(medium) & Diff==0.4375 & lag(medium)=="Air","Snow",
                               ifelse(is.na(medium) & Diff==0.1875 & lag(medium)=="Air","Ice", medium)),
                        medium=ifelse(is.na(medium) & Diff>=0.4375 & lag(medium)=="Snow","Snow",
                               ifelse(is.na(medium) & Diff <=0.1875 & Temp2<Min_BotTemp,"Ice",
                               ifelse(is.na(medium) & Temp2 >= Min_BotTemp & Temp2 <= Max_BotTemp, "Water",medium))))

prac3<-prac %>% group_by(SendTime2) %>% arrange(SendTime2,desc(Measurement2)) %>%
  mutate(medium=case_when(Measurement2==239~"Water",
                          Temp2 >= Min_BotTemp & Temp2 <= Max_BotTemp~"Water"),
         medium=ifelse(is.na(medium) & Temp2<Min_BotTemp & lag(medium)=="Water","Ice", medium),
         medium=ifelse(is.na(medium) & lag(medium)=="Ice","Ice",medium)) #doesn't fill in the missing blanks but I think that's ok, we only need to mark the interfaces


prac3<-prac3 %>%group_by(SendTime2) %>% arrange(SendTime2,Measurement2) %>%
                 mutate(medium=ifelse(Measurement2==0|Measurement2==-1,"Air",
                        ifelse(Diff<0.4375 & Temp_14>1.5,"Air", medium)),
                     medium=ifelse(is.na(medium) & Diff>=0.4375 & lag(medium)=="Air","Snow", medium),
                     medium=ifelse(is.na(medium)& Diff<=0.1875 & lag(medium)=="Air","Ice", medium))



Simba_AirWater4<-Simba_AirWater3 %>% group_by(SendTime2) %>% arrange(SendTime2,desc(Measurement2)) %>%
  mutate(medium=case_when(Measurement2==239~"Water",
                          Temp2 >= Min_BotTemp & Temp2 <= Max_BotTemp~"Water"),
         medium=ifelse(is.na(medium) & Temp2<Min_BotTemp & lag(medium)=="Water","Ice", medium),
         medium=ifelse(is.na(medium) & lag(medium)=="Ice","Ice",medium)) #doesn't fill in the missing blanks but I think that's ok, we only need to mark the interfaces

Ice_Bottom<-Simba_AirWater4 %>% 
  group_by(SendTime2) %>% arrange(SendTime2,desc(Measurement2)) %>%
  filter(medium == "Ice" & is.na(lead(medium,2))) %>% filter(Measurement2==max(Measurement2))  


Simba_AirWater4<-Simba_AirWater4 %>%group_by(SendTime2) %>% arrange(SendTime2,Measurement2) %>%
  mutate(medium=ifelse(Measurement2==0|Measurement2==-1,"Air",
                ifelse(Diff<0.4375 & Temp_14>1.5,"Air", medium)),
         medium=ifelse(is.na(medium) & Diff>=0.4375 & Measurement2<75,"Snow", medium),
         medium=ifelse(is.na(medium)& Diff<=0.1875 & lag(medium)=="Air" & Measurement2<75,"Ice",
                ifelse(is.na(medium)& Diff<=0.1875 & lag(medium)=="Snow" & Measurement2<75,"Ice",medium)))

Ice_Top<-Simba_AirWater4 %>% 
  group_by(SendTime2) %>% arrange(SendTime2,Measurement2) %>%
  filter(medium == "Ice" & Measurement2<75) %>% filter(Measurement2==max(Measurement2)) 

#a few wonky measurements I'll need to go through and fix and a few are missing but I think that's ok bc ultimately we just want to know where the snow starts and the ice stops I think

Simba_AirWater4<-Simba_AirWater4 %>% filter(Measurement2>-1)
ggplot(Simba_AirWater4, aes(SendTime2, Measurement2, fill= Temp2)) + 
  geom_tile()+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3","orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_line(data=Ice_Top, aes(x=SendTime2, y=Measurement2), size=.5,color = "white")+
  geom_line(data=Ice_Bottom, aes(x=SendTime2, y=Measurement2), size=.5,color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Measurement")+labs(title="Simba Readings Diff 27Feb-31May2020", fill="Temp (C)")


##What to do about snow....?
##hmmmm lots of outliers what to do about that....
#ooof ok, so looking at siku data there was never more than 32cm of snow on the ground.
#Although not in the same location, perhaps it's safe to assume there was never >40 
#(20 sensors), so make a new snow layer with some cutoffs
Snow<-Simba_AirWater4 %>% filter(medium=="Snow" & Measurement2>49 & lead(medium,2)=="Snow") %>% filter(Measurement2==min(Measurement2))

##still outliers in some of the layers
Layers<-as.data.frame(unique(Simba_AirWater4$SendTime2))
Layers<-Layers %>% rename(SendTime2= `unique(Simba_AirWater4$SendTime2)`)
Layers<-merge(Layers, Snow[,c("SendTime2", "Measurement2")], all.x=T)
Layers<-Layers %>% arrange(SendTime2) %>% mutate(Snow2=na.approx(Measurement2, na.rm=F)) %>% rename(Snow=Measurement2)
Layers<-merge(Layers, Ice_Bottom[,c("SendTime2", "Measurement2")], all.x=T)
Layers<-Layers %>% rename(Ice_Bottom=Measurement2)
Layers<-merge(Layers, Ice_Top[,c("SendTime2", "Measurement2")], all.x=T)
Layers<-Layers %>% rename(Ice_Top=Measurement2) %>%  mutate(Ice_Top2=ifelse(Ice_Top<55,NA,Ice_Top),                                                           Ice_Top2=na.approx(Ice_Top2, na.rm=F))
Layers<-Layers %>% mutate(Diff=abs(Snow2-Ice_Bottom)*2)

summ<-Simba_AirWater4 %>% ungroup() %>%
  summarise(mean_Diff=mean(Diff,na.rm=T), sd_Diff=sd(Diff,na.rm=T), tot=n())%>%
  mutate(se_Diff=sd_Diff/(sqrt(tot))) 
0.1+(1.96*.0005)   #rather than setting values outside CI to top 95% might set to 0.5 since diff between sensors in snow=>0.4528 and those should be the greatest diff bt sensors      

Simba_AirWater4$Diff_smooth<-with(Simba_AirWater4, ifelse(Diff>0.4528,0.5,Simba_AirWater4$Diff))


ggplot(Simba_AirWater4, aes(SendTime2, Measurement2)) + 
  geom_tile(aes(fill=Diff))+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3",
                                "orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_line(data=Layers, aes(x=SendTime2, y=Snow2), size=.5,color = "white")+
  geom_line(data=Layers, aes(x=SendTime2, y=Ice_Top2), size=.5,color = "white")+
  geom_line(data=Layers, aes(x=SendTime2, y=Ice_Bottom), size=.5,color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Simba Readings Diff 27Feb-31May2020", fill="Temp (C)",
       x="Date",y="Measurement")


ggplot(Simba_AirWater4, aes(SendTime2, Measurement2)) + 
  geom_tile(aes(fill=Diff_smooth))+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3",
                                "orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0))+
  geom_line(data=Layers, aes(x=SendTime2, y=Snow2), size=.5,color = "white")+
  geom_line(data=Layers, aes(x=SendTime2, y=Ice_Top2), size=.5,color = "white")+
  geom_line(data=Layers, aes(x=SendTime2, y=Ice_Bottom), size=.5,color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Simba Readings Smooth Diff 27Feb-31May2020", fill="Temp Diff Smoothed (C)",
       x="Date",y="Measurement")



ggplot(Layers, aes(SendTime2, Diff)) + 
  geom_line(color="dodgerblue3", size=1)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  xlab("Date")+ylab("Snow Ice Thickness (cm)")+
  labs(title="Simba Snow Ice Thickness 27Feb-31May2020")

test<-Simba_heat %>%filter(Measurement2<69)
test2<-Simba_long3 %>%filter(Measurement2<69)
Layers<-Layers %>% mutate(SnowDepth=abs(Snow2-Ice_Top2)*2, IceDepth=abs(Ice_Bottom-Ice_Top2)*2)


  ggplot(Simba_AirWater4, aes(SendTime2, Measurement2)) + 
  geom_tile(aes(fill=Diff_smooth))+
  scale_fill_gradientn(colors=c("navyblue","blue2","dodgerblue2","green4","yellow3",
                                "orange3","orangered3","red"))+
  scale_y_continuous(trans = "reverse", expand = c(0, 0)) +
  scale_x_datetime(expand=c(0,0), limits=c(as.POSIXct("2020-05-13"),as.POSIXct("2020-06-01")))+
  geom_line(data=Layers, aes(x=SendTime2, y=Snow2), size=.5,color = "white")+
  geom_line(data=Layers, aes(x=SendTime2, y=Ice_Top2), size=.5,color = "white")+
  geom_line(data=Layers, aes(x=SendTime2, y=Ice_Bottom), size=.5,color = "white")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  labs(title="Simba Readings Smooth Diff 14May-31May2020", fill="Temp Diff Smoothed (C)",
       x="Date",y="Measurement")
                   
  write.csv(Layers,"Layers.csv", row.names=F)
  