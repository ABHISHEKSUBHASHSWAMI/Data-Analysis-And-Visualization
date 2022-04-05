#loading libraries
library(ggplot2)

#loading data set
deathData<-read.csv("Deathdata.csv")

#storing values
deaths<-(deathData$Deaths)
diseases<-(deathData$Diseases)
male<-(deathData$Male)
female<-(deathData$Female)
gender<-c("Male","Female")
infant<-(deathData$X0.4)
children<-(deathData$X5.14)
youth<-(deathData$X15.29)
adult<-(deathData$X30.44)
middleage<-(deathData$X45.54)
adulthood<-(deathData$X55.69)
senior<-(deathData$X70)


#creating a data frame
frame1<-data.frame(diseases,deaths,male,female)
frame2<-data.frame(diseases,infant,children)

#calculating mean of deaths 

avg<-as.integer(mean(deathData$Deaths))
cat("On average",avg,"people died due to various diseases in 2010.")

#Creating a function for data analysis
analyse<-function(A,B,C){
  #for minimum
  x1<-B[which(A==min(A)),]$diseases
  if(min(A)==0){
    cat("#Minimum:\nNone of the",C,"affected by",x1,"in 2010.\n\nDetailed view :\n\n")
  }
  else{
    cat("#Minimum:\n",min(A),C,"affected by",x1,"in 2010.\n\nDetailed view :\n\n")
  }
  print(B[which(A==min(A)),])
  #for maximum
  x2<-B[which(A==max(A)),]$diseases
  cat("\n#Maximum:\n",max(A),C,"affected by",x2,"in 2010.\n\nDetailed view :\n\n")
  print(B[which(A==max(A)),])
}

#Analyzing affected people
analyse(deaths,frame1,"people")

#plotting circular bar graph of Disease vs Deaths
plot1<-ggplot(frame1,aes(x=diseases,y=deaths,fill=diseases))+
  geom_bar(stat="identity")+
  theme(legend.position="right")+
  ylim(0,45000)+
  coord_polar(start = 0)+
  labs(title="DEATHS DUE TO VARIOUS DISEASES IN 2010",x="Diseases",y="No. of Deaths")
plot1
#Analyzing affected males
analyse(male,frame1,"men")

#Analyzing affected females
analyse(female,frame1,"women")

#extracting male and female deaths
data1<-rbind(male,female)

#bar plot of male vs female
par(mar=c(7.5,5, 2, 1))
plot2<-barplot(data1,main="Male vs Female plot",
        beside = T,
        ylab = "No. of Deaths",
        ylim = c(0, 30000),
        col = c("darkblue","magenta"),
        las=2,
        names.arg= diseases,
        cex.names = 0.6)+
  legend("topright",gender,
         cex=1,col=c("darkblue","magenta"),pch = c(16,16))

#Analyzing affected infants
analyse(infant,frame2,"babies")

#Analyzing affected children
analyse(children,frame2,"children")

#plotting graph (infants)
plot3<-ggplot(frame2,aes(x=diseases,y=infant,fill=diseases))+
  geom_bar(stat="identity")+
  theme(legend.position="right")+
  ylim(0,12000)+coord_flip()+
  labs(title="Infants affected due to various diseases",y="No. of Infants",x="Diseases")
plot3

#plotting graph (children)
plot4<-ggplot(frame2,aes(x=diseases,y=children,fill=diseases))+
  geom_bar(stat="identity")+
  theme(legend.position="right")+
  ylim(0,1000)+coord_flip()+
  labs(title="Children affected due to various diseases",y="No. of Children",x="Diseases")
plot4

#Data age group
agegroups<-c("0 to 4","5 to 14","15 to 29","30 to 44","45 to 54","55 to 69","70 and above")
agewisedeaths<-c(sum(infant),sum(children),sum(youth),sum(adult),sum(middleage),sum(adulthood),sum(senior))
agedata<-data.frame(agegroups,agewisedeaths)

#Calculating percentage death per age group
total<-sum(agewisedeaths)
percentdeaths<-c()
for (values in agewisedeaths){
  percentdeaths<-c(percentdeaths,(values*100/total))
}
percentdeaths<-round(percentdeaths,2)
#plotting pie chart
plot5<-ggplot(agedata, aes(x="",y=percentdeaths,fill=agegroups)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()
plot5

#animating birth rate and death rate year wise
rates<-read.csv("rate.csv")
prop<-c("Births", "Deaths")

#calculate population in billions
Population<-rates$POPULATION/1000000000
#loading required libraries
library(plotly)
library(hrbrthemes)

# Usual area chart
plot6<-ggplotly(ggplot(rates,aes(x=YEAR, y=Population)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("Population in Billions") +
  theme_ipsum())
plot6
#visualize birth rate vs Death rate
library(gganimate)

plot7<-ggplot(rates,aes(x=YEAR)) +
  geom_line(aes(y=RATEBIRTH),colour="green") +
  geom_line(aes(y=RATEDEATH),colour="red")+
  scale_color_viridis_d() +
  labs(title="Birth Rate vs Death Rate",x = "Year", y = "Rate")+
  theme_classic()+
  transition_reveal(YEAR)
plot7

#visualizing deaths due to various diseases in each states
#tree plot

statewise<-read.csv("states.csv")
States<-statewise$States
People<-statewise$People

library(treemap)
plot8<-treemap(statewise,index="States",vSize="People",
        palette ="Set3",ylim(c(0,60000)),title =("Density of Deaths in Various States")
        )

#3D plot of 
library(plotly)
library(dplyr)
plot9<-plot_ly(statewise,x=statewise$Male,y=statewise$Female,z=States)
plot9%>%layout(scene = list(xaxis = list(title = 'Male'),
                      yaxis = list(title = 'Female'),
                      zaxis = list(title = 'States')))





