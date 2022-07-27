



library(ggmap)
library(ggplot2)

#anno file from AADR#

anno=read.csv("v50.0_1240K_public1.anno",header=FALSE,sep="\t")

fam=read.table("ind_m45_g50_dates.txt",header = TRUE)
head(fam)
nrow(fam)

fam$lon=anno$V17[match(fam$IID,anno$V2)]
fam$lat=anno$V16[match(fam$IID,anno$V2)]
fam$meanBP=((fam$fromBP+fam$toBP)/2)

fam$lon=as.numeric(fam$lon)
fam$lat=as.numeric(fam$lat)


euromid=c(lon=18,lat=50)
euromap=get_map(location=euromid,zoom=4,crop=FALSE,maptype="terrain-background",
                scale=2)

p=ggmap(euromap)
p=p+geom_point(aes(x=lon,y=lat,color=meanBP),data=fam,alpha=1,
               size=2)
p=p+labs(y="Latitude",x="Longitude",title="Geographic distribution of ancient EUR individuals",
         subtitle="9000 to 2000 years BP",color="Years BP")
p=p+scale_colour_gradient(high="royalblue",low="indianred")
p=p+theme(axis.title.x=element_text(margin=margin(t=10),size=25),
          axis.title.y=element_text(margin=margin(r=10),size=25),
          plot.title=element_text(margin=margin(r=10,0,0,0),size=40,hjust=0.5),
          plot.subtitle=element_text(hjust=0.5,size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          legend.text=element_text(size=15),
          legend.title=element_text(size=20,hjust=0.5))
#guide_axis())
p=p+guides(color=guide_colorsteps())
print(p)


fam$sex=anno$V23[match(fam$IID,anno$V2)]
head(fam)
mean(fam$sex=="U")

ggplot(fam,aes(x=(fromBP+toBP)/2,fill=sex))+
  geom_histogram(binwidth=100,color="black",boundary=0)+
  labs(y="Number of individuals",x="Years BP",title="Temporal distribution of EUR ancient individuals",
       subtitle="9000 to 2000 year BP",fill="Sex")+
  theme(axis.title.x=element_text(margin=margin(t=10),size=25),
        axis.title.y=element_text(margin=margin(r=10),size=25),
        plot.title=element_text(margin=margin(r=10,0,0,0),size=40,hjust=0.5),
        plot.subtitle=element_text(hjust=0.5,size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20,hjust=0.5))+
  scale_x_continuous(breaks=c(seq(2000,9000,1000)),limits=c(2000,9000),
                     labels=c(seq(2000,9000,1000)))+
  scale_fill_manual(values=c("F"="indianred","M"="royalblue","U"="black"),
                    labels=c("Female","Male","Unknown"))



#16 to 0#

fam1=read.table("individuals_16to0_m45_g50_dates.txt",header=TRUE)
head(fam1)
nrow(fam1)

ggplot(fam1,aes(x=(fromBP+toBP)/2))+
  geom_histogram(binwidth=100,color="black",fill="royalblue",boundary=0)+
  labs(y="Number of individuals",x="Years BP",title="Temporal distribution of ancient EUR individuals",
       subtitle="16000 to 0 years BP")+
  theme(axis.title.x=element_text(margin=margin(t=10),size=25),
        axis.title.y=element_text(margin=margin(r=10),size=25),
        plot.title=element_text(margin=margin(r=10,0,0,0),size=40,hjust=0.5),
        plot.subtitle=element_text(hjust=0.5,size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15))+
  scale_x_continuous(breaks=seq(0,16000,1000),limits=c(0,16000),
                     labels=seq(0,16000,1000))+
  scale_y_continuous(breaks=seq(0,70,5),limits=c(0,70),labels=seq(0,70,5))


fam2=read.table("individuals_16to0_m45_g50_dates.txt",header=TRUE)

fam2$lon=anno$V17[match(fam2$IID,anno$V2)]
fam2$lat=anno$V16[match(fam2$IID,anno$V2)]
fam2$meanBP=((fam2$fromBP+fam2$toBP)/2)
head(fam2)

euromid=c(lat=50.045074,lon=11.556853)
euromid=c(lon=18,lat=50)
euromap=get_map(location=euromid,zoom=4,crop=FALSE,maptype="terrain-background",
                scale=2)
fam2$lon=as.numeric(fam2$lon)
fam2$lat=as.numeric(fam2$lat)

p=ggmap(euromap)
p=p+geom_point(aes(x=lon,y=lat,color=meanBP),data=fam2,alpha=1,
               size=2)
p=p+labs(y="Latitude",x="Longitude",title="Geographic distribution of EUR ancient individuals",
         subtitle="16000 to 0 years BP",color="Years BP")
p=p+scale_colour_gradient(high="royalblue",low="indianred")
p=p+theme(axis.title.x=element_text(margin=margin(t=10),size=25),
          axis.title.y=element_text(margin=margin(r=10),size=25),
          plot.title=element_text(margin=margin(r=10,0,0,0),size=40,hjust=0.5),
          plot.subtitle=element_text(hjust=0.5,size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          legend.text=element_text(size=15),
          legend.title=element_text(size=20,hjust=0.5))
#guide_axis())
p=p+guides(color=guide_colorsteps())
print(p)



# BI 7 to 0 #


fam3=read.table("ind_7to0_UK_m45_g50_dates.txt",header=TRUE)
head(fam3)
nrow(fam3)

ggplot(fam3,aes(x=(fromBP+toBP)/2))+
  geom_histogram(binwidth=100,color="black",fill="royalblue",boundary=0)+
  labs(y="Number of individuals",x="Years BP",title="Temporal distribution of ancient BI individuals",
       subtitle="7000 to 0 years BP")+
  theme(axis.title.x=element_text(margin=margin(t=10),size=25),
        axis.title.y=element_text(margin=margin(r=10),size=25),
        plot.title=element_text(margin=margin(r=10,0,0,0),size=40,hjust=0.5),
        plot.subtitle=element_text(hjust=0.5,size=20),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15))+
  scale_x_continuous(breaks=seq(0,7000,1000),limits=c(0,7000),
                     labels=seq(0,7000,1000))+
  scale_y_continuous(breaks=seq(0,25,5),limits=c(0,25),labels=seq(0,25,5))


fam3$lon=anno$V17[match(fam3$IID,anno$V2)]
fam3$lat=anno$V16[match(fam3$IID,anno$V2)]
fam3$meanBP=((fam3$fromBP+fam3$toBP)/2)
fam3$lon=as.numeric(fam3$lon)
fam3$lat=as.numeric(fam3$lat)
head(fam3)


euromid=c(lon=-4.25,lat=55)
euromap=get_map(location=euromid,zoom=6,crop=FALSE,maptype="terrain-background",
                scale=2)


p=ggmap(euromap)
p=p+geom_point(aes(x=lon,y=lat,color=meanBP),data=fam3,alpha=1,
               size=2)
p=p+labs(y="Latitude",x="Longitude",title="Geographic distribution of BI ancient individuals",
         subtitle="7000 to 0 years BP",color="Years BP")
p=p+scale_colour_gradient(high="royalblue",low="indianred")
p=p+theme(axis.title.x=element_text(margin=margin(t=10),size=25),
          axis.title.y=element_text(margin=margin(r=10),size=25),
          plot.title=element_text(margin=margin(r=10,0,0,0),size=40,hjust=0.5),
          plot.subtitle=element_text(hjust=0.5,size=20),
          axis.text.x=element_text(size=15),
          axis.text.y=element_text(size=15),
          legend.text=element_text(size=15),
          legend.title=element_text(size=20,hjust=0.5))
#guide_axis())
p=p+guides(color=guide_colorsteps())
print(p)









