library(ggplot2)
library(ggmap)
UC = read.csv("~/study/UCD/2016fall/141A/hw3/uc universities.csv",header = T)
UCBc = "Berkeley,CA"
UCLAc = "Los Angeles,CA"
UCSBc = "Santa Barbara,CA"
UCIc = "Irvine,CA"
UCDc = "1 Shields Ave, Davis, CA"
UCSDc = "9500 Gilman Dr, La Jolla, CA"
UCSCc = "1156 High St, Santa Cruz, CA"
UCRc = "900 University Ave, Riverside, CA"
UCMc = "5200 Lake Rd, Merced, CA"
UC$address = rbind(UCBc,UCLAc,UCSBc,UCIc,UCDc,UCSDc,UCSCc,UCRc,UCMc)
UC$address <-paste0("UC ",UC$name,",California,USA")
UC$address
UC$location <-geocode(UC$address)
UC$location[7,] <-geocode("156 High St, Santa Cruz, CA 95064")

CA = get_map("California",zoom=6,maptype="satellite")
ggmap(CA)
ca.state = map_data("state","california")
names(ca.state$long)="lon"
pl.ca = ggmap(CA) + geom_polygon(aes(x=long,y=lat,group=group), fill=NA, col="white",data = ca.state)
pl.ca
uc.cities = us.cities[us.cities$name %in% c("Berkeley CA","Los Angeles CA","Santa Barbara CA","Irvine CA",
                                            "Davis CA","San Diego CA", "Santa Cruz CA","Riverside CA","Merced CA"),]
uc.cities$name <-sub('...$','',uc.cities$name) 
#
#pl.uc <-pl.ca+ geom_point(data =uc.cities, aes(x = long, y = lat),col="violet", size=3)
#pl.uc+ geom_text(aes(label=name,x=long,y=lat),size=3,col="white",data=uc.cities)
pl.uc <-pl.ca+ geom_point(data =UC, aes(x = location$lon, y = location$lat),col="violet", size=3)+geom_text(aes(label=name,x = location$lon, y = location$lat),size=3,col="white",data=UC)
pl.uc
##1.2-------------------------------------------------
#university <-merge(uc.cities, UC, by = "name", all = TRUE)
#
#pl.un <-pl.ca+ geom_point(data = university, aes(x = long, y = lat,col=heat.colors(9), alpha=Percent,size=Undergraduate.Enrollment))+ geom_text(aes(label=name,x=long,y=lat),size=3,col="pink",data=university)
colorfun <- colorRamp(c("red","purple"), space="Lab")
pl.un <-pl.ca+geom_point(data = UC, aes(x = location$lon, y = location$lat,col=factor(Rank),size=Undergraduate.Enrollment))+geom_point(data=UC, aes(x = location$lon, y = location$lat, size=percent,alpha=0.6))+ geom_text(aes(label=name,x = location$lon, y = location$lat),angle=30,size=3,col="white",data=UC)
pl.un

##1.3 why they have bicycle road?
combination <-combn(UC$address,2)
#names(combination)[1:2] <-c("from","to")
#
paths <-mapply(function(from,to){
  path=route(from,to, structure="route",mode="bicycling")
  path$fromto=paste0(from,to)
  if(!is.data.frame(path))
    return(NULL)
  return (path)
},combination[1,],combination[2,],SIMPLIFY=FALSE)
if (file.exists("~/study/UCD/2016fall/141A/hw3/paths.rds")) {
  # Load the routes from disk if they're cached.
  paths = readRDS("~/study/UCD/2016fall/141A/hw3/paths.rds")
} else {
  # Query Google Maps for the routes.
  # paths = mapply(...)
  saveRDS(paths, "~/study/UCD/2016fall/141A/hw3/paths.rds")
}
paths=do.call(rbind,paths)

#
pl.ca+sapply(1:36, function(i) geom_path(aes(x=lon,y=lat),size=0.5,col="yellow",data=paths[[i]]))+ geom_point(data =UC, aes(x = location$lon, y = location$lat),col="violet", size=3)+geom_text(aes(label=name,x = location$lon, y = location$lat),size=3,col="white",data=UC)
#trek_df <- route("UC Berkeley, CA","UC Los Angeles, CA", structure = "route",mode="bicycling")
#  Source : https://maps.googleapis.com/maps/api/directions/json?origin=houson%2C%20texas&destination=waco%2C%20texas&mode=driving&units=metric&alternatives=false
#trek couldnt find it

##2.1--------------------------------------------------
#get_googlemap("waco texas", zoom = 12, maptype = "hybrid")
library(tidyverse)
road=numeric(0)
for(i in 1:36) {
  road<-rbind(road,mapdist(combination[1,i],combination[2,i],mode="driving"))}

roads <-mapply(function(from,to){
  road=mapdist(from,to,mode="driving")
  road$fromto=paste0(from,to)
  if(!is.data.frame(road))
    return(NULL)
  return (road)
},combination[1,],combination[2,],SIMPLIFY=FALSE)
roads <-do.call(rbind,roads)
road100 <-roads[which(road$miles<100),]


for(i in 1:36) {if(road$miles<=100) gr <-gr+edge(road[i,1],road[i,2])}

saveRDS(road, "~/study/UCD/2016fall/141A/hw3/road.rds")
library(igraph)
gr <-graph.formula(1,2,3,4,5,6,7,8,9)
 V(gr)$name = UC$address

for(i in 1:36) {if(road[i,5]<=100) {gr <-gr+edge(road[i,1],road[i,2])}}
 V(gr)$name=c("UCB","UCLA" ,"UCSB", "UCI","UCD","UCSD" , "UCSC","UCR","UCM")
plot(gr)
##2.3-----------------------------------------------------------------
V(gr)$name = UC$address
smallUC=UC$address[UC$Undergraduate.Enrollment<20000]

V(gr)$color=ifelse(V(gr)$name %in% smallUC, "red", "green")
V(gr)$name=c("UCB","UCLA" ,"UCSB", "UCI","UCD","UCSD" , "UCSC","UCR","UCM")
plot(gr)


