library(tidyverse)
library(readxl)
library(rgdal)
library(ggspatial)
library(fuzzyjoin)
setwd("PATHHERE")

#SOURCES
#---covid data
#https://data.ontario.ca/dataset/status-of-covid-19-cases-in-ontario-by-public-health-unit-phu
#---PHU map
#https://geohub.lio.gov.on.ca/datasets/lio::ministry-of-health-public-health-unit-boundary/about
#---phu population
#https://www.cbc.ca/news/canada/kitchener-waterloo/chart-data-active-covid-19-cases-ontario-1.5591686
#---fuzzymatching
#https://stackoverflow.com/questions/26405895/how-can-i-match-fuzzy-match-strings-from-two-datasets

casedata <- read.csv("cases_by_status_and_phu.csv")
#Huron perth data on this date is missing the 
#health unit name and ID, but cases are still present
which(is.na(casedata$PHU_NUM))
casedata[10779,2] <- "HURON PERTH"
casedata[10779,3] <- 5183
#Some dates are duplicated, remove these
casedata <- casedata[!duplicated(casedata),]

#format to wide with each date being a column 
#and each PHU a row for easier ggplotting
casedata <- casedata[,1:4] %>% 
  group_by(PHU_NUM) %>% 
  pivot_wider(names_from=FILE_DATE, values_from = ACTIVE_CASES)

#PHU population data
phupop <- read.csv("phupop.csv")
#MATCHING PHU POP TO PHU in 1 data frame
#(fuzzy matching didn't quite work, corrected names in one dataframe to help it)
casedata[casedata$PHU_NUM==4913,]$PHU_NAME <- "Southwestern"
casedata[casedata$PHU_NUM==2237,]$PHU_NAME <- "Hamilton Public Health Services"
casedata[casedata$PHU_NUM==2251,]$PHU_NAME <- "Ottawa Public Health"

casedata <- stringdist_join(phupop,casedata,by="PHU_NAME",mode="left",ignore_case=T,method="jw",max_dist=99,distance_col="dist") %>% 
  group_by(PHU_NAME.x)%>% 
  slice_min(order_by=dist,n=1)
colnames(casedata)[3] <-"PHU_NAME"
casedata <- casedata[,-1]
casedata <- casedata[,c(2,1,3:ncol(casedata))]

#Getting cases/100k for each date column
casedata[,4:ncol(casedata)] <- t(t(casedata[,4:ncol(casedata)]))/casedata$POP*100000

#truncate values to keep colour range sensible, can change or remove this
casedata[,4:ncol(casedata)] <- casedata[,4:ncol(casedata)] %>% 
  mutate_all(funs(ifelse(.>=300, 300, .)))


###### MAP DATA
ontario <- readOGR(dsn = "./PHUshapes/Ministry_of_Health_Public_Health_Unit_Boundary.shp", 
                   stringsAsFactors = T)

#Join map data to case data for easy ggplotting
ontario@data <- left_join(ontario@data,casedata[,-c(1,2)], by=c("PHU_ID"="PHU_NUM")) 


dates <- colnames(ontario@data)[10:(length(colnames(ontario@data))-1)]

#Looping to create a plot for each date
for(i in 1:length(dates)){
  print(i)
  ggplot() +
    annotation_spatial(ontario) +
    layer_spatial(ontario,aes_string(fill=paste0("`", dates[i], "`")))+
    scale_fill_gradient2(low="white",mid="orange",high="red",
                         midpoint=150,
                         breaks=seq(0,300,100),
                         labels = c(0,100,200,">300"),
                         limits =c(0,300), 
                         guide = guide_colourbar(direction = "horizontal", 
                                                 title.position = "bottom",
                                                 title.hjust = 0.5,
                                                 title = "Active Cases per 100k",
                                                 barwidth = 35, 
                                                 barheight = 0.8,
                                                 frame.colour = "black"))+
    #cropped large PHU in northern ontario, otherwise hard to see rest of ontario
    coord_sf(xlim=c(-96,-74),ylim=c(41.6,50),expand=F)+ 
    theme(legend.position = "bottom", #legend below map
          axis.line=element_blank(),  #bunch of options to remove "graph" visuals
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          legend.text=element_text(size=10),  #legend text sizes
          legend.title=element_text(size=13))+
    annotate("text",x=-92,y=43,label=paste0(dates[i]),size=8)+
    annotate("text",x=-77.5,y=41.8,label="Source: Ontario Ministry of Health",size=2)#add date
  ggsave(paste0(i,".png"),path="PATHHERE",width=9.6,height=5.4,units="in",dpi=200)
}
