library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(tidyselect)
library(highcharter)
library(reshape2)
library(ggthemes)
library(extrafont)
library(lemon)
library(ggrepel)
library(kableExtra)
library(DT)
library(shiny)
library(leaflet)
library(sf)
library(cowplot)
library(shinyBS)
library(weights)
#install.packages("dplyr")
#require(devtools)
#install_version("sf", version = "3.4.2.", repos = "http://cran.us.r-project.org")
#Plan: Display data in charts split into five different tabs: Demographics, Water, Hygine, Sanitation, 
#and District-level
#The charts will display results overall by default for CHOLERA, FAMINE and BOTH, then dropdown menu will 
#allow disaggregation by HoH gender, IDP status and to isolate results by district


#file.exists("WASHdashboard/www/REACH OFFICIAL Logo.jpg")
#Read files
dataT<-as.data.frame(read_csv("WASHdata.csv"))
Dist <- read_sf("extdata/YEM_adm2_Districts.shp")
Gov<- read_sf("extdata/YEM_adm1_Governorates.shp")

#subset and transform to lat/lng
Gov<-st_transform( Gov[,c(4,6)], 4326)
Dist<- st_transform(Dist[,c(4,7,9)], 4326)

#subset to surveyed districts
Dist<-Dist %>% 
  filter(admin2pcod %in% c("YE1117", "YE1210", "YE1211", "YE1415", "YE1502", 
                           "YE1512", "YE1514", "YE1520", "YE1604", "YE1606", 
                           "YE1607", "YE1704", "YE1706", "YE1707", "YE1712",
                           "YE1716", "YE1730", "YE1731",
                           "YE1906", 
                           "YE2113", "YE2210",
                           "YE2213", "YE2214", "YE2302", "YE2310", "YE2314",
                           "YE2510", "YE2512", "YE2515", "YE2612", "YE2613", 
                           "YE2707", "YE2909", "YE2910", "YE2918", "YE2919",
                           "YE2920","YE3106"))
Dist<-arrange(Dist, admin2pcod)

Dist2<-Dist
#appending priority to district
Dist2$Prio<-c("Famine","Famine","Famine","Cholera","Famine",
              "Famine","Famine","Famine","Cholera","Cholera",
              "Cholera","Famine","Both","Cholera","Both",
              "Cholera","Cholera","Cholera",
              "Famine",
              "Famine","Famine",
              "Famine","Both","Cholera","Both","Famine",
              "Famine","Famine","Famine","Cholera","Cholera",
              "Famine","Famine","Cholera","Cholera","Both",
              "Famine","Cholera")


# c("Famine","Famine","Cholera","Cholera","Both",
#   "Cholera","Cholera","Cholera","Cholera", "Famine",
#   "Famine", "Both","Cholera","Both","Cholera", 
#   "Cholera","Cholera", "Cholera",
#   "Famine",
#   "Famine", "Famine",
#   "Both","Famine", "Cholera", "Famine", "Famine", 
#   "Famine", "Famine", "Cholera",'Famine', "Famine", 
#   "Famine","Famine","Famine","Famine", "Cholera",
#   "Both", "Famine")


#List for Disstrict-Level tab dropdown menu
distlist<-c( "Abs","Al Abr", "Al Hashwah","Al Jafariyah","Al Maqatirah","Al Maslub","Al Maton","Al Miftah","Al Qabbaytah","Arhab","Ash Shamayatayn",
             "Aslem","At Ta'iziyah","Ataq","Az Zahir","Bani Dhabyan","Bani Sa'd","Bani Suraim","Dhi Bin","Dimnat Khadir",
             "Iyal Surayh","Khamir","Khanfir","Kharif","Kitaf wa Al Boqe'e","Kushar","Majz","Manakhah","Marib","Marib City",
             "Mudhaykhirah","Mustaba","Qarah","Shara'b As Salam","Tuban","Wald Rabi'", "Washhah", "Zingibar")


#top reference map polygon color palette
factpal <- colorFactor(c("#009999", "#666666","#FFFFFF"), Dist2$Prio)
Dist2

