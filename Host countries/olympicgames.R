library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(factoextra)
library(gridExtra)
library(tidyr)
library(NbClust)
library(GGally)
library(hrbrthemes)
library(viridis)
library(forcats)
library(RColorBrewer)
library(plyr)
library(stringr)
library(ggrepel)
library(scales)
library(forcats)
library(lares)
library(knitr)
library(ggridges)
library(gganimate)

#Import data set 
athlete_events <- read_csv("Desktop/Project_4_SCV/athlete_events.csv")

#Palette:
color <- brewer.pal(9, 'YlOrRd' )
pal <- colorRampPalette(color)

#Select only year after 1976
athlete_events = athlete_events[-which(athlete_events$Year < 1976),]

#Remove missing values:
athlete_events = na.omit(athlete_events)

#Summer Games: 
athlete_events_summer = athlete_events[which(athlete_events$Season == "Summer"),]
athlete_events_winter = athlete_events[which(athlete_events$Season == "Winter"),]

#Problematic: Does the host countries win more medals ? 
table(as.factor(athlete_events_winter$City))
# 11 different towns, Country: France, USA ( *2), Italy(*2), Austria, Norway, Japan, Russia, Canada,Bosnia and Herzegovina
#France: Albertville: 
france_host <- athlete_events_winter[which(athlete_events_winter$City == "Albertville"),]
USA_host <- athlete_events_winter[which(athlete_events_winter$City == "Lake Placid" | athlete_events_winter$City == "Salt Lake City"),]
Italy_host <- athlete_events_winter[which(athlete_events_winter$City == "Calgary" | athlete_events_winter$City == "Torino"),]
Austria_host <- athlete_events_winter[which(athlete_events_winter$City == "Innsbruck"),]
Norway_host <- athlete_events_winter[which(athlete_events_winter$City == "Lillehammer"),]
Japan_host <- athlete_events_winter[which(athlete_events_winter$City == "Nagano"),]
Russia_host <- athlete_events_winter[which(athlete_events_winter$City == "Sochi"),]
Canada_host <- athlete_events_winter[which(athlete_events_winter$City == "Vancouver"),]
BH_host <- athlete_events_winter[which(athlete_events_winter$City == "Sarajevo"),]

#French Team: 
team_french <- france_host[which(france_host$Team == "France"),]
#French Medals: 
france_gold <- length(which(team_french$Medal == "Gold")) 
france_silver <- length(which(team_french$Medal == "Silver")) 
france_bronze <- length(which(team_french$Medal == "Bronze")) 
#Proportion French Medals: 
prop_france_gold <- length(which(team_french$Medal == "Gold"))/(length(which(france_host$Medal == "Gold")))
prop_france_silver <- length(which(team_french$Medal == "Silver"))/(length(which(france_host$Medal == "Silver")))
prop_france_bronze <- length(which(team_french$Medal == "Bronze"))/(length(which(france_host$Medal == "Bronze"))) 
prop_france_total <- length(team_french$ID)/length(france_host$ID)
#Pie chart1: 
#Number of Medals by country
count1 <- count(france_host, vars="Team")
#Remove countries who win only a small number of medals: China, Canada-1,France-1,Germany-1,Germany-2,Italiy-1,New Zealand, North Korea,Spain,Switerland,Luxembourg,Netherlands,
count1$percentage <- 100*count1$freq /sum(count1$freq)
count1 <- format(count1,digits=2,justfy="none")
count1 <- type.convert(count1)
count1 <- count1[-c(4,5,9,11,12,14,16,17,18,19,22,24,27,28,21,2,25),]

ggplot(count1,aes(x="",y=percentage,fill=Team)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=pal(17)) +labs(x="",y="Medals") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank())

#Pie chart2 : Number of Medals won by French Team in Winter games: 
fr_med_fr <- length(which(france_host$Team == "France"))
fr_med_it <- length(which(Italy_host$Team == "France"))
fr_med_aus <- length(which(Austria_host$Team == "France"))
fr_med_usa <- length(which(USA_host$Team == "France"))
fr_med_jap <- length(which(Japan_host$Team == "France"))
fr_med_rus <- length(which(Russia_host$Team == "France"))
fr_med_can <- length(which(Canada_host$Team == "France"))
fr_med_nor <- length(which(Norway_host$Team == "France"))
fr_med_bh <- length(which(BH_host$Team == "France"))
y <- c(fr_med_aus,fr_med_bh,fr_med_can,fr_med_fr,fr_med_it,fr_med_jap,fr_med_nor,fr_med_rus,fr_med_usa)
count2 <- data.frame(c("Austria","BH","Canda","France","Italy","Japan","Norway","Russia","USA"),y) 
count2$percentage <- 100*count2$y /sum(count2$y)
count2 <- format(count2,digits=2,justfy="none")
count2 <- type.convert(count2)
colnames(count2) <- c("Country","Count","Percentage")
ggplot(count2,aes(x="",y=Count, fill=Country)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=pal(17)) +labs(x="",y="Medals") + theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank()) + geom_text(aes(x=1.6,label=paste0(Percentage,"%")),position=position_stack(vjust=0.5))

#Russia: 
team_russia <- Russia_host[which(Russia_host$Team == "Russia"),]
#Proportion:
prop_russia_gold <- length(which(team_russia$Medal == "Gold"))/(length(which(Russia_host$Medal == "Gold")))
prop_russia_silver <- length(which(team_russia$Medal == "Silver"))/(length(which(Russia_host$Medal == "Silver")))
prop_russia_bronze <- length(which(team_russia$Medal == "Bronze"))/(length(which(Russia_host$Medal == "Bronze"))) 
prop_russia_total <- length(team_russia$ID)/length(Russia_host$ID)

#Pie chart1: 
#Number of Medals by country
count3 <- count(Russia_host, vars="Team")
#Remove countries who win only a small number of medals: China, Canada-1,France-1,Germany-1,Germany-2,Italiy-1,New Zealand, North Korea,Spain,Switerland,Luxembourg,Netherlands,
count3$percentage <- 100*count3$freq /sum(count3$freq)
count3 <- format(count3,digits=2,justfy="none")
count3 <- type.convert(count3)
count3 <- count3[-c(1,3,4,6,7,8,9,14,16,13,17,18,19,24,25,26,30,31,34.22,33),]
count3$percentage <- 100*count3$freq/sum(count3$freq)

ggplot(count3,aes(x="",y=percentage,fill=Team)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=pal(19)) +labs(x="",y="Medals") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank())

#Italia: 
team_italy <- Italy_host[which(Italy_host$Team == "Italy"),]
#Proportion: 
prop_italy_gold <- length(which(team_italy$Medal == "Gold"))/(length(which(Italy_host$Medal == "Gold")))
prop_italy_silver <- length(which(team_italy$Medal == "Silver"))/(length(which(Italy_host$Medal == "Silver")))
prop_italy_bronze <- length(which(team_italy$Medal == "Bronze"))/(length(which(Italy_host$Medal == "Bronze")))
prop_italy_total <- length(team_italy$Name)/length(Italy_host$ID)

#Austria: 
team_austria <- Austria_host[which(Austria_host$Team == "Austria"),]
#Proportion: 
prop_austria_gold <- length(which(team_austria$Medal == "Gold"))/(length(which(Austria_host$Medal == "Gold")))
prop_austria_silver <- length(which(team_austria$Medal == "Silver"))/(length(which(Austria_host$Medal == "Silver")))
prop_austria_bronze <- length(which(team_austria$Medal == "Bronze"))/(length(which(Austria_host$Medal == "Bronze")))
prop_austria_total <- length(team_austria$Name)/length(Austria_host$ID)

#USA: 
team_USA <- USA_host[which(USA_host$Team == "United States"),]
#Proportion: 
prop_USA_gold <- length(which(team_USA$Medal == "Gold"))/(length(which(USA_host$Medal == "Gold")))
prop_USA_silver <- length(which(team_USA$Medal == "Silver"))/(length(which(USA_host$Medal == "Silver")))
prop_USA_bronze <- length(which(team_USA$Medal == "Bronze"))/(length(which(USA_host$Medal == "Bronze")))
prop_USA_total <-  length(team_USA$ID)/length(USA_host$ID)

#Norway: 
team_norway <- Norway_host[which(Norway_host$Team == "Norway"),]
#Proportion: 
prop_norway_gold <- length(which(team_norway$Medal == "Gold"))/(length(which(Norway_host$Medal == "Gold")))
prop_norway_silver <- length(which(team_norway$Medal == "Silver"))/(length(which(Norway_host$Medal == "Silver")))
prop_norway_bronze <- length(which(team_norway$Medal == "Bronze"))/(length(which(Norway_host$Medal == "Bronze")))
prop_norway_total <- length(team_norway$ID)/length(Norway_host$ID)

#Japan: 
team_japan <- Japan_host[which(Japan_host$Team == "Japan"),]
#Proportion: 
prop_japan_gold <- length(which(team_japan$Medal == "Gold"))/(length(which(Japan_host$Medal == "Gold")))
prop_japan_silver <- length(which(team_japan$Medal == "Silver"))/(length(which(Japan_host$Medal == "Silver")))
prop_japan_bronze <- length(which(team_japan$Medal == "Bronze"))/(length(which(Japan_host$Medal == "Bronze")))
prop_japan_total <- length(team_japan)/length(Japan_host$ID)

#BH: 
team_bh <- BH_host[which(BH_host$Team == "Yugoslavia"),]

#Canada: 
team_canada <- Canada_host[which(Canada_host$Team == "Canada"),]
#Proportion: 
prop_canada_gold <- length(which(team_canada$Medal == "Gold"))/(length(which(Canada_host$Medal == "Gold")))
prop_canada_silver <- length(which(team_canada$Medal == "Silver"))/(length(which(Canada_host$Medal == "Silver")))
prop_canada_bronze <- length(which(team_canada$Medal == "Bronze"))/(length(which(Canada_host$Medal == "Bronze")))
prop_canada_total <- length(team_canada$ID)/length(Canada_host$ID)

#Histogram of proportion the medals won by a host country:
count_world <- c(prop_france_total,prop_italy_total,prop_austria_total,prop_USA_total,prop_norway_total,prop_japan_total,prop_russia_total,prop_canada_total)
country <- c("France","Italy","Austria","USA","Norway","Japan","Russia","Canada")
data_histo <- data.frame(country,count_world)
ggplot(data_histo,aes(x=country,y=count_world,fill=country)) + geom_col() + labs(c="Genre",y="Log of the Budget") + theme(axis.text.x = element_text(angle=90)) + scale_fill_manual(values= pal(15)) + ylim(c(0,1))

#Summer Games
table(as.factor(athlete_events_summer$City))
#City: Athina(Greece), Atlanta ( USA), Barcelona ( Spain), Beijing(China), London(England), L.A ( USA), Montreal ( Canada), Moskva ( Russia), Rio de Janeiro ( Brasil), Seoul ( South Corea), Sydney ( Australia)
Greece_host <- athlete_events_summer[which(athlete_events_summer$City == "Athina"),]
USA_host_summer <- athlete_events_summer[which(athlete_events_summer$City == "Atlanta" | athlete_events_summer$City == "Los Angeles"),]
Spain_host <- athlete_events_summer[which(athlete_events_summer$City == "Barcelona"),]
China_host <- athlete_events_summer[which(athlete_events_summer$City == "Beijing"),]
England_host <- athlete_events_summer[which(athlete_events_summer$City == "London"),]
Canada_host_summer <- athlete_events_summer[which(athlete_events_summer$City == "Montreal"),]
Russia_host_summer <- athlete_events_summer[which(athlete_events_summer$City == "Moskva"),]
Brasil_host <- athlete_events_summer[which(athlete_events_summer$City == "Rio de Janeiro"),]
South_Corea_host <- athlete_events_summer[which(athlete_events_summer$City == "Seoul"),]
Australia_host <- athlete_events_summer[which(athlete_events_summer$City == "Sydney"),]

#China: 
#China Team: 
team_china <- China_host[which(China_host$Team == "China"),]
#China Medals: 
china_gold <- length(which(team_china$Medal == "Gold")) 
china_silver <- length(which(team_china$Medal == "Silver")) 
china_bronze <- length(which(team_french$Medal == "Bronze")) 
#Proportion China Medals: 
prop_china_gold <- length(which(team_china$Medal == "Gold"))/(length(which(China_host$Medal == "Gold")))
prop_china_silver <- length(which(team_china$Medal == "Silver"))/(length(which(China_host$Medal == "Silver")))
prop_china_bronze <- length(which(team_china$Medal == "Bronze"))/(length(which(China_host$Medal == "Bronze"))) 
prop_china_total <- length(team_china$ID)/length(China_host$ID)
#Pie chart1: 
#Number of Medals by country
count4 <- count(China_host, vars="Team")
#Remove countries who win only a small number of medals:
count4$percentage <- 100*count4$freq /sum(count4$freq)
count4 <- format(count4,digits=2,justfy="none")
count4 <- type.convert(count4)
count4 <- count1[-c(1,2,4,6,7,8,10,11,13,14,15,16,18,21,22,20,23,24,25,27,29,30,31,32,33,34,35,36,37,39,42,45,46,47,48,49,50,56,57,57,59,60,61,62,63,64,65,69,71,73,76,77,79,80,81,82,84,86,87,88,89,90,92,93,94,95,96,97,100,101,102,103,104,105),]
count4$percentage <- 100*count4$freq /sum(count4$freq)
count4 <- format(count4,digits=2,justfy="none")
count4 <- type.convert(count4)

ggplot(count4,aes(x="",y=percentage,fill=Team)) + geom_bar(stat="identity") + coord_polar("y") + scale_fill_manual(values=pal(32)) +labs(x="",y="Medals") + geom_text(aes(x=1.6,label=paste0(percentage,"%")),position=position_stack(vjust=0.5))+ theme(panel.background = element_blank(),axis.line = element_blank(),axis.text = element_blank(),axis.ticks = element_blank(),axis.title = element_blank())

#Greece : 
team_greece <- Greece_host[which(Greece_host$Team == "Greece"),]
#Proportion: 
prop_greece_gold <- length(which(team_greece$Medal == "Gold"))/(length(which(Greece_host$Medal == "Gold")))
prop_greece_silver <- length(which(team_greece$Medal == "Silver"))/(length(which(Greece_host$Medal == "Silver")))
prop_greece_bronze <- length(which(team_greece$Medal == "Bronze"))/(length(which(Greece_host$Medal == "Bronze")))
prop_greece_total <- length(team_greece$ID)/length(Greece_host$ID)

#USA summer: 
team_USA_summer <- USA_host_summer[which(USA_host_summer$Team == "United States"),]
#Proportion: 
prop_usa_summer_gold <- length(which(team_USA_summer$Medal == "Gold"))/(length(which(USA_host_summer$Medal == "Gold")))
prop_usa_summer_silver <- length(which(team_USA_summer$Medal == "Silver"))/(length(which(USA_host_summer$Medal == "Silver")))
prop_usa_summer_bronze <- length(which(team_USA_summer$Medal == "Bronze"))/(length(which(USA_host_summer$Medal == "Bronze")))
prop_usa_summer_total <- length(team_USA_summer$ID)/length(USA_host_summer$ID)

#Spain : 
team_spain <- Spain_host[which(Spain_host$Team == "Spain"),]
#Proportion: 
prop_spain_gold <- length(which(team_spain$Medal == "Gold"))/(length(which(Greece_host$Medal == "Gold")))
prop_spain_silver <- length(which(team_spain$Medal == "Silver"))/(length(which(Greece_host$Medal == "Silver")))
prop_spain_bronze <- length(which(team_spain$Medal == "Bronze"))/(length(which(Greece_host$Medal == "Bronze")))
prop_spain_total <- length(team_spain$ID)/length(Spain_host$ID)

#England: 
team_england <- England_host[which(England_host$Team == "Great Britain"),]
#Proportion: 
prop_england_gold <- length(which(team_england$Medal == "Gold"))/(length(which(England_host$Medal == "Gold")))
prop_england_silver <- length(which(team_england$Medal == "Silver"))/(length(which(England_host$Medal == "Silver")))
prop_england_bronze <- length(which(team_england$Medal == "Bronze"))/(length(which(England_host$Medal == "Bronze")))
prop_england_total <- length(team_england$ID)/length(England_host$ID)

#Canada: 
team_canada_summer <- Canada_host_summer[which(Canada_host_summer$Team == "Canada"),]
#Proportion: 
prop_canada_summer_gold <- length(which(team_canada_summer$Medal == "Gold"))/(length(which(Canada_host_summer$Medal == "Gold")))
prop_canada_summer_silver <- length(which(team_canada_summer$Medal == "Silver"))/(length(which(Canada_host_summer$Medal == "Silver")))
prop_canada_summer_bronze <- length(which(team_canada_summer$Medal == "Bronze"))/(length(which(Canada_host_summer$Medal == "Bronze")))
prop_canada_summer_total <- length(team_canada_summer$ID)/length(Canada_host_summer$ID)

#Russia: 
team_russia_summer <- Russia_host_summer[which(Russia_host_summer$Team == "Soviet Union"),]
#Proportion: 
prop_russia_summer_gold <- length(which(team_russia_summer$Medal == "Gold"))/(length(which(Russia_host_summer$Medal == "Gold")))
prop_russia_summer_silver <- length(which(team_russia_summer$Medal == "Silver"))/(length(which(Russia_host_summer$Medal == "Silver")))
prop_russia_summer_bronze <- length(which(team_russia_summer$Medal == "Bronze"))/(length(which(Russia_host_summer$Medal == "Bronze")))
prop_russia_summer_total <- length(team_russia_summer$ID)/length(Russia_host_summer$ID)

#Brasil: 
team_brasil <- Brasil_host[which(Brasil_host$Team == "Brazil"),]
#Proportion: 
prop_brasil_gold <- length(which(team_brasil$Medal == "Gold"))/(length(which(Brasil_host$Medal == "Gold")))
prop_brasil_silver <- length(which(team_brasil$Medal == "Silver"))/(length(which(Brasil_host$Medal == "Silver")))
prop_brasil_bronze <- length(which(team_brasil$Medal == "Bronze"))/(length(which(Brasil_host$Medal == "Bronze")))
prop_brasil_total <- length(team_brasil$ID)/length(Brasil_host$ID)

#South Korea: 
team_south_corea <- South_Corea_host[which(South_Corea_host$Team == "South Korea"),]
#Proportion: 
prop_sk_gold <- length(which(team_south_corea$Medal == "Gold"))/(length(which(South_Corea_host$Medal == "Gold")))
prop_sk_silver <- length(which(team_south_corea$Medal == "Silver"))/(length(which(South_Corea_host$Medal == "Silver")))
prop_sk_bronze <- length(which(team_south_corea$Medal == "Bronze"))/(length(which(South_Corea_host$Medal == "Bronze")))
prop_sk_total <- length(team_south_corea$ID)/length(South_Corea_host$ID)

#Australia: 
team_australia <- Australia_host[which(Australia_host$Team == "Australia"),]
#Proportion: 
prop_australia_gold <- length(which(team_australia$Medal == "Gold"))/(length(which(Australia_host$Medal == "Gold")))
prop_australia_silver <- length(which(team_australia$Medal == "Silver"))/(length(which(Australia_host$Medal == "Silver")))
prop_australia_bronze <- length(which(team_australia$Medal == "Bronze"))/(length(which(Australia_host$Medal == "Bronze")))
prop_australia_total <- length(team_australia$ID)/length(Australia_host$ID)

#Histogram of proportion the medals won by a host country:
count_world2 <- c(prop_greece_total,prop_USA_total_summer,prop_spain_total,prop_china_total,prop_england_total,prop_canada_summer_total,prop_russia_summer_total,prop_brasil_total,prop_sk_total,prop_australia_total)
country2 <- c("Greece","USA","Spain","China","England","Canada","Russia","Brasil","Sk","Australia")
data_histo2 <- data.frame(country2,count_world2)
ggplot(data_histo2,aes(x=country2,y=count_world2,fill=country2)) + geom_col() + labs(c="Genre",y="Log of the Budget") + theme(axis.text.x = element_text(angle=90)) + scale_fill_manual(values= pal(15)) + ylim(c(0,1))

#Has the number of countries increased throughout time ? 

numbers <- athlete_events %>% group_by(Year,Season) %>% summarize(Nations = length(unique(NOC)))

ggplot(numbers,aes(x=Year,y=Nations,color=Season)) + geom_point(size=2) + geom_line() + scale_color_manual(values=pal(16))

#Which nation win the most medals ? 
medal_counts <- athlete_events %>% group_by(NOC,Medal,Event,Games) %>% summarize(isMedal=1)
medal_counts <- medal_counts %>% group_by(NOC,Medal) %>% summarize(Count=sum(isMedal))

levelsTeam <- medal_counts %>% group_by(NOC) %>% summarize(Total=sum(Count)) %>% arrange(desc(Total)) %>% select(NOC) %>% slice(30:1)

medal_counts$NOC <- factor(medal_counts$NOC,levels=levelsTeam$NOC)

medal_counts <- medal_counts %>% filter(NOC != "NA")

ggplot(medal_counts, aes(x=NOC, y=Count, fill=Medal)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values=pal(3)) +
  labs(x = "Nations", y = "Count")

#Animated plot: 
medal_counts <- athlete_events %>% filter(!is.na(Medal)) %>% group_by(NOC,Medal,Event,Games,Year) %>% summarize(isMedal=1)

medal_counts <- medal_counts %>% group_by(NOC,Medal,Year) %>% summarize(Count=sum(isMedal))

medal_counts <- medal_counts %>% select(Medal,NOC,Year,Count)

levelsTeam <- medal_counts %>% group_by(NOC) %>% summarize(Total=sum(Count)) %>% arrange(desc(Total)) %>% select(NOC) %>% slice(10:1)

medal_counts$NOC <- factor(medal_counts$NOC,levels=levelsTeam$NOC)

ggplot(medal_counts,aes(x=NOC,y=Count,fill=Medal)) + transition_time(Year) + geom_col() + coord_flip() + scale_fill_manual(values=pal(3))

#Link GDP and number of medal by country: 
athlete_events_2020 <- athlete_events[which(athlete_events$Year == 2016),]
data_country <- economic_freedom_index2019_data[c(3,5,6,7,8,9,10,11,14,15,22,24,27,30,35,36,41,42,43,45,46,49,51,55,57,58,59,60,63,64,66,74,76,77,78,80,81,82,83,84,85,86,87,91,101,107,113,116,118,122,123,125,126,127,134,135,136,137,138,139,147,150,151,152,155,156,160,161,164,166,170,171,172,175,176,178,180,182,183),]
athlete_events_2020 <- athlete_events_2020[- which(athlete_events_2020$Team == "Brazil-1"),]
athlete_events_2020 <- athlete_events_2020[- which(athlete_events_2020$Team == "Czech Republic-1"),]
athlete_events_2020 <- athlete_events_2020[- which(athlete_events_2020$Team == "Germany-1"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Chinese Taipei"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Grenada"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Indonesia-1"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Italy-1"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Netherlands-1"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Puerto Rico"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Russia-2"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "South Korea-1"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Spain-2"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "United States-2"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "United States-1"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Individual Olympic Athletes"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "China-1"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "Great Britain"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "North Korea"),]
athlete_events_2020 <- athlete_events_2020[-which(athlete_events_2020$Team == "South Korea"),]

athlete_events_2020 <- athlete_events_2020[order(athlete_events_2020$Team),]
count5 <- count(athlete_events_2020,vars=Team)
combined <- data.frame(count5$vars,count5$n,data_country$`GDP per Capita (PPP)`)
colnames(combined) <- c("Country","Medals","GDP per capita")
combined$`GDP per capita` <- substr(combined$`GDP per capita`,2,9)
combined$`GDP per capita` <- as.numeric(combined$`GDP per capita`)

plot(combined$`GDP per capita`,combined$Medals) + xlim(c(0,100)) + ylim(c(0,100))
abline(lm(combined$Medals ~ combined$`GDP per capita`))

cor(combined$Medals,combined$`GDP per capita`)

#Link population and number of medals: 
combined2 <- data.frame(count5$vars,count5$n,data_country$`Population (Millions)`)
colnames(combined2) <- c("Country","Medals","Population")
combined2$Population <- as.numeric(combined2$Population)

plot(combined2$Population,combined2$Medals) + xlim(c(0,100)) + ylim(c(0,100))
abline(lm(combined2$Medals ~ combined2$Population))
cor(combined2$Medals,combined2$Population)


