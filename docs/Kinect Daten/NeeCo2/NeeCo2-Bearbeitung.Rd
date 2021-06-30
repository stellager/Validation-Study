
rm(list=all)
library(tidyverse)
setwd("/Volumes/feuk/HAUN LAB/STUDIEN/STELLA/Validierungsstudie/Validierung-R/Kinect Daten/NeeCo2")
library(sciplot)
neeco2.chest<-read.table("Kinect_neeco2_chest.txt", header=T)
neeco2.hip<-read.table("Kinect_neeco2_hip.txt", header=T)
neeco2.age<-read.table("NeeCo2_ages.txt", header=T)

colnames(neeco2.chest)
neeco2.chest$expansion<-neeco2.chest$Change-neeco2.hip$Change


# chest expansion 
neeco2.chest.expansion.ave<-neeco2.chest %>% 
	group_by(Subject, Dyad   ,  Condition ,Need     , Side  ,   Gender ,helped) %>% 
	summarize(expansion=mean(expansion)) 
	
	neeco2.chest.expansion.ave	%>% 
	drop_na() %>% 
	group_by( Need ,helped)%>% 
	summarize(se=se(expansion),expansion=mean(expansion)) %>% 
	ggplot(aes(Need, expansion,  fill=helped))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = expansion-se, ymax=expansion+se), position="dodge")

neeco2.chest.expansion.ave %>% 
	lm(expansion~Need+helped, data=.)%>% 
	summary()

neeco2.age<-neeco2.age %>% 
	rename("Subject"="ID")

neeco2.chest.expansion.ave<-right_join(neeco2.age, neeco2.chest.expansion.ave)

write.table(neeco2.chest.expansion.ave, file="neeco2.chest.expansion.ave.txt" )


# chest height 
neeco2.chest.ave<-neeco2.chest %>% 
	group_by(Subject, Dyad   ,  Condition ,Need     , Side  ,   Gender ,helped) %>% 
	summarize(Chest=mean(Change)) 
	quartz(6,6)
	neeco2.chest.ave	%>% 
	drop_na() %>% 
	group_by( Need ,helped)%>% 
	summarize(se=se(Chest),Chest=mean(Chest)) %>% 
	ggplot(aes(Need, Chest,  fill=helped))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = Chest-se, ymax=Chest+se), position="dodge")



neeco2.chest%>% 
	lm(Change~Need+helped, data=.)%>% 
	summary()
	
neeco2.chest.ave<-right_join(neeco2.age, neeco2.chest.ave)

write.table(neeco2.chest.ave, file="neeco2.chest.ave.txt" )

names(neeco2.hip)

# hip height 
neeco2.hip.ave<-neeco2.hip %>% 
	group_by(Subject, Dyad   ,  Condition ,Need     , Side  ,   Gender ,helped) %>% 
	summarize(Hip=mean(Change)) 
	
	neeco2.hip.ave	%>% 
	drop_na() %>% 
	group_by( Need ,helped)%>% 
	summarize(se=se(Hip),Hip=mean(Hip)) %>% 
	ggplot(aes(Need, Hip,  fill=helped))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = Hip-se, ymax=Hip+se), position="dodge")

neeco2.chest%>% 
	lm(Change~Need+helped, data=.)%>% 
	summary()

neeco2.hip.ave<-right_join(neeco2.age, neeco2.hip.ave)

write.table(neeco2.hip.ave, file="neeco2.hip.ave.txt" )










# chest height /expansion: plot by times! 
neeco2.chest.ave<-neeco2.chest %>% 
	group_by(Subject, Dyad   ,  Condition ,Need     , Side  ,   Gender ,helped) %>% 
	summarize(Chest=mean(Change)) 
	quartz(6,6)
	neeco2.chest	%>% 
	drop_na() %>% 
	group_by( Need ,helped, Time)%>% 
	summarize(se=se(expansion),Chest=mean(expansion)) %>% 
	ggplot(aes(Time, Chest,  color=helped))+
	geom_line()+facet_wrap(~Need)+
	geom_errorbar(aes(ymin = Chest-se, ymax=Chest+se), position="dodge")



neeco2.chest%>% 
	lm(Change~Need+helped, data=.)%>% 
	summary()
write.table(neeco2.chest.ave, file="neeco2.chest.ave.txt" )

names(neeco2.hip)



