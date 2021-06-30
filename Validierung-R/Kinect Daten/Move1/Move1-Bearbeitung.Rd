
rm(list=all)

setwd("/Volumes/FEUK-1/HAUN LAB/STUDIEN/STELLA/Validierungsstudie/Validierung-R/Kinect Daten/Move1")
library(sciplot)
library(tidyverse)
move1.chest<-read.table("Kinect_move1_chest.txt", header=T)
move1.hip<-read.table("Kinect_move1_hip.txt", header=T)

colnames(move1.chest)
move1.chest$expansion<-move1.chest$Change-move1.hip$Change


# chest expansion 

move1.chest.expansion.ave<-move1.chest %>% 
	group_by(Subject  ,   Gender    , Age_group  , Condition  , Game   ,     Team     ,   Phase ) %>% 
	summarize(expansion=mean(expansion)) 


dev.new()
quartz(6,6)
	move1.chest.expansion.ave	%>% 
	drop_na() %>% 
	group_by( Age_group  ,Condition  )%>% 
	summarize(se=se(expansion),expansion=mean(expansion)) %>% 
	ggplot(aes(Age_group  , expansion,  fill=Condition))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = expansion-se, ymax=expansion+se), position="dodge")
dev.copy(png,"../Move1/expansion.pdf",  width=6, height=6)
dev.off()

move1.chest.expansion.ave %>% 
	lm(expansion~Age_group+Condition+Phase, data=.)%>% 
	summary()
write.table(move1.chest.expansion.ave, file="move1.expansion.ave.txt" )


# chest height
move1.chest.ave<-move1.chest %>% 
	group_by(Subject  ,   Gender    , Age_group  , Condition  , Game   ,     Team     ,   Phase ) %>% 
	summarize(chest=mean(Change)) 
	
	move1.chest.chest.ave	%>% 
	drop_na() %>% 
	group_by( Age_group  ,Condition  )%>% 
	summarize(se=se(chest),chest=mean(chest)) %>% 
	ggplot(aes(Age_group  , chest,  fill=Condition))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = chest-se, ymax=chest+se), position="dodge")

move1.chest.expansion.ave %>% 
	lm(chest~Age_group+Condition+Phase, data=.)%>% 
	summary()
write.table(move1.chest.ave, file="move1.chest.ave.txt" )



# hip height
move1.hip.ave<-move1.hip %>% 
	group_by(Subject  ,   Gender    , Age_group  , Condition  , Game   ,     Team     ,   Phase ) %>% 
	summarize(hip=mean(Change)) 
	
	move1.hip.ave	%>% 
	drop_na() %>% 
	group_by( Age_group  ,Condition  )%>% 
	summarize(se=se(hip),hip=mean(hip)) %>% 
	ggplot(aes(Age_group  , hip,  fill=Condition))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = hip-se, ymax=hip+se), position="dodge")

move1.hip.ave %>% 
	lm(hip~Age_group+Condition+Phase, data=.)%>% 
	summary()
write.table(move1.hip.ave, file="move1.hip.ave.txt" )

