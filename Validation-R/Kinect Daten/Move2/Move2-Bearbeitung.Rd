
rm(list=all)

setwd("/Volumes/FEUK-1/HAUN LAB/STUDIEN/STELLA/Validierungsstudie/Validierung-R/Kinect Daten/Move2")
library(sciplot)
library(tidyverse)
move2.chest<-read.table("Kinect_move2_chest.txt", header=T)
move2.hip<-read.table("Kinect_move2_hip.txt", header=T)

colnames(move2.chest)
move2.chest$expansion<-move2.chest$Change-move2.hip$Change

length(unique(move2.chest.expansion.ave$Subject))
# chest expansion 
move2.chest.expansion.ave<-move2.chest %>% 
	group_by(Subject,Gender,Help,Condition,Team     ,Phase ) %>% 
	summarize(expansion=mean(expansion)) 
	
quartz(6,6)
move2.chest.expansion.ave	%>% 
	group_by( Help  ,Condition  )%>% 
	summarize(se=se(expansion),expansion=mean(expansion)) %>% 
	ggplot(aes(Help  , expansion,  fill=Condition))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = expansion-se, ymax=expansion+se), position="dodge")
	dev.copy(pdf,"../expansion.pdf",  width=6, height=6)
dev.off()

	move2.chest.expansion.ave%>% 

	drop_na()
	
length(unique(move2.chest.expansion.ave$Subject))
move2.chest.expansion.ave %>% 
	lm(expansion~Help+Condition+Phase, data=.)%>% 
	summary()
write.table(move2.chest.expansion.ave, file="move2.expansion.ave.txt" )

which(move2.chest.expansion.ave$Subject=="243342")
# chest height
move2.chest.ave<-move2.chest %>% 
	group_by(Subject,Gender,Help,Condition,Team     ,Phase) %>% 
	summarize(chest=mean(Change)) 
	
	move2.chest.ave	%>% 
	drop_na() %>% 
	group_by( Help  ,Condition  )%>% 
	summarize(se=se(chest),chest=mean(chest)) %>% 
	ggplot(aes(Help  , chest,  fill=Condition))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = chest-se, ymax=chest+se), position="dodge")

move2.chest.expansion.ave %>% 
	lm(chest~Help+Condition+Phase, data=.)%>% 
	summary()
write.table(move2.chest.ave, file="move2.chest.ave.txt" )



# hip height
move2.hip.ave<-move2.hip %>% 
	group_by(Subject,Gender,Help,Condition,Team     ,Phase)  %>% 
	summarize(hip=mean(Change)) 
	
	move2.hip.ave	%>% 
	drop_na() %>% 
	group_by( Help  ,Condition  )%>% 
	summarize(se=se(hip),hip=mean(hip)) %>% 
	ggplot(aes(Help  , hip,  fill=Condition))+
	geom_bar(stat="identity", position="dodge")+
	geom_errorbar(aes(ymin = hip-se, ymax=hip+se), position="dodge")

move2.hip.ave %>% 
	lm(hip~Help+Condition+Phase, data=.)%>% 
	summary()
write.table(move2.hip.ave, file="move2.hip.ave.txt" )

