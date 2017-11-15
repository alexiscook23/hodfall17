

pgafair<-pga%>%filter(round==1)%>%
  group_by(player,tournament)%>%
  mutate(fairway=ifelse(DriveLocation=="Fairway",1,0))%>% ## May need more stuff
  summarize(fairway_drives=sum(fairway))

pga<-pga%>%filter(round==3)%>%
  group_by(player,tournament)%>%
  arrange(-rtp_score)%>%
  mutate(top10=ifelse(row_number(-rtp_score)>11,1,0))

analaysis_data<-left_join(pga_fair,pga,by="player")

mod<-glm(top10~fairway_drives,family=binomial(link="logit"))


mod<-glm(top10~as.factor())