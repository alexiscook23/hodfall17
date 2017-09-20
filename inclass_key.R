
pc_frl_total<-free2%>%select(state,
                             pc_frl_2000,
                             pc_frl_2010,
                             pc_frl_2011,
                             pc_frl_2012)

names(pc.frl_total)<-c("state","2000","2010","2011","2012")

pc_frl_total<-pc_frl_total%>%gather(`2000`,`2010`,`2011`,`2012`,key=year,value=pc_frl_students)

## ------------------------------------------------------------------------
```