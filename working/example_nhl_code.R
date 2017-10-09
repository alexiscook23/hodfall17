df<-data.frame(champ,pct_score)
#Create a quantile variable for 
df<-df%>%mutate(pct_score_quant=ntile(pct_score,n=4))
#conditional mean of champ by pct_score_quant
df%>%group_by(pct_score_quant)%>%summarize(champ_prob=mean(champ))

masters$topar[masters$topar=="E"]<-0