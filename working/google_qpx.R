library(tidyverse)
library(httr)
library(jsonlite)
library(tidyjson)
library(stringr)

x<-readLines("qpx_req.txt",warn = FALSE)

url <- "https://www.googleapis.com/qpxExpress/v1/trips/search"

my_Key<-"AIzaSyCU7M_n9rRk8E-_NH2nkTAjx38QgZ4B6Xc"

qpx_data<-POST(url, query = list(key = my_Key), body = x, content_type_json(),verbose())

my_trip_data<-content(qpx_data,as="text")

my_trips<-my_trip_data%>%as.tbl_json()%>%spread_all(json.col)

carrier_data<-my_trips%>%
  enter_object("carrier")%>%
  gather_array%>%
  spread_values(code=jstring("code"))%>%
  spread_values(carrier_name=jstring("name"))

trip_tidy_a<-my_trips%>%
  enter_object("trips")%>%
  enter_object("tripOption")%>%
  gather_array%>%
  spread_values(price=jstring("saleTotal"))%>%
  spread_values(id=jstring("id"))%>%
  enter_object("slice")%>%
  gather_array%>%
  spread_values(duration=jstring("duration"))%>%
  enter_object("segment")%>%
  gather_array%>%
  enter_object("flight")%>%spread_values(carrier=jstring("carrier"))

  trip_tidy_b<-my_trips%>%
  enter_object("trips")%>%
  enter_object("tripOption")%>%
  gather_array%>%
  spread_values(id=jstring("id"))%>%
  enter_object("slice")%>%
  gather_array%>%
  enter_object("segment")%>%
  gather_array%>%  
  enter_object("leg")%>%
  gather_array%>%
  spread_values(leg_duration=jstring("duration"),
                departure_time=jstring("departureTime"),
                arrival_time=jstring("arrivalTime")
                )

final_data<-left_join(trip_tidy_a,trip_tidy_b,by="id")

# This will extract price as numeric
final_data<-mutate(final_data,price=as.numeric(str_extract(price,"\\d+")))

## Quick plot of prices
gg<-ggplot(final_data,aes(x=price))
gg<-gg+geom_density()
gg

reg1<-lm(price~as.factor(carrier),data=final_data)

coal_loss_total<-coal_loss_total%>%
  mutate(state_sort=fct_reorder(f=state,x=percent_change))

df$var<-as.numeric(df$var)

gg<-ggplot(coal_loss_total,aes(x=state,y=percent_change))
gg<-gg+geom_bar()
gg<-coord_flip()
gg<-gg+scale_y_continuous(breaks=c(-5,0,5,10))


