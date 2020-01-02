require('mongolite');
require('jsonlite');
require('dplyr',warn.conflicts = F);
require('stringr',warn.conflicts = T);
all_day_data<-mongo(collection = 'options',db = 'upstroke',url = 'mongodb://localhost:27017',verbose = T);
prev_dates_frame=all_day_data$aggregate('[{"$group": {"_id": "$date"}}, {"$addFields": {"_stamp": {"$dateFromString":{"dateString":"$_id","format":"%d/%m/%Y"}}}}, {"$sort": { "_stamp": -1 }},{"$project":{ "date":1}}]');
req_dates=prev_dates_frame$`_id`[1:1];
margin_data<<-data.frame(date=c(0),symbol=c(0),open=c(0),sl=c(0),slt=c(0),slh=c(0),close=c(0),margin=c(0),stringsAsFactors = FALSE);

aggreOutput<-lapply(req_dates,function(l){
  singleDay(l,"9:15AM",all_day_data,"3:10PM")
})