singleDay<-function(data,date,entry,exit,ce,pe){
  single_day_data<-data$find(paste('{"date":"',date,'"}',sep = ''));
  entry_string<-paste(date,entry,sep=" ");
  entrytimems<-as.numeric(as.POSIXct(entry_string,format="%d/%m/%Y %I:%M%p"))*1000;
  exit_string<-paste(date,exit,sep=" ");
  exittimems<-as.numeric(as.POSIXct(exit_string,format="%d/%m/%Y %I:%M%p"))*1000;
  entryCandles<-single_day_data%>%filter(timestamp == entrytimems);
  entryCandles<-entryCandles%>%mutate(
    index=((function(x){str_extract(x,pattern ="[A-Z]+")})(entryCandles$symbol)),
    strike=((function(x){
      extracted<-str_extract(x,pattern ="\\d{2}(\\d+|[A-Z]+)\\d+");
      str_sub(extracted,6)
      
      })(entryCandles$symbol)),
    type=((function(x){str_sub(x,-2)})(entryCandles$symbol)),
  )%>%arrange(type,index,strike)
   entrySymbols<-entryCandles%>%group_by(index,type)%>%mutate(rank=rank(as.numeric(strike)));
   entryTokens<-entrySymbols%>%filter((type=="CE"&rank==ce) | (type=="PE"&rank==pe));
   
  apply(entryTokens,1,function(token){
    
    token_symbol<-token['symbol'];
    open_price<-as.numeric(token['open']);
    single_day_symbol_data<-single_day_data%>%filter(symbol== token_symbol,timestamp<=exittimems);
    stop_loss_candles<-single_day_symbol_data%>%filter(high > (open_price*1.5))%>%arrange(timestamp);
    exit_candle<-single_day_symbol_data%>%filter(timestamp ==exittimems);
    exit_price<-exit_candle$close;
    
    contra_open<-(entryTokens%>%filter(as.numeric(open) != open_price & index==token['index']))$open;
    
    if(open_price > contra_open){
      lot<-1;
    }
    if(open_price<contra_open){
      lot<-round(contra_open/open_price)
    }
    lot_size<-index_data[token['index'],]
    if(nrow(stop_loss_candles) > 0){
      first_stop_loss_candle<-stop_loss_candles[1,];
      margin_data<<-rbind(margin_data,c(date=date,index=token['index'],symbol=token_symbol,open=open_price,sl=T,slt=first_stop_loss_candle$time,slh=first_stop_loss_candle$high,close=exit_price,lot=lot,margin=lot*lot_size*(open_price-(open_price*1.5))));
    }
    if(nrow(stop_loss_candles) < 1){
      margin_data<<-rbind(margin_data,c(date=date,index=token['index'],symbol=token_symbol,open=open_price,sl=F,slt="",slh="",close=exit_price,lot=lot,margin=lot*lot_size*((open_price-exit_price))));
    }
    
    })
}