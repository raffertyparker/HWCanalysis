if (!exists("dFile")){
  dFile <- "~/HWCanalysis/Masters/data/" 
}
if (!exists("DT_hh")){
load(paste0(dFile, "DT_hh.Rda"))
}

library(dplyr)
library(data.table)

for (house in unique(DT_hh$linkID)){
  p <- forecast::naive(DT_hh$HWelec[DT_hh$linkID == house], h = 1)
  pMdl <- cbind(DT_hh$hHour[DT_hh$linkID == house], p$x, p$fitted)
  pMdl <- data.table(pMdl)
  names(pMdl) <- c("Time", "Actual", "Fitted")
  pMdl$Time <- lubridate::as_datetime(pMdl$Time, tz = 'Pacific/Auckland')
  assign(house, p)
  get(house) %>%
    save(file = paste0(dFile, "models/randomWalk/", house, "_at_30.Rda"))
  assign(house, pMdl)
  get(house) %>%
    save(file = paste0(dFile, "models/randomWalk/", house, "_at_30_for_plotting.Rda"))
}

library(ggplot2)
p <- ggplot(data = rf_06[1:48, ], aes(x = Time, y = Actual)) +
  geom_line(col = 'red') +
  geom_line(aes(y = Fitted), col = 'blue') 

#p1a<-ggplot(data=,aes(x=date,y=observed)) 
#p1a<-p1a+geom_line(col='red')
#p1a<-p1a+geom_line(aes(y=fitted),col='blue')
#p1a<-p1a+geom_line(aes(y=forecast))+geom_ribbon(aes(ymin=lo95,ymax=hi95),alpha=.25)
#p1a<-p1a+scale_x_date(name='',breaks='1 year',minor_breaks='1 month',labels=date_format("%b-%y"),expand=c(0,0))
#p1a<-p1a+scale_y_continuous(name='Units of Y')
#p1a<-p1a+opts(axis.text.x=theme_text(size=10),title='Arima Fit to Simulated Data\n (black=forecast, blue=fitted, red=data, shadow=95% conf. interval)')