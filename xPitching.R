# Analysis based on following article on Fangraphs:
# http://www.fangraphs.com/blogs/bettermatch-up-data-forecasting-strikeout-rate/
# http://www.fangraphs.com/fantasy/potential-starting-pitcher-strikeout-rate-surgers/
# http://www.fangraphs.com/fantasy/the-bestest-pitcher-expected-bb-formula-yet/
# http://www.fangraphs.com/fantasy/last-14-day-al-starting-pitcher-velocity-decliners/

# Load libraries
library("griffun")
library("XML")
library("stringr")
library("plyr")
library("data.table")
library("sqldf")

# Scrape Fangraphs current year
## Get year
yearto<-iif(as.numeric(format(Sys.time(), "%m%d"))<=331,as.numeric(format(Sys.time(), "%Y"))-1,as.numeric(format(Sys.time(), "%Y")))

# Get Full Season Stats
base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=10&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,59,62,75,76,77,81,83,105,106,107,108,109,110,111,112,113,114,120,121,122,218,219,220,221,222,223,224&season=",
                 yearto, "&month=0&season1=", # month=0 - full season; month=2 - 14 days; month=3 - 30 days
                 yearto, "&ind=0&team=")
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&rost=1&age=0&filter=&players=0&sort=39,d")

# Scrape
fangraphsP<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$LeaderBoard1_dg1_ctl00)})

# Combine Scrapes
fangraphsPitchingFull<-c()
for(i in 1:30) {
  fangraphsPitchingFull<-as.data.frame(rbind(fangraphsPitchingFull,fangraphsP[[i]]))
}

# Rename columns
colnames(fangraphsPitchingFull)<-c("Number","Name","Team","Age","W","L","ERA","G","GS","CG","ShO","SV","BS","IP","TBF","H","R","ER","HR","BB","IBB","HBP","WP","BK","SO","GB","FB","LD","IFFB","Balls","Strikes","Pitches","RS","IFH","BU","BUH","K_9","BB_9","K_BB","H_9","HR_9","AVG","WHIP","BABIP","LOB_","FIP","GB_FB","LD_","GB_","FB_","IFFB_","HR_FB","IFH_","BUH_","WAR","xFIP","FB__","FBv","SL_","CB_","CH_","OSwing_","ZSwing_","Swing_","OContact_","ZContact_","Contact_","Zone_","FStrike_","SwStr_","HLD","K_","BB_","SIERA","Pull_","Cent_","Oppo_","Soft_","Med_","Hard_")
# str(fangraphsPitchingFull)

# Remove percentages
for(i in c(1,4:ncol(fangraphsPitchingFull))) {
  fangraphsPitchingFull[,i]<-str_trim(str_replace_all(fangraphsPitchingFull[,i],"%",""))
}

# Char to Number
for(i in c(1,4:ncol(fangraphsPitchingFull))) {
  fangraphsPitchingFull[,i]<-as.numeric(as.character(fangraphsPitchingFull[,i]))
}

# Calculated HR/Batted Ball 
fangraphsPitchingFull$HR_BB <- iif(fangraphsPitchingFull$HR > 0,round((fangraphsPitchingFull$HR / (fangraphsPitchingFull$LD+fangraphsPitchingFull$GB+fangraphsPitchingFull$FB+fangraphsPitchingFull$BUH)) * 100,2), 0)

# Calculated Avg of FIP and xFIP 
fangraphsPitchingFull$aFIP <- round(rowMeans(subset(fangraphsPitchingFull, select = c(FIP, xFIP)), na.rm = TRUE),2)

# replace NA with 0
fangraphsPitchingFull[is.na(fangraphsPitchingFull)]<-0
# summary(fangraphsPitchingFull)
# str(fangraphsPitchingFull)

## END Full Season Collection

## Get 30 Day Collection
base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=10&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,59,62,75,76,77,81,83,105,106,107,108,109,110,111,112,113,114,120,121,122,218,219,220,221,222,223,224&season=",
                 yearto, "&month=3&season1=", # month=0 - full season; month=2 - 14 days; month=3 - 30 days
                 yearto, "&ind=0&team=")
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&rost=1&age=0&filter=&players=0&sort=39,d")

# Scrape
fangraphsP<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$LeaderBoard1_dg1_ctl00)})

# Combine Scrapes
fangraphsPitching30d<-c()
for(i in 1:30) {
  fangraphsPitching30d<-as.data.frame(rbind(fangraphsPitching30d,fangraphsP[[i]]))
}

# Rename columns
colnames(fangraphsPitching30d)<-c("Number","Name","Team","Age","W","L","ERA","G","GS","CG","ShO","SV","BS","IP","TBF","H","R","ER","HR","BB","IBB","HBP","WP","BK","SO","GB","FB","LD","IFFB","Balls","Strikes","Pitches","RS","IFH","BU","BUH","K_9","BB_9","K_BB","H_9","HR_9","AVG","WHIP","BABIP","LOB_","FIP","GB_FB","LD_","GB_","FB_","IFFB_","HR_FB","IFH_","BUH_","WAR","xFIP","FB__","FBv","SL_","CB_","CH_","OSwing_","ZSwing_","Swing_","OContact_","ZContact_","Contact_","Zone_","FStrike_","SwStr_","HLD","K_","BB_","SIERA","Pull_","Cent_","Oppo_","Soft_","Med_","Hard_","kwERA")
# str(fangraphsPitching30d)

# Remove percentages
for(i in c(1,4:ncol(fangraphsPitching30d))) {
  fangraphsPitching30d[,i]<-str_trim(str_replace_all(fangraphsPitching30d[,i],"%",""))
}

# Char to Number
for(i in c(1,4:ncol(fangraphsPitching30d))) {
  fangraphsPitching30d[,i]<-as.numeric(as.character(fangraphsPitching30d[,i]))
}

# Calculated HR/Batted Ball 
fangraphsPitching30d$HR_BB <- iif(fangraphsPitching30d$HR > 0,round((fangraphsPitching30d$HR / (fangraphsPitching30d$LD+fangraphsPitching30d$GB+fangraphsPitching30d$FB+fangraphsPitching30d$BUH)) * 100,2), 0)

# Calculated Avg of FIP and xFIP 
fangraphsPitching30d$aFIP <- round(rowMeans(subset(fangraphsPitching30d, select = c(FIP, xFIP)), na.rm = TRUE),2)
  
# replace NA with 0
fangraphsPitching30d[is.na(fangraphsPitching30d)]<-0
# summary(fangraphsPitching30d)
# str(fangraphsPitching30d)

## END 30 Day Collection

xPitch = sqldf("SELECT a.Name, a.IP, a.SV, a.HLD, a.FBv,a.K_, a.BB_,
                       a.SwStr_,a.OSwing_,a.ZSwing_,a.Swing_,a.OContact_,a.ZContact_,
                       a.FB_,a.Zone_,a.Hard_,a.Pull_,round(a.HR_BB,2) as HR_BB,
                       a.ERA, a.FIP, a.xFIP, a.aFIP, a.SIERA, a.kwERA,
                       a.FBv-b.FBv as FBvDIFF,
                       a.K_-b.K_ as K_DIFF,
                       a.BB_-b.BB_ as BB_DIFF,
                       a.SwStr_-b.SwStr_ as SwStr_DIFF,
                       a.OSwing_-b.OSwing_ as OSwing_DIFF,
                       a.ZSwing_-b.ZSwing_ as ZSwing_DIFF,
                       a.Swing_-b.Swing_ as Swing_DIFF,
                       a.OContact_-b.OContact_ as OContact_DIFF,
                       a.ZContact_-b.ZContact_ as ZContact_DIFF,
                       a.FB_-b.FB_ as FB_DIFF,
                       a.Zone_-b.Zone_ as Zone_DIFF,
                       a.Hard_-b.Hard_ as Hard_DIFF,
                       a.Pull_-b.Pull_ as Pull_DIFF
                  from fangraphsPitching30d a
                  join fangraphsPitchingFull b on b.Name = a.Name
                  where (a.IP >= 20 or a.SV > 3 or a.HLD > 5)
                  ")

############### xPected K% ########################
xK_.fit = lm(K_~IP+SwStr_+OSwing_+ZSwing_+Swing_+OContact_+ZContact_+Zone_+FBv+Pull_+FBvDIFF+K_DIFF+BB_DIFF+SwStr_DIFF+OSwing_DIFF+ZSwing_DIFF+Swing_DIFF+OContact_DIFF+ZContact_DIFF+FB_DIFF+Zone_DIFF+Hard_DIFF+Pull_DIFF, data=xPitch)
summary(xK_.fit)
xPitch = cbind(xPitch,'xK_'=round(xK_.fit$fitted.values,2), 'GapxK_'=round(xK_.fit$residuals,2))

############### xPected BB% ########################
xBB_.fit = lm(BB_~IP+SwStr_+OSwing_+ZSwing_+Swing_+OContact_+ZContact_+Zone_+FBv+Pull_+FBvDIFF+K_DIFF+BB_DIFF+SwStr_DIFF+OSwing_DIFF+ZSwing_DIFF+Swing_DIFF+OContact_DIFF+ZContact_DIFF+FB_DIFF+Zone_DIFF+Hard_DIFF+Pull_DIFF, data=xPitch)
summary(xBB_.fit)
xPitch = cbind(xPitch,'xBB_'=round(xBB_.fit$fitted.values,2), 'GapxBB_'=round(xBB_.fit$residuals,2))

############### xPected aFIP #######################
xaFIP.fit = lm(aFIP~IP+xK_+Hard_+Pull_+ERA+kwERA+SIERA+K_DIFF+Pull_DIFF, data=xPitch)
summary(xaFIP.fit)
xPitch = cbind(xPitch,'xaFIP'=round(xaFIP.fit$fitted.values,2), 'GapxaFIP'=round(xaFIP.fit$residuals,2))
xPitching <- subset(xPitch, xaFIP < 5, select = c(Name,IP,SV,HLD,FBv,K_,xK_,GapxK_,BB_,xBB_,GapxBB_,HR_BB,SwStr_,SIERA,ERA,kwERA,FIP,xFIP,aFIP,xaFIP,GapxaFIP,FBvDIFF))
xPitching = xPitching[order(xPitching$GapxaFIP),]

#######################################################

# Aim for players with -Gap on xK% and +Gap for xFIP AND +FBvDIFF
tail(xPitching,25)
csv_name <- paste("xPitching",".csv", sep = '')
write.table(xPitching,file=csv_name,sep=",",row.names=F)

# END