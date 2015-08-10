# Analysis based on following article on Fangraphs:
# http://www.fangraphs.com/blogs/bettermatch-up-data-forecasting-strikeout-rate/
# http://www.fangraphs.com/fantasy/potential-starting-pitcher-strikeout-rate-surgers/
# http://www.fangraphs.com/fantasy/the-bestest-pitcher-expected-bb-formula-yet/

# Load libraries
library("griffun")
library("XML")
library("stringr")
library("plyr")
library("data.table")
library("sqldf")

# Scrape Fangraphs current year
# Get year
yearto<-iif(as.numeric(format(Sys.time(), "%m%d"))<=331,as.numeric(format(Sys.time(), "%Y"))-1,as.numeric(format(Sys.time(), "%Y")))

base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=10&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,59,62,75,76,77,81,83,105,106,107,108,109,110,111,112,113,114,120,121,122,218,219,220,221,222,223&season=",
                 yearto, "&month=3&season1=", # month=0 - full season; month=3 - 30 days
                 yearto, "&ind=0&team=")
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&rost=1&age=0&filter=&players=0&sort=39,d")

# Scrape
fangraphsP<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$LeaderBoard1_dg1_ctl00)})

# Combine Scrapes
fangraphsPitching<-c()
for(i in 1:30) {
  fangraphsPitching<-as.data.frame(rbind(fangraphsPitching,fangraphsP[[i]]))
}

# Rename columns
colnames(fangraphsPitching)<-c("Number","Name","Team","Age","W","L","ERA","G","GS","CG","ShO","SV","BS","IP","TBF","H","R","ER","HR","BB","IBB","HBP","WP","BK","SO","GB","FB","LD","IFFB","Balls","Strikes","Pitches","RS","IFH","BU","BUH","K_9","BB_9","K_BB","H_9","HR_9","AVG","WHIP","BABIP","LOB_","FIP","GB_FB","LD_","GB_","FB_","IFFB_","HR_FB","IFH_","BUH_","WAR","xFIP","FB__","FBv","SL_","CB_","CH_","OSwing_","ZSwing_","Swing_","OContact_","ZContact_","Contact_","Zone_","FStrike_","SwStr_","HLD","K_","BB_","SIERA","Pull_","Cent_","Oppo_","Soft_","Med_","Hard_")
# str(fangraphsPitching)

# Remove percentages
for(i in c(1,4:ncol(fangraphsPitching))) {
  fangraphsPitching[,i]<-str_trim(str_replace_all(fangraphsPitching[,i],"%",""))
}

# Char to Number
for(i in c(1,4:ncol(fangraphsPitching))) {
  fangraphsPitching[,i]<-as.numeric(as.character(fangraphsPitching[,i]))
}

# Calculated HR/Batted Ball 
fangraphsPitching$HR_BB <- iif(fangraphsPitching$HR > 0,round((fangraphsPitching$HR / (fangraphsPitching$LD+fangraphsPitching$GB+fangraphsPitching$FB+fangraphsPitching$BUH)) * 100,2), 0)

# replace NA with 0
fangraphsPitching[is.na(fangraphsPitching)]<-0

# summary(fangraphsPitching)

# str(fangraphsPitching)
xPitch = sqldf("SELECT a.Name, IP, SV, HLD, FBv, K_, BB_,
                  SwStr_,OSwing_,ZSwing_,Swing_,OContact_,ZContact_,
                  Zone_,Hard_,round(HR_BB,2) as HR_BB, ERA, FIP, SIERA
                  from fangraphsPitching a
                  where (IP >= 20 or SV > 3 or HLD > 3)
                  ")

############### xPected K% ########################
xK_.fit = lm(K_~IP+SwStr_+OSwing_+ZSwing_+Swing_+OContact_+ZContact_+Zone_+FBv, data=xPitch)
summary(xK_.fit)
xPitch = cbind(xPitch,'xK_'=round(xK_.fit$fitted.values,2), 'GapxK_'=round(xK_.fit$residuals,2))

############### xPected FIP #######################
xFIP.fit = lm(FIP~IP+OSwing_+FBv+Hard_+ERA+SIERA, data=xPitch)
summary(xFIP.fit)
xPitch = cbind(xPitch,'xFIP'=round(xFIP.fit$fitted.values,2), 'GapxFIP'=round(xFIP.fit$residuals,2))
xPitching <- subset(xPitch, xFIP < 5, select = c(Name,IP,SV,HLD,FBv,K_,xK_,BB_,HR_BB,SwStr_,SIERA,ERA,FIP,xFIP,GapxFIP,GapxK_))
xPitching = xPitching[order(xPitching$GapxFIP),]

#######################################################

# Aim for players with -Gap on xK% and +Gap for xFIP
tail(xPitching,25)

# END