# Analysis based on following article on Fangraphs:
# http://www.fangraphs.com/fantasy/muscling-up-iso-surgers/
# http://www.fangraphs.com/fantasy/new-hitter-xbabip-based-on-bis-batted-ball-data/
# http://www.fangraphs.com/fantasy/nl-outfield-poweriso-buy-low-candidates/
# http://www.fangraphs.com/fantasy/is-it-time-to-move-past-hrfb-rate/

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

base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=y&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,57,58,60,72,73,74,75,76,77,78,79,80,81,82,83,102,103,104,105,106,107,108,109,110,206,207,208,209,210,211&season=",
                 yearto, "&month=3&season1=", # month=0 - full season; month=3 - 30 days
                 yearto, "&ind=0&team=")
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&rost=1&age=0&filter=&players=0&sort=32,d")
# Scrape
fangraphsB<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$LeaderBoard1_dg1_ctl00)})

# Combine Scrapes
fangraphsBatting<-c()
for(i in 1:30) {
  fangraphsBatting<-as.data.frame(rbind(fangraphsBatting,fangraphsB[[i]]))
}

# Rename columns
colnames(fangraphsBatting)<-c("RowID","Name","Team","Age","G","AB","PA","H","1B","2B","3B","HR","R","RBI","BB","IBB","SO","HBP","SF","SH","GDP","SB","CS","AVG","GB","FB","LD","IFFB","Pitches","Balls","Strikes","IFH","BU","BUH","BB_","K_","BB_K","OBP","SLG","OPS","ISO","BABIP","GB_FB","LD_","GB_","FB_","IFFB_","HR_FB","IFH_","BUH_","wOBA","RAR","WAR","Spd","FB__","FBv","SL_","SLv","CT_","CTv","CB_","CBv","CH_","CHv","SF_","SFv","O_Swing_","Z_Swing_","Swing_","O_Contact_","Z_Contact_","Contact_","Zone_","F_Strike_","SwStr_","Pull_","Cent_","Oppo_","Soft_","Med_","Hard_")
head(fangraphsBatting)
# Remove percentages
for(i in c(1,4:ncol(fangraphsBatting))) {
  fangraphsBatting[,i]<-str_trim(str_replace_all(fangraphsBatting[,i],"%",""))
}

# Char to Number
for(i in c(1,4:ncol(fangraphsBatting))) {
  fangraphsBatting[,i]<-as.numeric(as.character(fangraphsBatting[,i]))
}

# Calculated HR/Batted Ball 
fangraphsBatting$HR_BB <- iif(fangraphsBatting$HR > 0,round((fangraphsBatting$HR / (fangraphsBatting$LD+fangraphsBatting$GB+fangraphsBatting$FB+fangraphsBatting$BUH)) * 100,2), 0)

# replace NA with 0
fangraphsBatting[is.na(fangraphsBatting)]<-0


# summary(fangraphsBatting)

# Scrape Batted Ball Distance/Velocity Data from Baseball Savant
htmltbl <- readHTMLTable(doc = "http://baseballsavant.com/apps/hit_leader.php?game_date=&abs=20&sort=5,1")
babip <- data.frame(htmltbl[1])
colnames(babip) <- c("Rank","Name","AB","MaxExitVel","MinExitVel","AvgExitVel","AvgFB_LDExitVel","AvgGBExitVel","MaxDistance","AvgDist","AvgHRDistance")
# Merging datasets for analysis
xHit <- sqldf("SELECT a.NAME,Age,
                 BABIP,GB_FB,LD_,GB_,FB_,IFFB_,IFH_,Pull_,Cent_,Oppo_,Soft_,Med_,Hard_,Contact_,Spd,
                 ISO,HR_BB,
                 HR_FB,wOBA,
                 round(AvgExitVel) as AvgExitVel,round(AvgDist) as AvgDist
                 FROM fangraphsBatting a
                 JOIN babip b ON a.NAME = b.NAME
                 ")

############### xPected XBABIP #######################
xBABIP.fit <- lm(BABIP~GB_FB+LD_+FB_+IFFB_+IFH_+Pull_+Cent_+Oppo_+Hard_+Spd+AvgExitVel, data=xHit)
summary(xBABIP.fit)
xBABIP_out <- cbind(xHit,'xBABIP'=round(xBABIP.fit$fitted.values,3), 'Gap'=round(xBABIP.fit$residuals,3))
xBABIP_out <- subset(xBABIP_out, select = c(Name,BABIP,xBABIP,Gap))
xBABIP_out <- xBABIP_out[order(xBABIP_out$xBABIP),]

############### xPected ISO #########################
xISO.fit <- lm(ISO~FB_+Pull_+Hard_+AvgDist+AvgExitVel, data=xHit)
summary(xISO.fit)
xISO_out <- cbind(xHit,'xISO'=round(xISO.fit$fitted.values,3), 'Gap'=round(xISO.fit$residuals,3))
xISO_out <- subset(xISO_out, select = c(Name,Hard_,AvgDist,ISO,xISO,Gap))
xISO_out <- xISO_out[order(xISO_out$xISO),]

############### xPected HR_BB#################
xHR.BB.fit <- lm(HR_BB~+FB_+Pull_+Hard_+AvgDist+AvgExitVel, data=xHit)
summary(xHR.BB.fit)
xHR_BB_output <- cbind(xHit,'xHR_BB'=round(xHR.BB.fit$fitted.values,2), 'Gap'=round(xHR.BB.fit$residuals,2))
xHR_BB_output <- subset(xHR_BB_output, select = c(Name,Hard_,AvgDist,HR_BB,xHR_BB,Gap))
xHR_BB_output <- xHR_BB_output[order(xHR_BB_output$xHR_BB),]

projectx <- sqldf("select h.Name, h.AvgExitVel as AvgVelo, h.AvgDist as AvgDist,
                  h.Hard_,h.Pull_,Contact_,
                  a.xBABIP, b.ISO, b.xISO,
                  c.HR_BB,c.xHR_BB,
                  b.Gap as GAPxISO,c.Gap as GAPxHR_BB,
                  h.wOBA
                  from xHit h
                  join xBABIP_out a on h.name = a.name
                  join xISO_out b on b.Name = h.Name
                  join xHR_BB_output c on c.name = h.name
                  order by b.gap desc
                  ")

################xPected wOBA ##########################

# Using the xBABIP,xISO and xHR_BB, calculate the xwOBA
wOBA.fit <- lm(wOBA~+Hard_+Pull_+Contact_+AvgVelo+AvgDist+xBABIP+xISO+xHR_BB, data=projectx)
summary(wOBA.fit)
xwOBA_output <- cbind(projectx,'xwOBA'=round(wOBA.fit$fitted.values,3), 'GapxwOBA_'=round(wOBA.fit$residuals,3))
xwOBA_output <- subset(xwOBA_output, xHR_BB > 0, select = c(Name,AvgVelo,AvgDist,Hard_,Pull_,ISO,xISO,HR_BB,xHR_BB,wOBA,xwOBA,GAPxISO,GAPxHR_BB,GapxwOBA_))
xHitting <- xwOBA_output[order(-xwOBA_output$GAPxISO),]

#######################################################

# Aim for players with -Gap
tail(xHitting,25)

# END
