# Analysis based on following article on Fangraphs:
# http://www.fangraphs.com/fantasy/muscling-up-iso-surgers/
# http://www.fangraphs.com/fantasy/new-hitter-xbabip-based-on-bis-batted-ball-data/
# http://www.fangraphs.com/fantasy/nl-outfield-poweriso-buy-low-candidates/

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

base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=150&type=c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,57,58,60,72,73,74,75,76,77,78,79,80,81,82,83,102,103,104,105,106,107,108,109,110,206,207,208,209,210,211&season=",
                 yearto, "&month=0&season1=",
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

# replace NA with 0
fangraphsBatting[is.na(fangraphsBatting)]<-0

# summary(fangraphsBatting)

# Scrape Batted Ball Distance/Velocity Data from Baseball Savant
htmltbl <- readHTMLTable(doc = "http://baseballsavant.com/apps/hit_leader.php?game_date=&abs=50&sort=5,1")
babip <- data.frame(htmltbl[1])
colnames(babip) <- c("Rank","Name","AB","MaxExitVel","MinExitVel","AvgExitVel","AvgFB_LDExitVel","AvgGBExitVel","MaxDistance","AvgDist","AvgHRDistance")
# Merging datasets for analysis
xHitting = sqldf("SELECT a.NAME,Age,
                  BABIP,GB_FB,LD_,GB_,FB_,IFFB_,IFH_,Pull_,Cent_,Oppo_,Soft_,Med_,Hard_,Contact_,Spd,
                  ISO,
                  HR_FB,wOBA,
                  round(AvgExitVel) as AvgExitVel,round(AvgDist) as AvgDist
                 FROM fangraphsBatting a
                 JOIN babip b ON a.NAME = b.NAME
                 ")

############### xPected XBABIP #######################
xBABIP.fit = lm(BABIP~GB_FB+LD_+FB_+IFFB_+IFH_+Pull_+Cent_+Oppo_+Hard_+Spd, data=xHitting)
summary(xBABIP.fit)
xBABIP_out = cbind(xHitting,'xBABIP'=round(xBABIP.fit$fitted.values,3), 'Gap'=round(xBABIP.fit$residuals,3))
xBABIP_out <- subset(xBABIP_out, select = c(Name,BABIP,xBABIP,Gap))
xBABIP_out = xBABIP_out[order(-xBABIP_out$Gap),]

############### xPected ISO #######################
xISO.fit = lm(ISO~FB_+Pull_+Hard_+AvgDist, data=xHitting)
summary(xISO.fit)
xISO_out = cbind(xHitting,'xISO'=round(xISO.fit$fitted.values,3), 'Gap'=round(xISO.fit$residuals,3))
xISO_out <- subset(xISO_out, select = c(Name,Hard_,AvgDist,ISO,xISO,Gap))
xISO_out = xISO_out[order(-xISO_out$Gap),]

################ xPected HR/FB #######################
xHR.FB.fit = lm(HR_FB~+FB_+Pull_+Hard_+AvgDist, data=xHitting)
summary(xHR.FB.fit)
xHR_FB_output = cbind(xHitting,'xHR_FB'=round(xHR.FB.fit$fitted.values,2), 'Gap'=round(xHR.FB.fit$residuals,2))
xHR_FB_output <- subset(xHR_FB_output, select = c(Name,Hard_,AvgDist,HR_FB,xHR_FB,Gap))
xHR_FB_output = xHR_FB_output[order(-xHR_FB_output$Gap),]

projectx <- sqldf("select h.Name, h.AvgExitVel as AvgVelo, h.AvgDist as AvgDist,
                          h.Hard_,h.Pull_,Oppo_,Contact_,Spd,
                          a.BABIP,a.xBABIP,a.Gap as GAPxBABIP,
                          b.ISO, b.xISO, b.Gap as GAPxISO,
                          c.HR_FB,c.xHR_FB,c.Gap as GAPxHR_FB,
                          h.wOBA
                  from xHitting h
                  join xBABIP_out a on h.name = a.name
                  join xISO_out b on b.Name = h.Name
                  join xHR_FB_output c on c.name = h.name
                  order by b.gap desc
                  ")

################xPected wOBA ##########################

# Using the xBABIP,xISO and xHR_FB, calculate the xOBP
wOBA.fit = lm(wOBA~+Hard_+Pull_+Oppo_+Contact_+Spd+AvgVelo+AvgDist+xBABIP+xISO+xHR_FB, data=projectx)
summary(wOBA.fit)
xwOBA_output = cbind(projectx,'xwOBA'=round(wOBA.fit$fitted.values,3), 'xwOBA_Gap'=round(wOBA.fit$residuals,3))
# xwOBA_output <- subset(xwOBA_output, select = c(Name,FB_,Pull_,Hard_,AvgExitVel,AvgDist,xwOBA,Gap))
xwOBA_output = xwOBA_output[order(xwOBA_output$xwOBA),]

#######################################################

# Zips  Projections on Fangraphs
base_url<-"http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=rzips&team="
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&players=0&sort=21,d")
# Scrape
zipsB<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$ProjectionBoard1_dg1_ctl00)})

# Combine Scrapes
zipsBatting<-c()
for(i in 1:30) {
  zipsBatting<-as.data.frame(rbind(zipsBatting,zipsB[[i]]))
}

# Rename columns
colnames(zipsBatting)<-c("Name","Team","G","PA","AB","H","x2B","x3B","HR","R","RBI","BB","SO","HBP","SB","CS","AVG","OBP","SLG","OPS","wOBA","Fld","BsR","WAR")

# Remove percentages
for(i in 3:ncol(zipsBatting)) {
  zipsBatting[,i]<-str_trim(str_replace_all(zipsBatting[,i],"%",""))
}

# # Char to Number
for(i in 3:ncol(zipsBatting)) {
  zipsBatting[,i]<-as.numeric(as.character(zipsBatting[,i]))
}

# replace NA with 0
zipsBatting[is.na(zipsBatting)]<-0

# Calculate singles in zips projections
zipsBatting$x1B<-zipsBatting$H-(zipsBatting$x2B+zipsBatting$x3B+zipsBatting$HR)

xHitting<-sqldf("select distinct oba.Name,oba.AvgVelo,oba.AvgDist,
                        oba.BABIP,oba.xBABIP,oba.GAPxBABIP,oba.ISO,oba.xISO,oba.GAPxISO,oba.HR_FB,oba.xHR_FB,oba.GAPxHR_FB,oba.wOBA,oba.xwOBA,oba.xwOBA_Gap,
                        zip.wOBA as zipswOBA, oba.wOBA-zip.wOBA as zipswOBA_GAP,
                 round((zip.AB*-1)+(zip.x1B*6)+(zip.x2B*9)+(zip.x3B*12)+(zip.HR*16)+((zip.BB+zip.HBP)*3)+(zip.SB*3)+(zip.CS*-4)) as ZipsRoS
                 from zipsBatting zip
                 join xwOBA_output oba on zip.name = oba.name
                 order by xwOBA asc
                 ")

# SELL!
# head(xBABIP_out,20)
# head(xISO_out,20)
# head(xHR_FB_output,20)

# BUY!!
# tail(xBABIP_out,20)
tail(xISO_out,20)
tail(xHR_FB_output,20)
tail(xwOBA_output,20)
