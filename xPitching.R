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

base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=y&type=c,4,5,7,8,11,12,13,15,16,17,18,19,20,21,114,24,36,37,40,113,112,111,76,77,81,211,43,50,44,47,48,49,51,218,219,220,221,222,223,6,45,122,3,59&season=",
                 yearto, "&month=0&season1=",
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
colnames(fangraphsPitching)<-c("Number","Name","Team","W","L","G","GS","SV","BS","IP","H","R","ER","HR","BB","IBB","HBP","HLD","SO","K_9","BB_9","HR_9","SwStr_","F_Strike_","Zone_","FBv","SL_","CB_","Pace","BABIP","IFFB_","LOB_","LD_","GB_","FB_","HR_FB","Pull_","Cent_","Oppo_","Soft_","Med_","Hard_","ERA","FIP","SIERA","Age","WAR")
# str(fangraphsPitching)

# Remove percentages
for(i in c(1,4:ncol(fangraphsPitching))) {
  fangraphsPitching[,i]<-str_trim(str_replace_all(fangraphsPitching[,i],"%",""))
}

# Char to Number
for(i in c(1,4:ncol(fangraphsPitching))) {
  fangraphsPitching[,i]<-as.numeric(as.character(fangraphsPitching[,i]))
}

# replace NA with 0
fangraphsPitching[is.na(fangraphsPitching)]<-0

# summary(fangraphsPitching)

# Data aquired from baseballheatmaps.com of current year leaderboard HTML table
y1=readHTMLTable(doc = "http://www.baseballheatmaps.com/graph/pitcherdistanceleader.php")
bhp.hrfb = data.frame(y1[1])
colnames(bhp.hrfb) <- c("Rank","Name","Stance","Year","Hits","Distance","Angle")
head(bhp.hrfb)

# Data aquired from baseballheatmaps.com of current year leaderboard HTML table
y2=readHTMLTable(doc = "http://www.baseballheatmaps.com/graph/pitcherdistancenobuntleader.php")
bhp.all.bb.dist = data.frame(y2[1])
colnames(bhp.all.bb.dist) <- c("Rank","Name","Year","Hits","Distance")
head(bhp.all.bb.dist)

# Combine baseballheatmaps.com leaderboard data
hrfbdist = bhp.hrfb; allbbdist = bhp.all.bb.dist
bhmPIT = sqldf("SELECT A.NAME,
               A.YEAR,
               avg(A.HITS) AS TOTALHRFB,
               avg(A.DISTANCE) AS HRFBDIST,
               avg(B.HITS) AS TOTALBABIP,
               avg(B.DISTANCE) BABIPDIST
               FROM hrfbdist A
               JOIN allbbdist B
               ON A.NAME = B.NAME
               AND A.YEAR = B.YEAR
               GROUP BY A.NAME,
               A.YEAR")

# fix inverted names
bhmPIT$Name = reverse_name(bhmPIT$Name)
bhmPIT$Name = str_replace_all(bhmPIT$Name,"Douglas Fister","Doug Fister")
bhmPIT$Name = str_replace_all(bhmPIT$Name,"Randall Wojciechowski","Asher Wojciechowski")

# str(fangraphsPitching)
xPitching = sqldf("SELECT a.Name, K_9,BB_9,HR_9,Round(SwStr_,2) SwStr_, BABIP, round(LOB_,2) LOB_, 
                  F_Strike_,Zone_,FBv,Pull_,Cent_,Oppo_,Soft_,Med_,Hard_,
                  round(HR_FB,2) HR_FB, era, fip,
                  round((K_9-BB_9)+SwStr_+FBv+WAR+Zone_+Soft_+(IP*.1)-ABS(Age-26),2) as STUFF,                  
                  round(HRFBDist) HRFBDist, round(BABIPDist) BBDist, 
                  FIP-ERA as Luck
                  from fangraphsPitching a 
                  join bhmPIT b on a.Name = b.Name
                  ")

xPitching$STUFF = rank(-xPitching$STUFF,ties.method= "max")

xPitching$BBDist[is.na(xPitching$BBDist)] <- 0

############### xPected FIP #######################
xFIP.fit = lm(FIP~SwStr_+Soft_+Hard_+STUFF+HRFBDist+BBDist, data=xPitching)
summary(xFIP.fit);
xFIP.output = cbind(xPitching,'xFIP'=round(xFIP.fit$fitted.values,2), 'Gap'=round(xFIP.fit$residuals,2))
xFIP.output = xFIP.output[order(xFIP.output$Gap),]
xFIP.output <- subset(xFIP.output, xFIP < 5 & xFIP > 2.5) # Removing outliers
head(xFIP.output,20); #SELL!!
tail(xFIP.output,20) #BUY!!