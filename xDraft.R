# Load libraries
library("griffun")
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")
library("sqldf")

# Get years
yearto<-iif(as.numeric(format(Sys.time(), "%m%d"))<=331,as.numeric(format(Sys.time(), "%Y"))-1,as.numeric(format(Sys.time(), "%Y")))
yearfrom<-iif(as.numeric(format(Sys.time(), "%m%d"))<=331,as.numeric(format(Sys.time(), "%Y"))-3,as.numeric(format(Sys.time(), "%Y"))-2)

#################################################################################################

# Get ESPN Rankings for 2015
ESPN<-"http://espn.go.com/fantasy/baseball/story/_/page/2015FLBrankings/updated-fantasy-baseball-rankings-position-2015-season"
thc<-readHTMLTable(ESPN)
PlayerRankings<-as.data.frame(thc[1])
colnames(PlayerRankings)<-c("ESPNRank","Name","Team","Position","PositionRank","Age","PreviousPlayerRater","DollarValue")
Rankings<-sqldf('select ESPNRank,Name,Team,Position,PositionRank,Age
                from PlayerRankings p where Name is not null')

# Factor to Number
for(i in c(1,6:ncol(Rankings))) {
  Rankings[,i]<-as.numeric(as.character(Rankings[,i]))
}

# Factor to Char
for(i in c(2:5)) {
  Rankings[,i]<-as.character(Rankings[,i])
} 
# str(Rankings)

#################################################################################################

# Scrape Fangraphs Batting from last three years

base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=200&type=c,4,5,6,8,9,10,11,14,15,16,17,18,19,20,21,22,23,34,35,40,41,43,44,45,46,47,60,107,50,58,3&season=",
                 yearto, "&month=0&season1=",
                 yearfrom, "&ind=0&team=")
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
colnames(fangraphsBatting)<-c("Number","Name","Team","G","AB","PA","x1B","x2B","x3B","HR","BB","IBB","SO","HBP","SF","SH","GDP","SB","CS","AVG","BB_","K_","ISO","BABIP","LD_","GB_","FB_","IFFB_","HR_FB","Spd","Contact_","wOBA","WAR","Age")

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

#################################################################################################

# Scrape Fangraphs Pitching from last three years

base_url<-paste0("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=50&type=c,4,5,7,8,11,12,13,15,16,17,18,19,20,21,114,24,36,37,113,112,111,76,77,81,211,43,50,44,47,48,49,51,6,45,122,3,59&season=",
                 yearto, "&month=0&season1=",
                 yearfrom, "&ind=0&team=")
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
colnames(fangraphsPitching)<-c("Number","Name","Team","W","L","G","GS","SV","BS","IP","H","R","ER","HR","BB","IBB","HBP","HLD","SO","K_9","BB_9","SwStr_","F_Strike_","Zone_","FBv","SL_","CB_","Pace","BABIP","IFFB_","LOB_","LD_","GB_","FB_","HR_FB","ERA","FIP","SIERA","Age","WAR")
# str(df.pitching)

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

#################################################################################################

# Scape This Year's Batting Projections for Steamer600

# Streamer 600 Projections on Fangraphs
base_url<-"http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer600&team="
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&players=0&sort=26,d")
# Scrape
SteamerB600<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$ProjectionBoard1_dg1_ctl00)})

# Combine Scrapes
SteamerBatting600<-c()
for(i in 1:30) {
  SteamerBatting600<-as.data.frame(rbind(SteamerBatting600,SteamerB600[[i]]))
}

# Rename columns
colnames(SteamerBatting600)<-c("Name","Team","PA","AB","H","x2B","x3B","HR","R","RBI","BB","SO","HBP","SB","CS","AVG","OBP","SLG","OPS","wOBA","wRC_","BsR","Fld","Off","Def","WAR")

# Remove percentages
for(i in 3:ncol(SteamerBatting600)) {
  SteamerBatting600[,i]<-str_trim(str_replace_all(SteamerBatting600[,i],"%",""))
}

# Char to Number
for(i in 3:ncol(SteamerBatting600)) {
  SteamerBatting600[,i]<-as.numeric(as.character(SteamerBatting600[,i]))
}

# replace NA with 0
SteamerBatting600[is.na(SteamerBatting600)]<-0

# summary(SteamerBatting600)

#################################################################################################

# Scape This Year's Pitching Projections Steamer600

# Streamer 600 Projections on Fangraphs
base_url<-"http://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=steamer600&team="
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&players=0&sort=19,d")
# Scrape
SteamerP600<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$ProjectionBoard1_dg1_ctl00)})

# Combine Scrapes
SteamerPitching600<-c()
for(i in 1:30) {
  SteamerPitching600<-as.data.frame(rbind(SteamerPitching600,SteamerP600[[i]]))
}

# Rename columns
colnames(SteamerPitching600)<-c("Name","Team","W","L","ERA","GS","G","SV","IP","H","ER","HR","SO","BB","WHIP","K_9","BB_9","FIP","WAR","RA9_WAR")

# Remove percentages
for(i in 3:ncol(SteamerPitching600)) {
  SteamerPitching600[,i]<-str_trim(str_replace_all(SteamerPitching600[,i],"%",""))
}

# Char to Number
for(i in 3:ncol(SteamerPitching600)) {
  SteamerPitching600[,i]<-as.numeric(as.character(SteamerPitching600[,i]))
}

# replace NA with 0
SteamerPitching600[is.na(SteamerPitching600)]<-0 

# summary(SteamerPitching600)

#################################################################################################

# Scape This Year's Batting Projections Steamer

# Streamer  Projections on Fangraphs
base_url<-"http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer&team="
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&players=0&sort=26,d")
# Scrape
SteamerB<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$ProjectionBoard1_dg1_ctl00)})

# Combine Scrapes
SteamerBatting<-c()
for(i in 1:30) {
  SteamerBatting<-as.data.frame(rbind(SteamerBatting,SteamerB[[i]]))
}

# Rename columns
colnames(SteamerBatting)<-c("Name","Team","PA","AB","H","x2B","x3B","HR","R","RBI","BB","SO","HBP","SB","CS","AVG","OBP","SLG","OPS","wOBA","wRC_","BsR","Fld","Off","Def","WAR")

# Remove percentages
for(i in 3:ncol(SteamerBatting)) {
  SteamerBatting[,i]<-str_trim(str_replace_all(SteamerBatting[,i],"%",""))
}

# Char to Number
for(i in 3:ncol(SteamerBatting)) {
  SteamerBatting[,i]<-as.numeric(as.character(SteamerBatting[,i]))
}

# replace NA with 0
SteamerBatting[is.na(SteamerBatting)]<-0

# summary(SteamerBatting)

#################################################################################################

# Scape This Year's Pitching Projections Steamer

# Streamer  Projections on Fangraphs
base_url<-"http://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=steamer&team="
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&players=0&sort=19,d")
# Scrape
SteamerP<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$ProjectionBoard1_dg1_ctl00)})

# Combine Scrapes
SteamerPitching<-c()
for(i in 1:30) {
  SteamerPitching<-as.data.frame(rbind(SteamerPitching,SteamerP[[i]]))
}

# Rename columns
colnames(SteamerPitching)<-c("Name","Team","W","L","ERA","GS","G","SV","IP","H","ER","HR","SO","BB","WHIP","K_9","BB_9","FIP","WAR","RA9_WAR")

# Remove percentages
for(i in 3:ncol(SteamerPitching)) {
  SteamerPitching[,i]<-str_trim(str_replace_all(SteamerPitching[,i],"%",""))
}

# Char to Number
for(i in 3:ncol(SteamerPitching)) {
  SteamerPitching[,i]<-as.numeric(as.character(SteamerPitching[,i]))
}

# replace NA with 0
SteamerPitching[is.na(SteamerPitching)]<-0 

# summary(SteamerPitching)

#################################################################################################

# Scape This Year's Batting Projections Zips

# Zips  Projections on Fangraphs
base_url<-"http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=zips&team="
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&players=0&sort=24,d")
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

# summary(zipsBatting)

#################################################################################################

# Scape This Year's Pitching Projections Zips

# Zips  Projections on Fangraphs
base_url<-"http://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=zips&team="
teams<-list("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30")
urls<-paste0(base_url, rep(teams, each=1),
             "&players=0&sort=18,d")
# Scrape
zipsP<-lapply(urls, function(x){data.table(readHTMLTable(x, as.data.frame=TRUE, stringsAsFactors=FALSE)$ProjectionBoard1_dg1_ctl00)})

# Combine Scrapes
zipsPitching<-c()
for(i in 1:30) {
  zipsPitching<-as.data.frame(rbind(zipsPitching,zipsP[[i]]))
}

# Rename columns
colnames(zipsPitching)<-c("Name","Team","W","L","ERA","GS","G","IP","H","ER","HR","SO","BB","WHIP","K_9","BB_9","FIP","WAR")

# Remove percentages
for(i in 3:ncol(zipsPitching)) {
  zipsPitching[,i]<-str_trim(str_replace_all(zipsPitching[,i],"%",""))
}

# Char to Number
for(i in 3:ncol(zipsPitching)) {
  zipsPitching[,i]<-as.numeric(as.character(zipsPitching[,i]))
}

# replace NA with 0
zipsPitching[is.na(zipsPitching)]<-0 

# summary(zipsPitching)

#################################################################################################
#################################################################################################

# Building Calculations for Batters

# Calculate singles in Steamer projections
SteamerBatting$x1B<-SteamerBatting$H-(SteamerBatting$x2B+SteamerBatting$x3B+SteamerBatting$HR)
SteamerBatting600$x1B<-SteamerBatting600$H-(SteamerBatting600$x2B+SteamerBatting600$x3B+SteamerBatting600$HR)
zipsBatting$x1B<-zipsBatting$H-(zipsBatting$x2B+zipsBatting$x3B+zipsBatting$HR)

batterProjections<-sqldf("select distinct r.Name, r.team, r.Position, r.Age,
                         sp.PA,sp.AB,sp.x1B,sp.x2B,sp.x3B,sp.HR,sp.BB,sp.SO,sp.HBP,sp.SB,sp.CS,sp.wOBA,sp.WAR, r.ESPNRank,
                         round(((fp.Spd/10)*.1)+fp.BABIP+((fp.Contact_/100)/4)+(fp.ISO*2)+(fp.wOBA*1)-ABS((fp.Age/100)-.26),4) as Skill,
                         round((sp.AB*-1)+(sp.x1B*6)+(sp.x2B*9)+(sp.x3B*12)+(sp.HR*16)+((sp.BB+sp.HBP)*3)+(sp.SB*3)+(sp.CS*-4)) as Steamer,
                         round((sp600.AB*-1)+(sp600.x1B*6)+(sp600.x2B*9)+(sp600.x3B*12)+(sp600.HR*16)+((sp600.BB+sp600.HBP)*3)+(sp600.SB*3)+(sp600.CS*-4)) as Steamer600,
                         round((zip.AB*-1)+(zip.x1B*6)+(zip.x2B*9)+(zip.x3B*12)+(zip.HR*16)+((zip.BB+zip.HBP)*3)+(zip.SB*3)+(zip.CS*-4)) as Zips,
                         round((sp.AB*-1)+(((fp.x1B/fp.PA)*sp.PA)*6)+(((fp.x2B/fp.PA)*sp.PA)*9)+(((fp.x3B/fp.PA)*sp.PA)*12)+(((fp.HR/fp.PA)*sp.PA)*16)+((((fp.BB+fp.HBP)/fp.PA)*sp.PA)*3)+(((fp.SB/fp.PA)*sp.PA)*3)+(((fp.CS/fp.PA)*sp.PA)*-4)) as Avg3Yrs                        
                         from Rankings r
                         join SteamerBatting sp on upper(sp.Name) = upper(r.Name)
                         left join SteamerBatting600 sp600 on lower(sp600.Name) = lower(r.Name)
                         left join fangraphsBatting fp on lower(fp.Name) = lower(r.Name)
                         left join zipsBatting zip on lower(zip.Name) = lower(r.Name)
                         where r.Position not in ('SP','RP','SP/RP', 'RP/SP')
                         and sp.PA > 1
                         ")

# Some players may be left out bc they are currently FA or top rated prospects
batterProjections[is.na(batterProjections)]<-0
batterProjections$Avg3Yrs <- iif(batterProjections$Avg3Yrs==0,batterProjections$Zips,batterProjections$Avg3Yrs)

batterProjections$AvgProj<-round((batterProjections$Steamer+batterProjections$Avg3Yrs+batterProjections$Steamer600+batterProjections$Zips)/4)
batterProjections$Skill<-rank(-batterProjections$Skill,ties.method="max")

batterProjections<-batterProjections[order(-batterProjections$AvgProj),]
# batterProjections
sub.batterProjections<-subset(batterProjections, AvgProj > 0, select = c("Name","Team","Position","Age","WAR","Steamer","Steamer600","Zips","Avg3Yrs","AvgProj","ESPNRank","Skill"))

#################################################################################################

# Building Calculations for Pitchers

pitcherProjections<-sqldf("select distinct r.Name, r.team, r.Position, r.Age,sp.IP,
                          sp.HR,sp.SO,sp.BB,sp.WHIP,sp.K_9,sp.BB_9,sp.FIP,sp.WAR,r.ESPNRank,
                          round((fp.K_9-fp.BB_9)+fp.SwStr_+(fp.FBv*.1)+
                          (fp.F_Strike_*.1)+(fp.Zone_*.1)+(fp.F_Strike_*.1)+(fp.War*2)-ABS(fp.Age - 26), 4) AS Skill,
                          round((sp.IP*4.5)+(sp.SO*2)+((sp.BB)*-3)+(sp.HR*-13)+(sp.SV*12)) as Steamer,
                          round((sp600.IP*4.5)+(sp600.SO*2)+((sp600.BB)*-3)+(sp600.HR*-13)+(sp600.SV*12)) as Steamer600,
                          round((zip.IP*4.5)+(zip.SO*2)+((zip.BB)*-3)+(zip.HR*-13)+(sp.SV*12)) as Zips,
                          round((sp.IP*4.5)+(((fp.SO / fp.IP)*sp.IP)*2)+(((fp.BB/fp.IP)*sp.IP)*-3)+
                          (((fp.HR/fp.IP)*sp.IP)*-13)+(((fp.SV/fp.IP)*sp.IP)*12)+(((fp.HLD/fp.IP)*sp.IP)*9)) as Avg3Yrs                                
                          from Rankings r
                          left join SteamerPitching sp on lower(sp.Name) = lower(r.Name)
                          left join SteamerPitching600 sp600 on lower(sp600.Name) = lower(r.Name)
                          left join fangraphsPitching fp on lower(fp.Name) = lower(r.Name)
                          left join zipsPitching zip on lower(zip.Name) = lower(r.Name)
                          where r.Position in ('SP','RP','SP/RP', 'RP/SP')
                          ")

# Some players may be left out bc they are currently FA or top rated prospects
pitcherProjections[is.na(pitcherProjections)]<-0
pitcherProjections$Avg3Yrs <- iif(pitcherProjections$Avg3Yrs==0,pitcherProjections$Zips,pitcherProjections$Avg3Yrs)

pitcherProjections$AvgProj<-round((pitcherProjections$Steamer+pitcherProjections$Avg3Yrs+pitcherProjections$Steamer600+pitcherProjections$Zips)/4)
pitcherProjections$Skill<-rank(-pitcherProjections$Skill,ties.method="max")

pitcherProjections<-pitcherProjections[order(-pitcherProjections$AvgProj),]
# pitcherProjections
sub.pitcherProjections<-subset(pitcherProjections, AvgProj > 0, select = c("Name","Team","Position","Age","WAR","Steamer","Steamer600","Zips","Avg3Yrs","AvgProj","ESPNRank","Skill"))

#################################################################################################

# Combine batting and pitching
projections<-rbind(sub.batterProjections,sub.pitcherProjections)
projections$AvgProjRank<-rank(-projections$AvgProj,ties.method="first")

# Get ESPN ADP for 2015
ESPN.ADP<-"http://games.espn.go.com/flb/livedraftresults"
ADP<-readHTMLTable(ESPN.ADP)
ADP<-as.data.frame(ADP[2])

colnames(ADP)<-c("Rank","Name","Position","AVGPick","7DayROC","AvgValue","7DayAvgROC","Owned")
# Remove Team Names from Player Name
ADP$Name <- gsub("\\,.*","",ADP$Name)
ADP.df<-sqldf('select Rank as ADP, Name
              from ADP p where Name is not null')

# Removing first two rows of non table data
ADP.df <- ADP.df[c(-1,-2),]
projections <- merge(projections,ADP.df,by="Name",all.x=TRUE)
projections$ADP <- as.numeric(as.character(projections$ADP))
projections$ADP <- iif(is.na(projections$ADP),projections$AvgProjRank,projections$ADP)

# Adding Draft column
projections$Draft<-round(rowMeans(projections[,11:13], na.rm = TRUE))
projections$Draft<-rank(projections$Draft,ties.method="first")
projections<-projections[order(projections$ADP),]

# Assign expected round based on 10 team league
projections$Round<-rep(1:100, each = 10, length = nrow(projections))

# Difference between my projections and ADP on ESPN (converted to rounds)
projections$Diff <- round((projections$Draft - projections$ADP) / 10,0)

# Sort best to last
projections<-projections[order(projections$ADP),]

# Output
projections

# Export to WD
csv_name<-paste("Draft",".csv", sep = '')
write.table(projections,file=csv_name,sep=",",row.names=F)

#################################################################################################
#################################################################################################
