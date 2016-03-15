# URL for our leagues projections, which will determine draft order everyone will use:
# http://games.espn.go.com/flb/tools/projections?leagueId=86607

# Load libraries
library("griffun")
library("XML")
library("stringr")
library("ggplot2")
library("plyr")
library("data.table")
library("sqldf")

#################################################################################################

# Get ESPN league Position Eligibility for Hitters AND Pitchers
EligibilityH <- ESPN.eligibility()$Hitters
EligibilityP <- ESPN.eligibility()$Pitchers

# Get ESPN league point projections for Hitters AND Pitchers in WAR Games league
WARGamesHitters <- ESPNProjections(86607)$Hitters
WARGamesPitchers <- ESPNProjections(86607)$Pitchers

# Get last three years
yearto <-
  iif(as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
        1,as.numeric(format(Sys.time(), "%Y")))
yearfrom <-
  iif(as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
        3,as.numeric(format(Sys.time(), "%Y")) - 2)

# Scrape Fangraphs leaderboards from last three years
fangraphsBatting <- FanGraphs.hitting(yearfrom,yearto, 200, 0) # min 200 PA
# str(fangraphsBatting)

fangraphsPitching <- FanGraphs.pitching(yearfrom,yearto, 50, 0) # min 50 IP
# str(fangraphsPitching)

# Streamer 600 Projections on Fangraphs
SteamerBatting600 <- SteamerBatting600()
# str(SteamerBatting600)

SteamerPitching600<-SteamerPitching600()
# str(SteamerPitching600)

#################################################################################################

# Scape This Year's Batting Projections Steamer

# Streamer  Projections on Fangraphs
base_url <-
  "http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer&team="
teams <-
  list(
    "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
  )
urls <- paste0(base_url, rep(teams, each = 1),
               "&players=0&sort=26,d")
# Scrape
SteamerB <-
  lapply(urls, function(x) {
    data.table(
      readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$ProjectionBoard1_dg1_ctl00
    )
  })

# Combine Scrapes
SteamerBatting <- c()
for (i in 1:30) {
  SteamerBatting <- as.data.frame(rbind(SteamerBatting,SteamerB[[i]]))
}

# Rename columns
colnames(SteamerBatting) <-
  c(
    "Name","Team","V2","PA","AB","H","x2B","x3B","HR","R","RBI","BB","SO","HBP","SB","CS","AVG","OBP","SLG","OPS","wOBA","wRC_","BsR","Fld","Off","Def","WAR"
  )

# Remove percentages
for (i in 3:ncol(SteamerBatting)) {
  SteamerBatting[,i] <-
    str_trim(str_replace_all(SteamerBatting[,i],"%",""))
}

# Char to Number
for (i in 3:ncol(SteamerBatting)) {
  SteamerBatting[,i] <- as.numeric(as.character(SteamerBatting[,i]))
}

# replace NA with 0
SteamerBatting[is.na(SteamerBatting)] <- 0

# summary(SteamerBatting)

#################################################################################################

# Scape This Year's Pitching Projections Steamer

# Streamer  Projections on Fangraphs
base_url <-
  "http://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=steamer&team="
teams <-
  list(
    "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
  )
urls <- paste0(base_url, rep(teams, each = 1),
               "&players=0&sort=19,d")
# Scrape
SteamerP <-
  lapply(urls, function(x) {
    data.table(
      readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$ProjectionBoard1_dg1_ctl00
    )
  })

# Combine Scrapes
SteamerPitching <- c()
for (i in 1:30) {
  SteamerPitching <-
    as.data.frame(rbind(SteamerPitching,SteamerP[[i]]))
}

# Rename columns
colnames(SteamerPitching) <-
  c(
    "Name","Team","V2","W","L","ERA","GS","G","SV","IP","H","ER","HR","SO","BB","WHIP","K_9","BB_9","FIP","WAR","RA9_WAR"
  )

# Remove percentages
for (i in 3:ncol(SteamerPitching)) {
  SteamerPitching[,i] <-
    str_trim(str_replace_all(SteamerPitching[,i],"%",""))
}

# Char to Number
for (i in 3:ncol(SteamerPitching)) {
  SteamerPitching[,i] <- as.numeric(as.character(SteamerPitching[,i]))
}

# replace NA with 0
SteamerPitching[is.na(SteamerPitching)] <- 0

# summary(SteamerPitching)

#################################################################################################

# Scape This Year's Batting Projections Zips

# Zips  Projections on Fangraphs
base_url <-
  "http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=zips&team="
teams <-
  list(
    "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
  )
urls <- paste0(base_url, rep(teams, each = 1),
               "&players=0&sort=24,d")
# Scrape
zipsB <-
  lapply(urls, function(x) {
    data.table(
      readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$ProjectionBoard1_dg1_ctl00
    )
  })

# Combine Scrapes
zipsBatting <- c()
for (i in 1:30) {
  zipsBatting <- as.data.frame(rbind(zipsBatting,zipsB[[i]]))
}

# Rename columns
colnames(zipsBatting) <-
  c(
    "Name","Team","V2","G","PA","AB","H","x2B","x3B","HR","R","RBI","BB","SO","HBP","SB","CS","AVG","OBP","SLG","OPS","wOBA","Fld","BsR","WAR"
  )

# Remove percentages
for (i in 3:ncol(zipsBatting)) {
  zipsBatting[,i] <- str_trim(str_replace_all(zipsBatting[,i],"%",""))
}

# # Char to Number
for (i in 3:ncol(zipsBatting)) {
  zipsBatting[,i] <- as.numeric(as.character(zipsBatting[,i]))
}

# replace NA with 0
zipsBatting[is.na(zipsBatting)] <- 0

# summary(zipsBatting)

#################################################################################################

# Scape This Year's Pitching Projections Zips

# Zips  Projections on Fangraphs
base_url <-
  "http://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=zips&team="
teams <-
  list(
    "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
  )
urls <- paste0(base_url, rep(teams, each = 1),
               "&players=0&sort=18,d")
# Scrape
zipsP <-
  lapply(urls, function(x) {
    data.table(
      readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$ProjectionBoard1_dg1_ctl00
    )
  })

# Combine Scrapes
zipsPitching <- c()
for (i in 1:30) {
  zipsPitching <- as.data.frame(rbind(zipsPitching,zipsP[[i]]))
}

# Rename columns
colnames(zipsPitching) <-
  c(
    "Name","Team","V2","W","L","ERA","GS","G","IP","H","ER","HR","SO","BB","WHIP","K_9","BB_9","FIP","WAR"
  )

# Remove percentages
for (i in 3:ncol(zipsPitching)) {
  zipsPitching[,i] <-
    str_trim(str_replace_all(zipsPitching[,i],"%",""))
}

# Char to Number
for (i in 3:ncol(zipsPitching)) {
  zipsPitching[,i] <- as.numeric(as.character(zipsPitching[,i]))
}

# replace NA with 0
zipsPitching[is.na(zipsPitching)] <- 0

# summary(zipsPitching)

#################################################################################################
#################################################################################################

# Building Calculations for Batters

# Calculate singles in Steamer projections
SteamerBatting$x1B <-
  SteamerBatting$H - (SteamerBatting$x2B + SteamerBatting$x3B + SteamerBatting$HR)
SteamerBatting600$x1B <-
  SteamerBatting600$H - (SteamerBatting600$x2B + SteamerBatting600$x3B + SteamerBatting600$HR)
zipsBatting$x1B <-
  zipsBatting$H - (zipsBatting$x2B + zipsBatting$x3B + zipsBatting$HR)

batterProjections <-
  sqldf(
    "select distinct r.Name, e.Position,
    sp.PA,sp.AB,sp.x1B,sp.x2B,sp.x3B,sp.HR,sp.BB,sp.SO,sp.HBP,sp.SB,sp.CS,sp.wOBA,sp.WAR, r.PTS as ESPN,
    round(((fp.Spd/10)*.1)+fp.BABIP+((fp.Contact_/100)/4)+(fp.ISO*2)+(fp.wOBA*1)-ABS((fp.Age/100)-.26),4) as Skill,
    round((sp.AB*-1)+(sp.x1B*6)+(sp.x2B*9)+(sp.x3B*12)+(sp.HR*16)+((sp.BB+sp.HBP)*3)+(sp.SB*3)+(sp.CS*-4)) as Steamer,
    round((sp600.AB*-1)+(sp600.x1B*6)+(sp600.x2B*9)+(sp600.x3B*12)+(sp600.HR*16)+((sp600.BB+sp600.HBP)*3)+(sp600.SB*3)+(sp600.CS*-4)) as Steamer600,
    round((zip.AB*-1)+(zip.x1B*6)+(zip.x2B*9)+(zip.x3B*12)+(zip.HR*16)+((zip.BB+zip.HBP)*3)+(zip.SB*3)+(zip.CS*-4)) as Zips,
    round((sp.AB*-1)+(((fp.x1B/fp.AB)*sp.AB)*6)+(((fp.x2B/fp.AB)*sp.PA)*9)+(((fp.x3B/fp.AB)*sp.AB)*12)+(((fp.HR/fp.AB)*sp.AB)*16)+((((fp.BB+fp.HBP)/fp.AB)*sp.AB)*3)+(((fp.SB/fp.AB)*sp.AB)*3)+(((fp.CS/fp.AB)*sp.AB)*-4)) as Avg3Yrs
    from WARGamesHitters r
    join EligibilityH e on e.Name = r.Name
    left join SteamerBatting sp on trim(upper(sp.Name)) = trim(upper(r.Name))
    left join SteamerBatting600 sp600 on lower(sp600.Name) = lower(r.Name)
    left join fangraphsBatting fp on lower(fp.Name) = lower(r.Name)
    left join zipsBatting zip on lower(zip.Name) = lower(r.Name)
    "
  )

# Some players may be left out bc they are currently FA or top rated prospects
batterProjections[is.na(batterProjections)] <- 0
batterProjections$Avg3Yrs <-
  iif(batterProjections$Avg3Yrs == 0,batterProjections$Zips,batterProjections$Avg3Yrs)

batterProjections$AvgProj <-
  round((
    batterProjections$Steamer + batterProjections$Avg3Yrs + batterProjections$Steamer600 +
      batterProjections$Zips
  ) / 4
  )
batterProjections$Skill <-
  rank(-batterProjections$Skill,ties.method = "max")

batterProjections <-
  batterProjections[order(-batterProjections$AvgProj),]
# batterProjections
sub.batterProjections <-
  subset(
    batterProjections, AvgProj > 0, select = c(
      "Name","Position","WAR","Steamer","Steamer600","Zips","Avg3Yrs","AvgProj","ESPN","Skill"
    )
  )

# Matt Duffy showing multiple times for some odd reason
sub.batterProjections <-
  sub.batterProjections[!duplicated(sub.batterProjections$Name),]

#################################################################################################

# Building Calculations for Pitchers

pitcherProjections <-
  sqldf(
    "select distinct r.Name, e.Position, sp.IP,
    sp.HR,sp.SO,sp.BB,sp.WHIP,sp.K_9,sp.BB_9,sp.FIP,sp.WAR, round(r.PTS) as ESPN,
    round((fp.K_9-fp.BB_9)+fp.SwStr_+(fp.FBv*.1)+
    (fp.F_Strike_*.1)+(fp.Zone_*.1)+(fp.F_Strike_*.1)+(fp.War*2)-ABS(fp.Age - 26), 4) AS Skill,
    round((sp.IP*4.5)+(sp.SO*2)+((sp.BB)*-3)+(sp.HR*-13)+(sp.SV*12)) as Steamer,
    round((sp600.IP*4.5)+(sp600.SO*2)+((sp600.BB)*-3)+(sp600.HR*-13)+(sp600.SV*12)) as Steamer600,
    round((zip.IP*4.5)+(zip.SO*2)+((zip.BB)*-3)+(zip.HR*-13)+(sp.SV*12)) as Zips,
    round((sp.IP*4.5)+(((fp.SO / fp.IP)*sp.IP)*2)+(((fp.BB/fp.IP)*sp.IP)*-3)+
    (((fp.HR/fp.IP)*sp.IP)*-13)+(((fp.SV/fp.IP)*sp.IP)*12)+(((fp.HLD/fp.IP)*sp.IP)*9)) as Avg3Yrs
    from WARGamesPitchers r
    join EligibilityP e on e.Name = r.Name
    left join SteamerPitching sp on lower(sp.Name) = lower(r.Name)
    left join SteamerPitching600 sp600 on lower(sp600.Name) = lower(r.Name)
    left join fangraphsPitching fp on lower(fp.Name) = lower(r.Name)
    left join zipsPitching zip on lower(zip.Name) = lower(r.Name)
    "
  )

# Some players may be left out bc they are currently FA or top rated prospects
pitcherProjections[is.na(pitcherProjections)] <- 0
pitcherProjections$Avg3Yrs <-
  iif(
    pitcherProjections$Avg3Yrs == 0,pitcherProjections$Zips,pitcherProjections$Avg3Yrs
  )

pitcherProjections$AvgProj <-
  round((
    pitcherProjections$Steamer + pitcherProjections$Avg3Yrs + pitcherProjections$Steamer600 +
      pitcherProjections$Zips
  ) / 4
  )
pitcherProjections$Skill <-
  rank(-pitcherProjections$Skill,ties.method = "max")

pitcherProjections <-
  pitcherProjections[order(-pitcherProjections$AvgProj),]
# pitcherProjections
sub.pitcherProjections <-
  subset(
    pitcherProjections, AvgProj > 0, select = c(
      "Name","Position","WAR","Steamer","Steamer600","Zips","Avg3Yrs","AvgProj","ESPN","Skill"
    )
  )

# Just in case there's dups here too...
sub.pitcherProjections <-
  sub.pitcherProjections[!duplicated(sub.pitcherProjections$Name),]

#################################################################################################

# Combine batting and pitching
projections <- rbind(sub.batterProjections,sub.pitcherProjections)
projections$MyDraft <-
  rank(-projections$AvgProj,ties.method = "first");
projections <- projections[order(projections$MyDraft),]
projections$MyDraftRound <-
  rep(1:110, each = 10, length = nrow(projections))

# Get ESPN ADP
ESPN.ADP <- "http://games.espn.go.com/flb/livedraftresults"
ADP <- readHTMLTable(ESPN.ADP)
ADP <- as.data.frame(ADP[2])

colnames(ADP) <-
  c("Rank","Name","Position","AVGPick","7DayROC","AvgValue","7DayAvgROC","Owned")
# Remove Team Names from Player Name
ADP$Name <- gsub("\\,.*","",ADP$Name)
ADP.df <- sqldf('select Rank as RotoADP, Name
                from ADP p where Name is not null')

# Removing first two rows of non table data
ADP.df <- ADP.df[c(-1,-2),]
projections <- merge(projections,ADP.df,by = "Name",all.x = TRUE)
projections$RotoADP <- as.numeric(as.character(projections$RotoADP))
projections$RotoADP <-
  iif(is.na(projections$RotoADP),projections$AvgProjRank,projections$RotoADP)

# My draft pick vs League ADP
projections$LeagueADP <-
  rank(-projections$ESPN,ties.method = "first")

# Sort so we can produce expected round based on 10 team league
projections <- projections[order(-projections$ESPN),]

# Assign ADP round based on 10 team league
projections$Round <-
  rep(1:110, each = 10, length = nrow(projections))

# Difference between my projections and ESPN ADP on ESPN (converted to rounds)
# Assign my expected round based on 10 team league based on ESPN projections
# LOOKING FOR NEGATIVE NUMBERS - GOOD!
projections$RoundDiff <-
  round((projections$MyDraftRound - projections$Round))

# Reorder columns
projections <- projections[c(1:10,13,11,12,14,15,16)]

# Output
head(projections,50)

# Export to WD
csv_name <- paste("Draft",".csv", sep = '')
# write.table(projections,file = csv_name,sep = ",",row.names = F)

#################################################################################################
#################################################################################################
