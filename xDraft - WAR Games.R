#################################################################################################

# Load libraries from github
library(devtools)
devtools::install_github('almartin82/projprep')
library(projprep)

library(devtools)
devtools::install_github('drewgriffith15/griffun')
library(griffun)

library(sqldf)

setwd("/Users/wgriffith2/Dropbox/R/MLB") #;getwd()

#################################################################################################

# Get ESPN league Position Eligibility for Hitters AND Pitchers
# EligibilityH <- griffun::ESPN_eligibility('bat')
# EligibilityP <- griffun::ESPN_eligibility('pit')

# Get ESPN league point projections for Hitters AND Pitchers in WAR Games league
WARGamesHitters <- griffun::WarGames_ESPN_proj('bat')
WARGamesPitchers <- griffun::WarGames_ESPN_proj('pit')

# Get last three years
yearto <-
  iif(as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
        1,as.numeric(format(Sys.time(), "%Y")))
yearfrom <-
  iif(as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
        3,as.numeric(format(Sys.time(), "%Y")) - 2)
currentyear <- as.numeric(format(Sys.time(), "%Y"))

# Scrape Fangraphs leaderboards from last three years
# fangraphsHitting <-
#   griffun::fangraphs_leaderboard('bat',yearfrom,yearto, 200, 0)
# fangraphsPitching <-
#   griffun::fangraphs_leaderboard('pit',yearfrom,yearto, 50, 0)

# steamer 600 Projections from Fangraphs
steamer600 <- projprep::get_fangraphs(currentyear,'steamer600')
steamerHitting600 <- steamer600$h
steamerPitching600 <- steamer600$p

# steamer  Projections from Fangraphs
steamer <- projprep::get_fangraphs(currentyear,'steamer')
steamerHitting <- steamer$h
steamerPitching <- steamer$p

# Zips  Projections from Fangraphs
zips <- projprep::get_fangraphs(currentyear,'zips')
zipsHitting <- zips$h
zipsPitching <- zips$p

# DepthCharts Projections from Fangraphs
DepthCharts <- projprep::get_fangraphs(currentyear,'fangraphsdc')
DepthChartsHitting <- DepthCharts$h
DepthChartsPitching <- DepthCharts$p

## END scraping

# Building Calculations for Batters
# Calculate singles in projections
steamerHitting$`1b` <-
  steamerHitting$h - (steamerHitting$`2b` + steamerHitting$`3b` + steamerHitting$hr)
steamerHitting600$`1b` <-
  steamerHitting600$h - (steamerHitting600$`2b` + steamerHitting600$`3b` + steamerHitting600$hr)
zipsHitting$`1b` <-
  zipsHitting$h - (zipsHitting$`2b` + zipsHitting$`3b` + zipsHitting$hr)
DepthChartsHitting$`1b` <-
  DepthChartsHitting$h - (DepthChartsHitting$`2b` + DepthChartsHitting$`3b` + DepthChartsHitting$hr)

# WAR Games hitting points
ab <- -1
x1b <- 6
x2b <- 9
x3b <- 12
hr <- 16
bb <- 3
sb <- 3
cs <- -4

steamerHitting$points <-
  round((steamerHitting$ab * ab) + (steamerHitting$`1b` * x1b) + (steamerHitting$`2b` * x2b) + (steamerHitting$`3b` * x3b) + (steamerHitting$hr * hr) + ((steamerHitting$bb + steamerHitting$hbp) * bb) + (steamerHitting$sb * sb) + (steamerHitting$cs * cs)
  )
steamerpts <-
  subset(steamerHitting, points > 100, select = c(fullname,mlbid,position,points,war))

steamerHitting600$points <-
  round((steamerHitting600$ab * ab) + (steamerHitting600$`1b` * x1b) + (steamerHitting600$`2b` * x2b) + (steamerHitting600$`3b` * x3b) + (steamerHitting600$hr * hr) + ((
    steamerHitting600$bb + steamerHitting600$hbp
  ) * bb) + (steamerHitting600$sb * sb) + (steamerHitting600$cs * cs)
  )
steamer600pts <-
  subset(steamerHitting600, points > 100, select = c(fullname,mlbid,position,points,war))

zipsHitting$points <-
  round((zipsHitting$ab * ab) + (zipsHitting$`1b` * x1b) + (zipsHitting$`2b` * x2b) + (zipsHitting$`3b` * x3b) + (zipsHitting$hr * hr) + ((zipsHitting$bb + zipsHitting$hbp) * bb) + (zipsHitting$sb * sb) + (zipsHitting$cs * cs)
  )
zipspts <-
  subset(zipsHitting, points > 100, select = c(fullname,mlbid,position,points))

DepthChartsHitting$points <-
  round((DepthChartsHitting$ab * ab) + (DepthChartsHitting$`1b` * x1b) + (DepthChartsHitting$`2b` * x2b) + (DepthChartsHitting$`3b` * x3b) + (DepthChartsHitting$hr * hr) + ((
    DepthChartsHitting$bb + DepthChartsHitting$hbp
  ) * bb) + (DepthChartsHitting$sb * sb) + (DepthChartsHitting$cs * cs)
  )
DepthChartspts <-
  subset(DepthChartsHitting, points > 100, select = c(fullname,mlbid,position,points,war))

# Clean Name
steamerpts$fullname <-
  str_trim(gsub('[^.a-zA-Z0-9]',' ',steamerpts$fullname))

# Combine all projections for hitting
batterProjections <-
  sqldf(
    "select distinct r.Name, sp.position as Position,
    sp.points as steamer,
    ifnull(sp600.points,sp.points) as steamer600,
    ifnull(zip.points,dc.points) as zips,
    dc.points as depthcharts,
    r.PTS as ESPN
    from WARGamesHitters r
    join steamerpts sp on lower(sp.fullname) = lower(r.Name)
    left join steamer600pts sp600 on sp600.mlbid = sp.mlbid
    left join zipspts zip on zip.mlbid = sp.mlbid
    left join DepthChartspts dc on dc.mlbid = sp.mlbid
    "
  )

# Some players may be left out bc they are currently FA or top rated prospects
batterProjections[is.na(batterProjections)] <- 0

batterProjections$AvgProj <-
  round((
    batterProjections$steamer + batterProjections$steamer600 + batterProjections$zips + batterProjections$depthcharts
  ) / 4
  )

batterProjections <-
  batterProjections[order(-batterProjections$AvgProj),]

# Check to see if any player(s) are missing from the final list
missingh <-
  sqldf(
    "select s.fullname, s.war, s.position from steamerpts s left join batterProjections p on lower(s.fullname) = lower(p.Name)
    where p.Name is null
    order by war desc
    "
  )

# Building Calculations for Pitchers
# WAR Games hitting points
gs <- -1
ip <- 4.5
so <- 2
bb_allowed <- -3
hr_allowed <- -13
sv <- 12
hld <- 9

steamerPitching$points <-
  round((steamerPitching$gs * gs) + (steamerPitching$ip * ip) + (steamerPitching$k * so) + ((steamerPitching$bb) * bb_allowed) + (steamerPitching$hr * hr_allowed) + (steamerPitching$sv * sv)
  )
steamerpts <-
  subset(steamerPitching, points > 100, select = c(fullname,mlbid,position,points,war))

steamerPitching600$points <-
  round((steamerPitching600$gs * gs) + (steamerPitching600$ip * ip) + (steamerPitching600$k * so) + ((steamerPitching600$bb) * bb_allowed) + (steamerPitching600$hr * hr_allowed) + (steamerPitching600$sv * sv)
  )
steamer600pts <-
  subset(steamerPitching600, points > 100, select = c(fullname,mlbid,position,points,war))

# Drop sv column from zips - current NA. going to pulling it in from steamer
zipsPitching <- subset(zipsPitching, select = -c(sv))

# Create sv stat in zips based on depthcharts
zipsPitching <- sqldf("select z.*, s.sv as sv
                      from DepthChartsPitching s
                      join zipsPitching z on z.mlbid = s.mlbid
                      ")

zipsPitching$points <-
  round((zipsPitching$gs * gs) + (zipsPitching$ip * ip) + (zipsPitching$k * so) + (zipsPitching$bb * bb_allowed) + (zipsPitching$hr * hr_allowed) + (zipsPitching$sv * sv)
  )
zipspts <-
  subset(zipsPitching, points > 100, select = c(fullname,mlbid,position,points,war))

DepthChartsPitching$points <-
  round((DepthChartsPitching$gs * gs) + (DepthChartsPitching$ip * ip) + (DepthChartsPitching$k * so) + (DepthChartsPitching$bb * bb_allowed) + (DepthChartsPitching$hr * hr_allowed) + (DepthChartsPitching$sv * sv)
  )
DepthChartspts <-
  subset(DepthChartsPitching, points > 100, select = c(fullname,mlbid,position,points,war))

# Clean Name
steamerpts$fullname <-
  str_trim(gsub('[^.a-zA-Z0-9]',' ',steamerpts$fullname))

pitcherProjections <- sqldf(
  "select distinct r.Name, sp.position as Position,
  sp.points as steamer,
  ifnull(sp600.points,sp.points) as steamer600,
  ifnull(zip.points,dc.points) as zips,
  dc.points as depthcharts,
  r.PTS as ESPN
  from WARGamesPitchers r
  join steamerpts sp on lower(sp.fullname) = lower(r.Name)
  left join steamer600pts sp600 on sp600.mlbid = sp.mlbid
  left join zipspts zip on zip.mlbid = sp.mlbid
  left join DepthChartspts dc on dc.mlbid = sp.mlbid
  "
)

# Some players may be left out bc they are currently FA or top rated prospects
pitcherProjections[is.na(pitcherProjections)] <- 0

pitcherProjections$AvgProj <-
  round((
    pitcherProjections$steamer + pitcherProjections$steamer600 + pitcherProjections$zips + pitcherProjections$depthcharts
  ) / 4
  )

pitcherProjections <-
  pitcherProjections[order(-pitcherProjections$AvgProj),]

# Check to see if any player(s) are missing from the final list
missingp <-
  sqldf(
    "select s.fullname, s.war, s.position from steamerpts s left join pitcherProjections p on lower(s.fullname) = lower(p.Name)
    where p.Name is null
    order by war desc
    ")

#################################################################################################

# Combine batting and pitching
projections <- rbind(batterProjections,pitcherProjections)
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
  c(
    "Rank","Name","Position","AVGPick","Roto_ROC","AvgValue","WeekADPAvgROC","Owned"
  )
# Remove Team Names from Player Name
ADP$Name <- gsub("\\,.*","",ADP$Name)
ADP.df <- sqldf('select Rank as RotoADP, Name, Roto_ROC
                from ADP p where Name is not null')

# Removing first two rows of non table data
ADP.df <- ADP.df[c(-1,-2),]
ADP.df$Roto_ROC <-
  round(as.numeric(gsub("+", "",ADP.df$Roto_ROC,fixed = TRUE)))
projections <- merge(projections,ADP.df,by = "Name",all.x = TRUE)
projections$RotoADP <- as.numeric(as.character(projections$RotoADP))
# projections$RotoADP <-
#   iif(is.na(projections$RotoADP),projections$AvgProjRank,projections$RotoADP)

# My draft pick vs League ADP
projections$WARGamesADP <-
  rank(-projections$ESPN,ties.method = "first")

# Sort so we can produce expected round based on 10 team league
projections <- projections[order(-projections$WARGamesADP),]

# Assign ADP round based on 10 team league
projections$WARGamesRound <-
  rep(1:110, each = 10, length = nrow(projections))

# Difference between my projections and ESPN ADP on ESPN (converted to rounds)
# Assign my expected round based on 10 team league based on ESPN projections
# LOOKING FOR NEGATIVE NUMBERS - GOOD!
projections$RoundDiff <-
  round((projections$MyDraftRound - projections$WARGamesRound))

# Show output
head(projections,100)

# Show missing players (most likely name mismatches between ESPN and fangraphs)
missing<-rbind(missingp, missingh); head(missing)

# Export to WD
csv_name <- paste("Draft",currentyear,".csv", sep = '')
write.table(projections,file = csv_name,sep = ",",row.names = F)

#################################################################################################
#################################################################################################
