# Analysis based on following articles:
# http://www.fangraphs.com/fantasy/muscling-up-iso-surgers/
# http://www.fangraphs.com/fantasy/2015-expected-iso-xiso-in-review/
# http://www.fangraphs.com/fantasy/new-hitter-xbabip-based-on-bis-batted-ball-data/
# http://www.fangraphs.com/fantasy/the-xhrfb-rate-equation-unmasked/
# http://www.fangraphs.com/fantasy/is-it-time-to-move-past-hrfb-rate/
# http://www.fangraphs.com/fantasy/espn-home-run-tracker-analysis-the-2016-hrfb-downsiders/
# http://www.baseballprospectus.com/article.php?articleid=28956
# http://www.fangraphs.com/blogs/launch-angle-matt-duffy-and-potential-power-surges/
# http://www.hardballtimes.com/improving-projections-with-exit-velocity/
# http://www.fangraphs.com/fantasy/xoba-and-using-statcast-data-to-measure-offensive/

# Load libraries from github
library(devtools)
devtools::install_github('drewgriffith15/griffun')
library(griffun)
devtools::install_github('almartin82/projprep')
library(projprep)
library(sqldf)

setwd("/Users/wgriffith2/Dropbox/R/MLB") #;getwd()

currentyear <-
  iif(as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
        1,as.numeric(format(Sys.time(), "%Y")))
last30date <- as.character(as.Date(format(Sys.time(), "%Y-%m-%d")) - 30)

q <- 60 # PAs qualifier (min PAs)

# Scrape Fangraphs last 30 days; get last year if its before the season starts
fangraphsHitting <-
  griffun::fangraphs_leaderboard('bat',currentyear,currentyear,qual = q, split = 3)

# Calculated HR/Batted Ball
fangraphsHitting$HR_BB <-
  iif(fangraphsHitting$HR > 0,round((
    fangraphsHitting$HR / (
      fangraphsHitting$LD + fangraphsHitting$GB + fangraphsHitting$FB + fangraphsHitting$BUH
    )
  ) * 100,2), 0)

# replace NA with 0
fangraphsHitting[is.na(fangraphsHitting)] <- 0

# Scrape Zips ROS projections current year
# FYI: There are duplicates in this list bc of the URL field (multiple positions)
rzips <- scrape_fangraphs(bat_pitch = 'bat',proj_system = 'rzips')
# replace NA with 0
rzips[is.na(rzips)] <- 0
# names(rzips)
colnames(rzips) <-
  c(
    "Name","V1","Team","G","PA","AB","H","x2B","x3B","HR","R","RBI","BB","SO","HBP","SB","CS","AVG","OBP","SLG","OPS","wOBA","Fld","BsR","WAR","url","V2"
  )

rzips <- sqldf(
  "select distinct ros.NAME as Name,
  round((ros.ab * -1) + (ros.x1B * 6) + (ros.x2B * 9) + (ros.x3B * 12) + (ros.HR * 16) + ((ros.BB + ros.HBP) * 3) + (ros.SB * 3) + (ros.CS * -4),0) as Points
  from (
  select rzips.*, H - (x2B + x3B + HR) as x1B
  from rzips) ros"
); rzips[is.na(rzips)] <- 0

# Scrape Batted Ball Distance/Velocity Data from Savant / MLB statcast
statcast<-statcast_leaderboard('bat', currentyear, last30date, 20)

# Calculate MLB AVGs
avg.dist <- round(mean(statcast$Avg_Distance),2)
avg.velo <- round(mean(statcast$Avg_Exit_Velocity),2)

# Merging datasets for analysis
xHit <- sqldf(
  "SELECT distinct a.NAME,Age,
  BABIP,LD_pct,GB_pct,K_pct,BB_pct,
  FB_pct,IFFB_pct,IFH_pct,
  Pull_pct,Cent_pct,Oppo_pct,
  Soft_pct,Med_pct,Hard_pct,
  Spd,ISO,HR_BB,
  HR_FB,wOBA,
  round(Avg_Exit_Velocity) as AvgExitVelocity,
  round(Avg_Distance) as AvgDistance,
  r.points
  FROM fangraphsHitting a
  left JOIN statcast b ON lower(b.NAME) = lower(a.NAME)
  left JOIN rzips r on lower(r.Name) = lower(a.NAME)
  "
  )

# replace NA with league avg
xHit$AvgDistance <-
  ifelse(is.na(xHit$AvgDistance),avg.dist,xHit$AvgDistance)
xHit$AvgExitVelocity <-
  ifelse(is.na(xHit$AvgExitVelocity),avg.velo,xHit$AvgExitVelocity)

############### xPected wOBA #######################
xOBA.fit <- lm(
  wOBA ~ BABIP + LD_pct + GB_pct + K_pct + BB_pct +
    FB_pct + Pull_pct + Hard_pct +
    Spd + AvgDistance + AvgExitVelocity, data = xHit
)
summary(xOBA.fit); unadj.rsquared(xOBA.fit)
xOBA_out <-
  cbind(
    xHit,'xOBA' = round(xOBA.fit$fitted.values,3), 'Gap' = round(xOBA.fit$residuals,3)
  )
xOBA_out <-
  subset(xOBA_out, select = c(Name,HR_BB,Hard_pct,AvgDistance,AvgExitVelocity,wOBA,xOBA,Gap))
xOBA_out <- xOBA_out[order(-xOBA_out$Gap),]
# plot(xOBA_out$wOBA ~ xOBA_out$xOBA, ylab="wOBA", xlab="xOBA", main="wOBA vs xOBA"); abline(0,1)

############### xPected BABIP #######################
xBABIP.fit <-
  lm(BABIP ~ LD_pct + FB_pct + IFH_pct + Hard_pct + Spd + Oppo_pct + AvgDistance + AvgExitVelocity, data = xHit)
summary(xBABIP.fit); unadj.rsquared(xBABIP.fit)
xBABIP_out <-
  cbind(
    xHit,'xBABIP' = round(xBABIP.fit$fitted.values,3), 'Gap' = round(xBABIP.fit$residuals,3)
  )
xBABIP_out <-
  subset(xBABIP_out, select = c(Name,HR_BB,Hard_pct,AvgDistance,AvgExitVelocity,BABIP,xBABIP,Gap))
xBABIP_out <- xBABIP_out[order(xBABIP_out$Gap),]
# plot(xBABIP_out$BABIP ~ xBABIP_out$xBABIP, ylab="BABIP", xlab="xBABIP", main="BABIP vs xBABIP"); abline(0,1)

############### xPected ISO #########################
xISO.fit <-
  lm(ISO ~ FB_pct + LD_pct + Hard_pct + Pull_pct + FB_pct + K_pct + AvgDistance + AvgExitVelocity, data =
       xHit)
summary(xISO.fit); unadj.rsquared(xISO.fit)
xISO_out <-
  cbind(
    xHit,'xISO' = round(xISO.fit$fitted.values,3), 'Gap' = round(xISO.fit$residuals,3)
  )
xISO_out <-
  subset(xISO_out, select = c(Name,HR_BB,Hard_pct,AvgDistance,AvgExitVelocity,ISO,xISO,Gap))
xISO_out <- xISO_out[order(xISO_out$Gap),]
# plot(xISO_out$ISO ~ xISO_out$xISO, ylab="ISO", xlab="xISO", main="ISO vs xISO"); abline(0,1)

############### xPected HR_BB#################
xHR.BB.fit <-
  lm(HR_BB ~ + FB_pct + Pull_pct + Hard_pct + AvgDistance + AvgExitVelocity, data = xHit)
summary(xHR.BB.fit); unadj.rsquared(xHR.BB.fit)
xHR_BB_output <-
  cbind(
    xHit,'xHR_BB' = round(xHR.BB.fit$fitted,2), 'Gap' = round(xHR.BB.fit$residuals,2)
  )
xHR_BB_output <-
  subset(xHR_BB_output, select = c(Name,Hard_pct,Pull_pct,AvgDistance,AvgExitVelocity,HR_BB,xHR_BB,Gap))
xHR_BB_output <- xHR_BB_output[order(xHR_BB_output$Gap),]
xHR_BB_output <- subset(xHR_BB_output, xHR_BB > 0)
# plot(xHR_BB_output$HR_BB ~ xHR_BB_output$xHR_BB, ylab="HR_BB", xlab="xHR_BB", main="HR_BB vs xHR_BB"); abline(0,1)

projectx <-
  sqldf(
    "select distinct h.Name, h.AvgDistance,h.AvgExitVelocity,
    h.FB_pct, h.LD_pct, h.Hard_pct, h.Pull_pct,
    h.BABIP, a.xBABIP, b.ISO, b.xISO,
    c.HR_BB,c.xHR_BB,o.wOBA, o.xOBA, 
    a.Gap as GapxBABIP, b.Gap as GAPxISO,c.Gap as GAPxHR_BB, o.Gap as GapxOBA,
    h.Points as ROS
    from xHit h
    join xOBA_out o on o.name = h.name
    join xBABIP_out a on a.name = h.name
    join xISO_out b on b.Name = h.Name
    join xHR_BB_output c on c.name = h.name
    order by o.Gap desc
    "
  )

#######################################################

# Aim for players with -Gap

# tail(projectx,15)
csv_name <- paste("xHitting",".csv", sep = '')
write.table(projectx,file = csv_name,sep = ",",row.names = F)

# END
