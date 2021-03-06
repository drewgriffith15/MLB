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
library(sqldf)

setwd("/Users/wgriffith2/Dropbox/R/MLB") #;getwd()

currentyear <-
  iif(as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
        1,as.numeric(format(Sys.time(), "%Y")))
last30date <-
  as.character(as.Date(format(Sys.time(), "%Y-%m-%d")) - 30)

# Scrape Fangraphs last 30 days; get last year if its before the season starts
fangraphsHitting <-
  griffun::fangraphs_leaderboard('bat',currentyear,currentyear,qual = 60, split = 3)

# Calculated HR/Batted Ball
fangraphsHitting$HR_BB <-
  iif(fangraphsHitting$HR > 0,round((
    fangraphsHitting$HR / (
      fangraphsHitting$LD + fangraphsHitting$GB + fangraphsHitting$FB + fangraphsHitting$BUH
    )
  ) * 100,2), 0)

# replace NA with 0
fangraphsHitting[is.na(fangraphsHitting)] <- 0

# Scrape Fangraphs FULL season
fangraphsHittingFULL <-
  griffun::fangraphs_leaderboard('bat',currentyear,currentyear,qual = 60, split = 0)

# Calculated HR/Batted Ball
fangraphsHittingFULL$HR_BB <-
  iif(fangraphsHittingFULL$HR > 0,round((
    fangraphsHittingFULL$HR / (
      fangraphsHittingFULL$LD + fangraphsHittingFULL$GB + fangraphsHittingFULL$FB + fangraphsHittingFULL$BUH
    )
  ) * 100,2), 0)

# replace NA with 0
fangraphsHittingFULL[is.na(fangraphsHittingFULL)] <- 0

# Scrape Batted Ball Distance/Velocity Data from Savant / MLB statcast
statcast <- statcast_leaderboard('bat', currentyear, last30date, 20)
statcastFULL <- statcast_leaderboard('bat', currentyear, NULL, 20)

# Calculate MLB AVGs to fill NAs
avg.dist <- round(mean(statcastFULL$AvgDistance),2)
avg.velo <- round(mean(statcastFULL$Avg_Exit_Velocity),2)

# Merging datasets for analysis
xHit <- sqldf(
  "SELECT distinct a.NAME,a.Age,
  a.BABIP,a.LD_pct,a.GB_pct,a.K_pct,a.BB_pct,
  a.FB_pct,a.IFFB_pct,a.IFH_pct,
  a.Pull_pct,a.Cent_pct,a.Oppo_pct,
  a.Soft_pct,a.Med_pct,a.Hard_pct,
  a.Spd,a.ISO,a.HR_BB,
  a.HR_FB,a.wOBA,
  round(b.Avg_Exit_Velocity) as AvgExitVelocity,
  round(b.Avg_Distance) as AvgDistance,
  a.Pull_pct - fa.Pull_pct as Pull_pct_diff,
  a.Hard_pct - fa.Hard_pct as Hard_pct_diff,
  b.Avg_Exit_Velocity - st.Avg_Exit_Velocity as Avg_Exit_Velocity_Diff,
  b.Avg_Distance - st.Avg_Distance as AvgDistance_Diff
  FROM fangraphsHitting a
  left join fangraphsHittingFULL fa on lower(fa.name) = lower(a.Name)
  left join statcast b ON lower(b.NAME) = lower(a.NAME)
  left join statcastFULL st on lower(st.Name) = lower(a.Name)
  "
)

# replace NA with league avg
xHit$AvgDistance <-
  ifelse(is.na(xHit$AvgDistance),avg.dist,xHit$AvgDistance)
xHit$AvgExitVelocity <-
  ifelse(is.na(xHit$AvgExitVelocity),avg.velo,xHit$AvgExitVelocity)

# replace NA with 0
xHit[is.na(xHit)] <- 0

############### xPected wOBA #######################
xOBA.fit <- lm(
  wOBA ~ BABIP + LD_pct + GB_pct + K_pct + BB_pct +
    FB_pct + Pull_pct + Hard_pct + Spd +
    AvgDistance + AvgExitVelocity +
    Pull_pct_diff + Hard_pct_diff + Avg_Exit_Velocity_Diff + AvgDistance_Diff, data = xHit
)
summary(xOBA.fit); unadj.rsquared(xOBA.fit)
xOBA_out <-
  cbind(
    xHit,'xOBA' = round(xOBA.fit$fitted.values,3), 'Gap' = round(xOBA.fit$residuals,3)
  )
xOBA_out <-
  subset(
    xOBA_out, select = c(
      Name,HR_BB,Hard_pct,AvgDistance,AvgExitVelocity,wOBA,xOBA,Gap
    )
  )
xOBA_out <- xOBA_out[order(-xOBA_out$Gap),]
# plot(xOBA_out$wOBA ~ xOBA_out$xOBA, ylab="wOBA", xlab="xOBA", main="wOBA vs xOBA"); abline(0,1)

############### xPected BABIP #######################
xBABIP.fit <-
  lm(
    BABIP ~ LD_pct + FB_pct + IFH_pct + Hard_pct + Spd + Oppo_pct + AvgDistance + AvgExitVelocity +
      Pull_pct_diff + Hard_pct_diff + Avg_Exit_Velocity_Diff + AvgDistance_Diff, data = xHit
  )
summary(xBABIP.fit); unadj.rsquared(xBABIP.fit)
xBABIP_out <-
  cbind(
    xHit,'xBABIP' = round(xBABIP.fit$fitted.values,3), 'Gap' = round(xBABIP.fit$residuals,3)
  )
xBABIP_out <-
  subset(
    xBABIP_out, select = c(
      Name,HR_BB,Hard_pct,AvgDistance,AvgExitVelocity,BABIP,xBABIP,Gap
    )
  )
xBABIP_out <- xBABIP_out[order(xBABIP_out$Gap),]
# plot(xBABIP_out$BABIP ~ xBABIP_out$xBABIP, ylab="BABIP", xlab="xBABIP", main="BABIP vs xBABIP"); abline(0,1)

############### xPected ISO #########################
xISO.fit <-
  lm(
    ISO ~ FB_pct + LD_pct + Hard_pct + Pull_pct + FB_pct + K_pct + AvgDistance + AvgExitVelocity +
      Pull_pct_diff + Hard_pct_diff + Avg_Exit_Velocity_Diff + AvgDistance_Diff, data = xHit
  )
summary(xISO.fit); unadj.rsquared(xISO.fit)
xISO_out <-
  cbind(
    xHit,'xISO' = round(xISO.fit$fitted.values,3), 'Gap' = round(xISO.fit$residuals,3)
  )
xISO_out <-
  subset(
    xISO_out, select = c(
      Name,HR_BB,Hard_pct,AvgDistance,AvgExitVelocity,ISO,xISO,Gap
    )
  )
xISO_out <- xISO_out[order(xISO_out$Gap),]
# plot(xISO_out$ISO ~ xISO_out$xISO, ylab="ISO", xlab="xISO", main="ISO vs xISO"); abline(0,1)

############### xPected HR_BB#################
xHR.BB.fit <-
  lm(
    HR_BB ~ +FB_pct + Pull_pct + Hard_pct + AvgDistance + AvgExitVelocity +
      Pull_pct_diff + Hard_pct_diff + Avg_Exit_Velocity_Diff + AvgDistance_Diff, data = xHit
  )
summary(xHR.BB.fit); unadj.rsquared(xHR.BB.fit)
xHR_BB_output <-
  cbind(
    xHit,'xHR_BB' = round(xHR.BB.fit$fitted,2), 'Gap' = round(xHR.BB.fit$residuals,2)
  )
xHR_BB_output <-
  subset(
    xHR_BB_output, select = c(
      Name,Hard_pct,Pull_pct,AvgDistance,AvgExitVelocity,HR_BB,xHR_BB,Gap
    )
  )
xHR_BB_output <- xHR_BB_output[order(xHR_BB_output$Gap),]
xHR_BB_output <- subset(xHR_BB_output, xHR_BB > 0)
# plot(xHR_BB_output$HR_BB ~ xHR_BB_output$xHR_BB, ylab="HR_BB", xlab="xHR_BB", main="HR_BB vs xHR_BB"); abline(0,1)

projectx <-
  sqldf(
    "select distinct h.Name, h.AvgDistance as AvgDistance,h.AvgExitVelocity,
    h.FB_pct, h.LD_pct, h.Hard_pct, h.Pull_pct,
    h.BABIP, a.xBABIP, b.ISO, b.xISO,
    c.HR_BB,c.xHR_BB,o.wOBA, o.xOBA,
    a.Gap as GapxBABIP, b.Gap as GAPxISO,c.Gap as GAPxHR_BB, o.Gap as GapxOBA
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
