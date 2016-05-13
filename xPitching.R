# Analysis based on following article on Fangraphs:
# http://www.fangraphs.com/blogs/bettermatch-up-data-forecasting-strikeout-rate/
# http://www.fangraphs.com/fantasy/potential-starting-pitcher-strikeout-rate-surgers/
# http://www.fangraphs.com/fantasy/last-14-day-al-starting-pitcher-velocity-decliners/
# http://www.fangraphs.com/fantasy/the-bestest-pitcher-expected-bb-formula-yet/
# http://www.fangraphs.com/fantasy/xk-and-potential-pitcher-strikeout-rate-decliners/


# Load libraries from github
library(devtools)
devtools::install_github('drewgriffith15/griffun')
library(griffun)
library(sqldf)
library(data.table)
library(XML)
library(stringr)

setwd("/Users/wgriffith2/Dropbox/R/MLB") #;getwd()

currentyear <-
  iif(as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
        1,as.numeric(format(Sys.time(), "%Y")))

last30date <-
  as.character(as.Date(format(Sys.time(), "%Y-%m-%d")) - 30)

# Scrape Fangraphs current year; get last year
## START Full season Collection
# fangraphsPitchingFull <- fangraphs_leaderboard('pit',currentyear,currentyear, q, 0)

# For some reason I get an "Error in `[.data.frame`(fangraphs_leaders, , i) : undefined columns selected" when I ref the function, so I have to use the raw code here
bat_pitch = 'pit'; yearfrom = currentyear; yearto = currentyear; qual = 10; split = 0

options(warn = -1)

qual <- ifelse(is.null(qual),"y",qual)
split <- ifelse(is.null(split),0,split)
yearfrom <- ifelse(is.null(yearfrom),
                   ifelse(
                     as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
                       1,as.numeric(format(Sys.time(), "%Y"))
                   )
                   ,yearfrom)
yearto <- ifelse(is.null(yearto),
                 ifelse(
                   as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
                     1,as.numeric(format(Sys.time(), "%Y"))
                 )
                 ,yearto)

if (bat_pitch == 'bat') {
  end_params <- '&players=0&sort=4,d'
  type <-
    'c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211'
} else if (bat_pitch == 'pit') {
  end_params <- '&players=0&sort=9,d'
  type <-
    'c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224'
}

base_url <-
  paste0(
    "http://www.fangraphs.com/leaders.aspx?pos=all&stats=",
    bat_pitch,"&lg=all&qual=",
    qual,"&type=",
    type,"&season=",
    yearto, "&month=",
    split,"&season1=",
    yearfrom, "&ind=0&team="
  )
teams <- unlist(strsplit(as.character(1:30),','))
urls <- paste0(base_url, rep(teams, each = 1),
               "&rost=1&age=0&filter=",end_params)
# Scrape
leaders <-
  lapply(urls, function(x) {
    data.table(
      readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
    )
  })

# Combine Scrapes
fangraphsPitchingFull <- c()
for (i in 1:30) {
  fangraphsPitchingFull <-
    as.data.frame(rbind(fangraphsPitchingFull,leaders[[i]]))
}

# Rename columns
c <- as.matrix(names(fangraphsPitchingFull))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
c <- gsub("+", "plus", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("1b", "x1b", c, fixed = TRUE)
c <- gsub("2b", "x2b", c, fixed = TRUE)
c <- gsub("3b", "x3b", c, fixed = TRUE)
c <-
  ifelse(substr(c, nchar(c) - 1 + 1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
names(fangraphsPitchingFull) <- c

# Remove percentages
for (i in c(1,4:ncol(fangraphsPitchingFull))) {
  fangraphsPitchingFull[,i] <-
    str_trim(str_replace_all(fangraphsPitchingFull[,i],"%",""))
}

# Char to Number
for (i in c(1,4:ncol(fangraphsPitchingFull))) {
  fangraphsPitchingFull[,i] <-
    as.numeric(as.character(fangraphsPitchingFull[,i]))
}

# replace NA with 0
fangraphsPitchingFull[is.na(fangraphsPitchingFull)] <- 0


# Calculated HR/Batted Ball
fangraphsPitchingFull$HR_BB <-
  iif(fangraphsPitchingFull$HR > 0,round((
    fangraphsPitchingFull$HR / (
      fangraphsPitchingFull$LD + fangraphsPitchingFull$GB + fangraphsPitchingFull$FB +
        fangraphsPitchingFull$BUH
    )
  ) * 100,2), 0)

# Calculated Avg of FIP and xFIP
fangraphsPitchingFull$aFIP <-
  round(rowMeans(subset(
    fangraphsPitchingFull, select = c(FIP, xFIP)
  ), na.rm = TRUE),2)

## END Full Season Collection

## START 30 Day Collection
bat_pitch = 'pit'; yearfrom = currentyear; yearto = currentyear; qual = 10; split = 3

qual <- ifelse(is.null(qual),"y",qual)
split <- ifelse(is.null(split),0,split)
yearfrom <- ifelse(is.null(yearfrom),
                   ifelse(
                     as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
                       1,as.numeric(format(Sys.time(), "%Y"))
                   )
                   ,yearfrom)
yearto <- ifelse(is.null(yearto),
                 ifelse(
                   as.numeric(format(Sys.time(), "%m%d")) <= 331,as.numeric(format(Sys.time(), "%Y")) -
                     1,as.numeric(format(Sys.time(), "%Y"))
                 )
                 ,yearto)

if (bat_pitch == 'bat') {
  end_params <- '&players=0&sort=4,d'
  type <-
    'c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211'
} else if (bat_pitch == 'pit') {
  end_params <- '&players=0&sort=9,d'
  type <-
    'c,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203,204,205,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,221,222,223,224'
}

base_url <-
  paste0(
    "http://www.fangraphs.com/leaders.aspx?pos=all&stats=",
    bat_pitch,"&lg=all&qual=",
    qual,"&type=",
    type,"&season=",
    yearto, "&month=",
    split,"&season1=",
    yearfrom, "&ind=0&team="
  )
teams <- unlist(strsplit(as.character(1:30),','))
urls <- paste0(base_url, rep(teams, each = 1),
               "&rost=1&age=0&filter=",end_params)
# Scrape
leaders <-
  lapply(urls, function(x) {
    data.table(
      readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
    )
  })

# Combine Scrapes
fangraphsPitching30d <- c()
for (i in 1:30) {
  fangraphsPitching30d <-
    as.data.frame(rbind(fangraphsPitching30d,leaders[[i]]))
}

# Rename columns
c <- as.matrix(names(fangraphsPitching30d))
c <- gsub("%", "_pct", c, fixed = TRUE)
c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
c <- gsub("+", "plus", c, fixed = TRUE)
c <- gsub("/", "_", c, fixed = TRUE)
c <- gsub("-", "_", c, fixed = TRUE)
c <- gsub("1b", "x1b", c, fixed = TRUE)
c <- gsub("2b", "x2b", c, fixed = TRUE)
c <- gsub("3b", "x3b", c, fixed = TRUE)
c <-
  ifelse(substr(c, nchar(c) - 1 + 1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
names(fangraphsPitching30d) <- c

# Remove percentages
for (i in c(1,4:ncol(fangraphsPitching30d))) {
  fangraphsPitching30d[,i] <-
    str_trim(str_replace_all(fangraphsPitching30d[,i],"%",""))
}

# Char to Number
for (i in c(1,4:ncol(fangraphsPitching30d))) {
  fangraphsPitching30d[,i] <-
    as.numeric(as.character(fangraphsPitching30d[,i]))
}

# replace NA with 0
fangraphsPitching30d[is.na(fangraphsPitching30d)] <- 0

# Calculated HR/Batted Ball
fangraphsPitching30d$HR_BB <-
  iif(fangraphsPitching30d$HR > 0,round((
    fangraphsPitching30d$HR / (
      fangraphsPitching30d$LD + fangraphsPitching30d$GB + fangraphsPitching30d$FB +
        fangraphsPitching30d$BUH
    )
  ) * 100,2), 0)

# Calculated Avg of FIP and xFIP
fangraphsPitching30d$aFIP <-
  round(rowMeans(subset(fangraphsPitching30d, select = c(FIP, xFIP)), na.rm = TRUE),2)

## Baseball reference scrape last 30 days
bb_ref30d <- bbref_leaderboard('pit',n=30)

# Scrape Batted Ball Distance/Velocity Data from Savant / MLB statcast
statcast<-statcast_leaderboard('pit', currentyear, last30date, 5)

## END 30 Day Collection

# Calculate MLB AVGs
avg.dist <- round(mean(statcast$Avg_Distance),2)
avg.velo <- round(mean(statcast$Avg_Exit_Velocity),2)

xPitch = sqldf(
  "
  SELECT d30.name,
  case when d30.gs > 1 then 'SP' else 'RP' end as Position,
  d30.ip,
  d30.sv,
  d30.hld,
  d30.fbv,
  d30.BABIP,
  d30.LOB_pct,
  d30.k_pct,
  d30.bb_pct,
  d30.swstr_pct,
  d30.o_swing_pct,
  d30.z_swing_pct,
  d30.swing_pct,
  d30.o_contact_pct,
  d30.z_contact_pct,
  d30.fb_pct,
  d30.zone_pct,
  d30.hard_pct,
  d30.pull_pct,
  round(d30.HR_BB, 2) AS HR_BB,
  d30.era,
  d30.fip,
  d30.xfip,
  d30.afip,
  d30.siera,
  d30.kwera,
  d30.fbv - f.fbv AS FBvDiff,
  d30.k_pct - f.k_pct AS K_Diff,
  d30.bb_pct - f.bb_pct AS BB_Diff,
  d30.swstr_pct - f.swstr_pct AS SwStr_Diff,
  d30.o_swing_pct - f.o_swing_pct AS O_Swing_Diff,
  d30.z_swing_pct - f.z_swing_pct AS Z_Swing_Diff,
  d30.swing_pct - f.swing_pct AS Swing_Diff,
  d30.o_contact_pct - f.o_contact_pct AS O_Contact_Diff,
  d30.z_contact_pct - f.z_contact_pct AS Z_Contact_Diff,
  d30.fb_pct - f.fb_pct AS FB_Diff,
  d30.zone_pct - f.zone_pct AS Zone_Diff,
  d30.hard_pct - f.hard_pct AS Hard_Diff,
  d30.pull_pct - f.pull_pct AS Pull_Diff,
  d30.f_strike_pct,
  bb_ref30d.stl,
  bb_ref30d.sts,
  bb_ref30d.str,
  statcast.Avg_Distance as AvgDistance, statcast.Avg_Exit_Velocity as AvgExitVelocity
  FROM fangraphsPitching30d d30
  JOIN fangraphsPitchingFull f ON lower(f.name) = lower(d30.name)
  JOIN bb_ref30d ON lower(bb_ref30d.name) = lower(d30.name)
  left JOIN statcast on lower(statcast.Name) = lower(d30.name)
  WHERE (d30.HLD > 2 or d30.SV > 2 or d30.GS > 1)
  "
)

# replace NA with league avg
xPitch$AvgDistance <-
  ifelse(is.na(xPitch$AvgDistance),avg.dist,xPitch$AvgDistance)
xPitch$AvgExitVelocity <-
  ifelse(is.na(xPitch$AvgExitVelocity),avg.velo,xPitch$AvgExitVelocity)

#run seperate models
df.sp <- subset(xPitch, Position == 'SP')
df.rp <- subset(xPitch, Position == 'RP')

############### xPected K% ########################

# model variables
K_pct.lm <-
  "K_pct~IP+FBv+SwStr_pct+O_Swing_pct+Z_Swing_pct+Swing_pct+O_Contact_pct+Z_Contact_pct+FB_pct+Zone_pct+FBvDiff+SwStr_Diff+O_Swing_Diff+Z_Swing_Diff+Swing_Diff+Zone_Diff+F_Strike_pct+StL+StS+Str"

# SP fit
xK_pct.sp.fit = lm(K_pct.lm, data = df.sp)
summary(xK_pct.sp.fit); unadj.rsquared(xK_pct.sp.fit)
df.sp = cbind(
  df.sp,'xK_pct' = round(xK_pct.sp.fit$fitted.values,2), 'GapxK_pct' = round(xK_pct.sp.fit$residuals,2)
)

# RP fit
xK_pct.rp.fit = lm(K_pct.lm, data = df.rp)
summary(xK_pct.rp.fit); unadj.rsquared(xK_pct.rp.fit)
df.rp = cbind(
  df.rp,'xK_pct' = round(xK_pct.rp.fit$fitted.values,2), 'GapxK_pct' = round(xK_pct.rp.fit$residuals,2)
)

############### xPected HR_BB% ########################

# model variables
xHR_BB_pct.lm <-
  "HR_BB~FBv+Z_Contact_pct+FB_pct+Hard_pct+Pull_pct+Z_Contact_Diff+Hard_Diff+Pull_Diff+AvgDistance+AvgExitVelocity"

# SP fit
xHR_BB_pct.sp.fit = lm(xHR_BB_pct.lm, data = df.sp)
summary(xHR_BB_pct.sp.fit); unadj.rsquared(xHR_BB_pct.sp.fit)
df.sp = cbind(
  df.sp,'xHR_BB_pct' = round(xHR_BB_pct.sp.fit$fitted.values,2), 'GapxHR_BB_pct' = round(xHR_BB_pct.sp.fit$residuals,2)
)

# RP fit
xHR_BB_pct.rp.fit = lm(xHR_BB_pct.lm, data = df.rp)
summary(xHR_BB_pct.rp.fit); unadj.rsquared(xHR_BB_pct.rp.fit)
df.rp = cbind(
  df.rp,'xHR_BB_pct' = round(xHR_BB_pct.rp.fit$fitted.values,2), 'GapxHR_BB_pct' = round(xHR_BB_pct.rp.fit$residuals,2)
)

############### xPected aFIP #######################

# model variables
xaFIP.lm <-
  "aFIP~IP+SwStr_pct+O_Contact_pct+Z_Contact_pct+Hard_pct+Pull_pct+FBvDiff+FBvDiff+BB_Diff+SwStr_Diff+F_Strike_pct+AvgDistance+AvgExitVelocity+LOB_pct"

# SP fit
xaFIP.sp.fit = lm(xaFIP.lm, data = df.sp)
summary(xaFIP.sp.fit); unadj.rsquared(xaFIP.sp.fit)
df.sp = cbind(
  df.sp,'xaFIP' = round(xaFIP.sp.fit$fitted.values,2), 'GapxaFIP' = round(xaFIP.sp.fit$residuals,2)
)

# RP fit
xaFIP.rp.fit = lm(xaFIP.lm, data = df.rp)
summary(xaFIP.rp.fit); unadj.rsquared(xaFIP.rp.fit)
df.rp = cbind(
  df.rp,'xaFIP' = round(xaFIP.rp.fit$fitted.values,2), 'GapxaFIP' = round(xaFIP.rp.fit$residuals,2)
)

# Combine all results
xPitch <- c()
xPitch <- rbind(df.sp,df.rp)

xPitching <-
  subset(
    xPitch, xaFIP < 7, select = c(
      Name,Position,IP,SV,HLD,FBv,FBvDiff,K_pct,xK_pct,GapxK_pct,HR_BB,xHR_BB_pct, GapxHR_BB_pct, SwStr_pct,SIERA,ERA,aFIP,xaFIP,GapxaFIP,Pull_pct,Hard_pct,AvgDistance,AvgExitVelocity
    )
  )
xPitching = xPitching[order(xPitching$GapxaFIP),]

xPitching$xHR_BB_pct <- iif(xPitching$xHR_BB_pct < 0, 0,xPitching$xHR_BB_pct ) # fix overfitting

#######################################################

# Aim for players with -Gap on xK% and +Gap for xFIP AND +FBvDiff
# tail(xPitching,25)
csv_name <- paste("xPitching",".csv", sep = '')
write.table(xPitching,file = csv_name,sep = ",",row.names = F)

# END