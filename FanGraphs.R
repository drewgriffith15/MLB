###############################################################################
###############################################################################

#' Scrape Batter Leaderboards from FanGraphs.com
#'
#' This function allows you to scrape all leaderboard statistics from FanGraphs.com.
#' @param yearfrom First season for which you want data.
#' @param yearto Last season for which you want data. If multiple years selected, data returned will be aggregate data for the date range. If yearto = yearfrom, function will return single-season data.
#' @param qual Whether you want only batters that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimumm number of plate appearaces, use the number desired.
#' @param duration either set to 0 - full season or 3 - 30 days. Multiple years must be set to 0!
#' @examples
#' fgh<-FanGraphs.batting(2013,2015, 200, 0)
#'
###############################################################################

FanGraphs.batting <- function(yearfrom, yearto, qual=NULL, duration=NULL) {
  
  library(data.table)
  library(XML)
  library(stringr)
  
  qual<-ifelse(is.null(qual),"y",qual)
  duration<-ifelse(is.null(duration),0,duration)
  yearfrom <-ifelse(is.null(yearfrom),as.numeric(format(Sys.time(), "%Y")),yearfrom)
  yearto <-ifelse(is.null(yearfrom),as.numeric(format(Sys.time(), "%Y")),yearto)
  
  base_url <-
    paste0(
      "http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=",
      qual,"&type=c,4,5,6,8,9,10,11,14,15,16,17,18,19,20,21,22,23,34,35,40,41,43,44,45,46,47,60,107,50,58,3&season=",
      yearto, "&month=",
      duration,"&season1=",
      yearfrom, "&ind=0&team="
    )
  teams <-
    list(
      "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
    )
  urls <- paste0(base_url, rep(teams, each = 1),
                 "&rost=1&age=0&filter=&players=0&sort=32,d")
  # Scrape
  fangraphsB <-
    lapply(urls, function(x) {
      data.table(
        readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
      )
    })
  
  # Combine Scrapes
  fangraphsBatting <- c()
  for (i in 1:30) {
    fangraphsBatting <-
      as.data.frame(rbind(fangraphsBatting,fangraphsB[[i]]))
  }
  
  # Rename columns
  c <- as.matrix(names(fangraphsBatting))
  c <- gsub("%", "_pct", c, fixed = TRUE)
  c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
  c <- gsub("/", "_", c, fixed = TRUE)
  c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
  names(fangraphsBatting) <- c
  
  # Remove percentages
  for (i in c(1,4:ncol(fangraphsBatting))) {
    fangraphsBatting[,i] <-
      str_trim(str_replace_all(fangraphsBatting[,i],"%",""))
  }
  
  # Char to Number
  for (i in c(1,4:ncol(fangraphsBatting))) {
    fangraphsBatting[,i] <-
      as.numeric(as.character(fangraphsBatting[,i]))
  }
  
  # replace NA with 0
  fangraphsBatting[is.na(fangraphsBatting)] <- 0
  
  return(fangraphsBatting)
  
}

###############################################################################
#' Scrape Pitcher Leaderboards from FanGraphs.com
#'
#' This function allows you to scrape all leaderboard statistics from FanGraphs.com.
#' @param yearfrom First season for which you want data.
#' @param yearto Last season for which you want data. If multiple years selected, data returned will be aggregate data for the date range. If yearto = yearfrom, function will return single-season data.
#' @param qual Whether you want only Pitchers that qualified in a given season, or the minimum number of plate appearances for inclusion. If you only want qualified hitters, use qual. If a minimumm number of plate appearaces, use the number desired.
#' @param duration either set to 0 - full season or 3 - 30 days. Multiple years must be set to 0!
#' @examples
#' fgp<-FanGraphs.pitching(2013,2015, 50, 0)
#'
###############################################################################

FanGraphs.pitching <- function(yearfrom, yearto, qual=NULL, duration=NULL) {
  
  library(data.table)
  library(XML)
  library(stringr)
  
  qual<-ifelse(is.null(qual),"y",qual)
  duration<-ifelse(is.null(duration),0,duration)
  yearfrom <-ifelse(is.null(yearfrom),as.numeric(format(Sys.time(), "%Y")),yearfrom)
  yearto <-ifelse(is.null(yearfrom),as.numeric(format(Sys.time(), "%Y")),yearto)
  
  base_url <-
    paste0(
      "http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=",
      qual,"&type=c,4,5,7,8,11,12,13,15,16,17,18,19,20,21,114,24,36,37,113,112,111,76,77,81,211,43,50,44,47,48,49,51,6,45,122,3,59&season=",
      yearto, "&month=",
      duration,"&season1=",
      yearfrom, "&ind=0&team="
    )
  teams <-
    list(
      "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
    )
  urls <- paste0(base_url, rep(teams, each = 1),
                 "&rost=1&age=0&filter=&players=0&sort=39,d")
  
  # Scrape
  fangraphsP <-
    lapply(urls, function(x) {
      data.table(
        readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$LeaderBoard1_dg1_ctl00
      )
    })
  
  # Combine Scrapes
  fangraphsPitching <- c()
  for (i in 1:30) {
    fangraphsPitching <-
      as.data.frame(rbind(fangraphsPitching,fangraphsP[[i]]))
  }
  
  # Rename columns
  c <- as.matrix(names(fangraphsPitching))
  c <- gsub("%", "_pct", c, fixed = TRUE)
  c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
  c <- gsub("/", "_", c, fixed = TRUE)
  c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
  names(fangraphsPitching) <- c
  
  # Remove percentages
  for (i in c(1,4:ncol(fangraphsPitching))) {
    fangraphsPitching[,i] <-
      str_trim(str_replace_all(fangraphsPitching[,i],"%",""))
  }
  
  # Char to Number
  for (i in c(1,4:ncol(fangraphsPitching))) {
    fangraphsPitching[,i] <-
      as.numeric(as.character(fangraphsPitching[,i]))
  }
  
  # replace NA with 0
  fangraphsPitching[is.na(fangraphsPitching)] <- 0
  
  return(fangraphsPitching)
  
}

###############################################################################

#' Scrape Steamer Batting (600) from FanGraphs.com
#'
#' This function allows you to scrape all Steamer600 batting projections from FanGraphs.com.
#' @examples
#' s600<-SteamerBatting600()
#'
###############################################################################

SteamerBatting600 <- function() {
  
  library(data.table)
  library(XML)
  library(stringr)

  base_url <-
    "http://www.fangraphs.com/projections.aspx?pos=all&stats=bat&type=steamer600&team="
  teams <-
    list(
      "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
    )
  urls <- paste0(base_url, rep(teams, each = 1),
                 "&players=0&sort=26,d")
  # Scrape
  SteamerB600 <-
    lapply(urls, function(x) {
      data.table(
        readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$ProjectionBoard1_dg1_ctl00
      )
    })
  
  # Combine Scrapes
  SteamerBatting600 <- c()
  for (i in 1:30) {
    SteamerBatting600 <-
      as.data.frame(rbind(SteamerBatting600,SteamerB600[[i]]))
  }
  
  # Rename columns
  c <- as.matrix(names(SteamerBatting600))
  c <- gsub("%", "_pct", c, fixed = TRUE)
  c <- gsub("+", "_plus", c, fixed = TRUE)
  c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
  c <- gsub("/", "_", c, fixed = TRUE)
  c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
  names(SteamerBatting600) <- c
  
  # Remove percentages
  for (i in 3:ncol(SteamerBatting600)) {
    SteamerBatting600[,i] <-
      str_trim(str_replace_all(SteamerBatting600[,i],"%",""))
  }
  
  # Char to Number
  for (i in 3:ncol(SteamerBatting600)) {
    SteamerBatting600[,i] <-
      as.numeric(as.character(SteamerBatting600[,i]))
  }
  
  # replace NA with 0
  SteamerBatting600[is.na(SteamerBatting600)] <- 0
  
  return(SteamerBatting600)
  
}

###############################################################################

#' Scrape Steamer Batting (600) from FanGraphs.com
#'
#' This function allows you to scrape all Steamer600 pitching projections from FanGraphs.com.
#' @examples
#' sp600<-SteamerPitching600()
#'
###############################################################################

SteamerPitching600 <- function() {
  
  library(data.table)
  library(XML)
  library(stringr)
  
  # Streamer 600 Projections on Fangraphs
  base_url <-
    "http://www.fangraphs.com/projections.aspx?pos=all&stats=pit&type=steamer600&team="
  teams <-
    list(
      "1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30"
    )
  urls <- paste0(base_url, rep(teams, each = 1),
                 "&players=0&sort=19,d")
  # Scrape
  SteamerP600 <-
    lapply(urls, function(x) {
      data.table(
        readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$ProjectionBoard1_dg1_ctl00
      )
    })
  
  # Combine Scrapes
  SteamerPitching600 <- c()
  for (i in 1:30) {
    SteamerPitching600 <-
      as.data.frame(rbind(SteamerPitching600,SteamerP600[[i]]))
  }
  
  # Rename columns
  c <- as.matrix(names(SteamerPitching600))
  c <- gsub("%", "_pct", c, fixed = TRUE)
  c <- gsub("+", "_plus", c, fixed = TRUE)
  c <- gsub("-", "_", c, fixed = TRUE)
  c <- gsub(" (pfx)", "_pfx", c, fixed = TRUE)
  c <- gsub("/", "_", c, fixed = TRUE)
  c <- ifelse(substr(c, nchar(c)-1+1, nchar(c)) == ".", gsub("\\.", "_pct", c), c)
  names(SteamerPitching600) <- c
  
  # Remove percentages
  for (i in 3:ncol(SteamerPitching600)) {
    SteamerPitching600[,i] <-
      str_trim(str_replace_all(SteamerPitching600[,i],"%",""))
  }
  
  # Char to Number
  for (i in 3:ncol(SteamerPitching600)) {
    SteamerPitching600[,i] <-
      as.numeric(as.character(SteamerPitching600[,i]))
  }
  
  # replace NA with 0
  SteamerPitching600[is.na(SteamerPitching600)] <- 0
  
  return(SteamerPitching600)
  
}