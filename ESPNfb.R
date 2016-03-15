###############################################################################
###############################################################################

#' Returns a full list of MLB players eligibility 
#'  for hitters AND pitchers for ESPN Standard leagues#'
#'  Retreives primary position only
#' @param none
#' @examples 
#' players<-ESPN.eligibility()$All
#' head(players)
###############################################################################

ESPN.eligibility <- function() {
  # Get ESPN league Position Eligibility for Hitters
  
  library(sqldf)
  library(data.table)
  library(XML)
  library(stringr)
  
  base_url <-
    paste0("http://games.espn.go.com/flb/tools/eligibility?startIndex=")
  indx <-
    list(
      "0","50","100","150","200","250","300","350","400","450","500","550","600","650","700","750","800","850","900"
    )
  urls <-
    paste0(base_url, rep(indx, each = 1), "&slotCategoryGroup=1")
  
  # Scrape
  ESPN <-
    lapply(urls, function(x) {
      data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0[-1,]) # removing first row
    })
  
  # Combine Scrapes
  EligibilityH <- c()
  for (i in c(1:18)) {
    EligibilityH <-
      as.data.frame(rbind(EligibilityH,ESPN[[i]]))
  }
  
  # Rename columns
  colnames(EligibilityH) <-
    c(
      "NameTeamPos","Name","C","x1B","x2B","x3B","SS","LF","CF","RF","DH","SP","RP"
    )
  
  # Remove DTD/SSPD/DL
  EligibilityH$NameTeamPos <-
    str_trim(gsub("DTD", "", EligibilityH$NameTeamPos))
  EligibilityH$NameTeamPos <-
    str_trim(gsub("SSPD", "", EligibilityH$NameTeamPos))
  EligibilityH$NameTeamPos <-
    str_trim(gsub("DL60", "", EligibilityH$NameTeamPos))
  EligibilityH$NameTeamPos <-
    str_trim(gsub("DL15", "", EligibilityH$NameTeamPos))
  
  # Parse Name
  EligibilityH$Name <- gsub(",.*$", "", EligibilityH$NameTeamPos)
  EligibilityH$Name <-
    str_trim(gsub('[^.a-zA-Z0-9]',' ',EligibilityH$Name))
  
  EligibilityH <- sqldf(
    "select Name,
    case when C <> '--' then 'C'
    when x1B <> '--' then '1B'
    when x2B <> '--' then '2B'
    when x3B <> '--' then '3B'
    when SS <> '--' then 'SS'
    when LF <> '--' then 'OF'
    when CF <> '--' then 'OF'
    when RF <> '--' then 'OF'
    when DH <> '--' then 'DH'
    when SP <> '--' then 'SP'
    when RP <> '--' then 'RP'
    else '--' end as Position
    from EligibilityH
    "
    )
  
  # Get ESPN league Position Eligibility for Pitchers
  
  base_url <-
    paste0("http://games.espn.go.com/flb/tools/eligibility?startIndex=")
  indx <-
    list(
      "0","50","100","150","200","250","300","350","400","450","500","550","600","650","700","750","800","850","900"
    )
  urls <-
    paste0(base_url, rep(indx, each = 1), "&slotCategoryGroup=2")
  
  # Scrape
  ESPN <-
    lapply(urls, function(x) {
      data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0[-1,]) # removing first row
    })
  
  # Combine Scrapes
  EligibilityP <- c()
  for (i in c(1:18)) {
    EligibilityP <-
      as.data.frame(rbind(EligibilityP,ESPN[[i]]))
  }
  
  # Rename columns
  colnames(EligibilityP) <-
    c(
      "NameTeamPos","BLANK","C","x1B","x2B","x3B","SS","LF","CF","RF","DH","SP","RP"
    )
  
  # Remove DTD/SSPD/DL
  EligibilityP$NameTeamPos <-
    str_trim(gsub("DTD", "", EligibilityP$NameTeamPos))
  EligibilityP$NameTeamPos <-
    str_trim(gsub("SSPD", "", EligibilityP$NameTeamPos))
  EligibilityP$NameTeamPos <-
    str_trim(gsub("DL60", "", EligibilityP$NameTeamPos))
  EligibilityP$NameTeamPos <-
    str_trim(gsub("DL15", "", EligibilityP$NameTeamPos))
  
  # Parse Name
  EligibilityP$Name <- gsub(",.*$", "", EligibilityP$NameTeamPos)
  EligibilityP$Name <-
    str_trim(gsub('[^.a-zA-Z0-9]',' ',EligibilityP$Name))
  
  EligibilityP <- sqldf(
    "select Name,
    case when C <> '--' then 'C'
    when x1B <> '--' then '1B'
    when x2B <> '--' then '2B'
    when x3B <> '--' then '3B'
    when SS <> '--' then 'SS'
    when LF <> '--' then 'OF'
    when CF <> '--' then 'OF'
    when RF <> '--' then 'OF'
    when DH <> '--' then 'DH'
    when SP <> '--' then 'SP'
    when RP <> '--' then 'RP'
    else '--' end as Position
    from EligibilityP
    "
  )
  
  all <- rbind(EligibilityH,EligibilityP)
  out<-list(all,EligibilityH,EligibilityP)
  names(out)[1] = "All"
  names(out)[2] = "Hitters"
  names(out)[3] = "Pitchers"
  return(out)
}


###############################################################################
###############################################################################

#' Returns projections for hitters and pitchers for a WAR Games ESPN FB league (ONLY)
#' Column headings will be different for other leagues!!
#' @param leagueID ESPN league ID
#' @examples 
#' WARGames.Hitters<-ESPNProjections(86607)$Hitters
#' WARGames.Pitchers<-ESPNProjections(86607)$Pitchers
#' 
###############################################################################
# Get ESPN league point projections for HITTERS

ESPNProjections <- function(leagueID) {
  
  library(sqldf)
  library(data.table)
  library(XML)
  library(stringr)
  
  # Disable Sci Notation and supress warnings
  options(scipen = 999)
  options(warn = -1)

base_url <-
  paste0(
    "http://games.espn.go.com/flb/tools/projections?leagueId="
    ,leagueID,"&slotCategoryGroup=1&startIndex="
  )
indx <-
  list(
    "0","40","80","120","160","200","240","280","320","360","400","440","480","520","560","600","640"
  )   #seq(0, 900, 50)
urls <- paste0(base_url, rep(indx, each = 1))

# Scrape
ESPNproj <-
  lapply(urls, function(x) {
    data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0[-1,]) # removing first row
  })

# Combine Scrapes
ESPNprojections <- c()
for (i in c(1:15)) {
  ESPNprojections <-
    as.data.frame(rbind(ESPNprojections,ESPNproj[[i]]))
}

# Rename columns
colnames(ESPNprojections) <-
  c(
    "Number","NameTeamPos","FA","Name","AB","1B","2B","3B","HR","BB","HBP","SAC","SB","CS","PTS"
  )

# Remove DTD/SSPD/DL
ESPNprojections$NameTeamPos <-
  str_trim(gsub("DTD", "", ESPNprojections$NameTeamPos))
ESPNprojections$NameTeamPos <-
  str_trim(gsub("SSPD", "", ESPNprojections$NameTeamPos))
ESPNprojections$NameTeamPos <-
  str_trim(gsub("DL60", "", ESPNprojections$NameTeamPos))
ESPNprojections$NameTeamPos <-
  str_trim(gsub("DL15", "", ESPNprojections$NameTeamPos))

# Char to Number
for (i in c(1,5:ncol(ESPNprojections))) {
  ESPNprojections[,i] <-
    as.numeric(as.character(ESPNprojections[,i]))
}

# replace NA with 0
ESPNprojections[is.na(ESPNprojections)] <- 0

# Parse Name
ESPNprojections$Name <-
  gsub(",.*$", "", ESPNprojections$NameTeamPos)
ESPNprojections$Name <-
  str_trim(gsub('[^.a-zA-Z0-9]',' ',ESPNprojections$Name))

# Sort by Points desc
Hitters <- ESPNprojections[order(-ESPNprojections$PTS),]

############################################################################################

# Get ESPN league point projections for Pitchers
base_url <-
  paste0(
    "http://games.espn.go.com/flb/tools/projections?leagueId=",
    leagueID,"&slotCategoryGroup=2&startIndex="
  )
indx <-
  list(
    "0","40","80","120","160","200","240","280","320","360","400","440","480","520","560","600","640"
  )   #seq(0, 640, 40)
urls <- paste0(base_url, rep(indx, each = 1))

# Scrape
ESPNproj <-
  lapply(urls, function(x) {
    data.table(readHTMLTable(x, as.data.frame = TRUE, stringsAsFactors = FALSE)$playertable_0[-1,]) # removing first row
  })

# Combine Scrapes
ESPNprojections <- c()
for (i in c(1:15)) {
  ESPNprojections <-
    as.data.frame(rbind(ESPNprojections,ESPNproj[[i]]))
}

# Rename columns
colnames(ESPNprojections) <-
  c(
    "Number","NameTeamPos","FA","Name","GS","IP","HR","BB","HB","K","SV","HD","PTS"
  )

# Remove DTD/SSPD/DL
ESPNprojections$NameTeamPos <-
  str_trim(gsub("DTD", "", ESPNprojections$NameTeamPos))
ESPNprojections$NameTeamPos <-
  str_trim(gsub("SSPD", "", ESPNprojections$NameTeamPos))
ESPNprojections$NameTeamPos <-
  str_trim(gsub("DL60", "", ESPNprojections$NameTeamPos))
ESPNprojections$NameTeamPos <-
  str_trim(gsub("DL15", "", ESPNprojections$NameTeamPos))

# Char to Number
for (i in c(1,5:ncol(ESPNprojections))) {
  ESPNprojections[,i] <-
    as.numeric(as.character(ESPNprojections[,i]))
}

# replace NA with 0
ESPNprojections[is.na(ESPNprojections)] <- 0

# Parse Name
ESPNprojections$Name <-
  gsub(",.*$", "", ESPNprojections$NameTeamPos)
ESPNprojections$Name <-
  str_trim(gsub('[^.a-zA-Z0-9]',' ',ESPNprojections$Name))

# Sort by Points desc
Pitchers <- ESPNprojections[order(-ESPNprojections$PTS),]

out <- list(Hitters, Pitchers)
names(out)[1] = "Hitters"
names(out)[2] = "Pitchers"
return(out)
}
