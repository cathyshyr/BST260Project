library(XML)
library(RCurl)
library(rlist)
library(Lahman)
library(tidyverse)
library(openxlsx)
teams <- unique(Teams$franchID)
years <- 1871:2017

urls <- matrix(0, length(teams), length(years))
for(i in 1:length(teams)) {
  for(j in 1:length(years)) {
    urls[i, j] <- paste0("https://www.baseball-reference.com/teams/", teams[i], "/", years[j], ".shtml")
  }
}
url_vector <- as.vector(urls)

list_of_batting <- list()
list_of_pitching <- list()
for(i in 1:5000) {
  url <- url_vector[i]
  
  res <- try(readLines(url), silent = TRUE)
  
  ## check if website exists
  if(inherits(res, "try-error")) {
    list_of_batting[[i]] <- NA
    list_of_pitching[[i]] <- NA
  }
  else {
    urltxt <- readLines(url)
    urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
    doc <- htmlParse(urltxt)
    tables_full <- readHTMLTable(doc)
    tmp1 <- tables_full$players_value_batting
    tmp2 <- tables_full$players_value_pitching
    list_of_batting[[i]] <- tmp1
    list_of_pitching[[i]] <- tmp2
  }
  print(i)
  closeAllConnections()
}

for(i in 5001:10000) {
  url <- url_vector[i]
  
  res <- try(readLines(url), silent = TRUE)
  
  ## check if website exists
  if(inherits(res, "try-error")) {
    list_of_batting[[i]] <- NA
    list_of_pitching[[i]] <- NA
  }
  else {
    urltxt <- readLines(url)
    urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
    doc <- htmlParse(urltxt)
    tables_full <- readHTMLTable(doc)
    tmp1 <- tables_full$players_value_batting
    tmp2 <- tables_full$players_value_pitching
    list_of_batting[[i]] <- tmp1
    list_of_pitching[[i]] <- tmp2
  }
  print(i)
  closeAllConnections()
}

for(i in 10001:17640) {
  url <- url_vector[i]
  
  res <- try(readLines(url), silent = TRUE)
  
  ## check if website exists
  if(inherits(res, "try-error")) {
    list_of_batting[[i]] <- NA
    list_of_pitching[[i]] <- NA
  }
  else {
    urltxt <- readLines(url)
    urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
    doc <- htmlParse(urltxt)
    tables_full <- readHTMLTable(doc)
    tmp1 <- tables_full$players_value_batting
    tmp2 <- tables_full$players_value_pitching
    list_of_batting[[i]] <- tmp1
    list_of_pitching[[i]] <- tmp2
  }
  print(i)
  closeAllConnections()
}

## find indices where the link exists
ind_batting <- which(!is.na(list_of_batting))
ind_pitching <- which(!is.na(list_of_pitching))

## find links that exist
url_batting <- url_vector[ind_batting]
url_pitching <- url_vector[ind_pitching]

## extract year from each url
years_batting <- as.numeric(unlist(regmatches(url_batting, gregexpr("[[:digit:]]+", url_batting))))
years_pitching <- as.numeric(unlist(regmatches(url_pitching, gregexpr("[[:digit:]]+", url_pitching))))

## extract team from each url
teams_batting <- basename(dirname(url_batting))
teams_pitching <- basename(dirname(url_pitching))

## remove NAs from lists
na.omit.list <- function(y) { 
  return(y[!sapply(y, function(x) all(is.na(x)))]) 
}

test_batting <- na.omit.list(list_of_batting)
test_pitching <- na.omit.list(list_of_pitching)

## add columns for year and team
test_batting <- mapply(cbind, test_batting, "Year" = years_batting, 
                       "Team" = teams_batting, SIMPLIFY = F)
test_pitching <- mapply(cbind, test_pitching, "Year" = years_pitching,
                        "Team" = teams_pitching, SIMPLIFY = F)

bbref_batting <- bind_rows(test_batting)
bbref_pitching <- bind_rows(test_pitching)



df_bbref_batting <- read.xlsx("BBRef_Batting.xlsx", na.strings = c("--", ""))
df_bbref_batting <- df_bbref_batting %>%
  mutate(Age = as.numeric(Age),
         WAR = as.numeric(WAR),
         oWAR = as.numeric(oWAR),
         dWAR = as.numeric(dWAR),
         Salary = as.numeric(gsub('\\$|,', '', Salary))) %>%
  select(Team, Year, WAR)

df_bbref_pitching <- read.xlsx("BBRef_Pitching.xlsx", na.strings = c("--", ""))
df_bbref_pitching <- df_bbref_pitching %>%
  mutate(Age = as.numeric(Age),
         WAR = as.numeric(WAR),
         Salary = as.numeric(gsub('\\$|,', '', Salary))) %>%
  select(Team, Year, WAR)

bbref <- rbind(df_bbref_batting, df_bbref_pitching)

bbref_WAR <- bbref %>%
  group_by(Team, Year) %>%
  summarize(WAR = sum(WAR, na.rm = TRUE)) %>%
  rename(franchID = Team, yearID = Year)
  
merged_bbref <- Teams %>%
  mutate(franchID = as.character(franchID)) %>%
  select(franchID, yearID, W) %>%
  left_join(bbref_WAR) %>%
  mutate(source = "Baseball Reference")


write.csv(merged_bbref, "BBReference.csv")


##### RERUNNING CODE TO RETRIEVE DATA FOR MISSING TEAMS #####
teams <- "CAL"
years <- 1961:1996
list_of_batting_CAL <- list()
list_of_pitching_CAL <- list()

urls <- rep(0, length(years))
for(j in 1:length(years)) {
    urls[j] <- paste0("https://www.baseball-reference.com/teams/", teams, "/", years[j], ".shtml")
}

for(i in 1:length(urls)) {
  url <- urls[i]
  
  res <- try(readLines(url), silent = TRUE)
  
  ## check if website exists
  if(inherits(res, "try-error")) {
    list_of_batting_CAL[[i]] <- NA
    list_of_pitching_CAL[[i]] <- NA
  }
  else {
    urltxt <- readLines(url)
    urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
    doc <- htmlParse(urltxt)
    tables_full <- readHTMLTable(doc)
    tmp1 <- tables_full$players_value_batting
    tmp2 <- tables_full$players_value_pitching
    list_of_batting_CAL[[i]] <- tmp1
    list_of_pitching_CAL[[i]] <- tmp2
  }
  print(i)
  closeAllConnections()
}

ind_batting <- which(!is.na(list_of_batting_CAL))
ind_pitching <- which(!is.na(list_of_batting_CAL))

url_batting <- urls[ind_batting]
url_pitching <- urls[ind_pitching]

years_batting <- as.numeric(unlist(regmatches(url_batting, gregexpr("[[:digit:]]+", url_batting))))
years_pitching <- as.numeric(unlist(regmatches(url_pitching, gregexpr("[[:digit:]]+", url_pitching))))

teams_batting <- basename(dirname(url_batting))
teams_pitching <- basename(dirname(url_pitching))

test_batting <- na.omit.list(list_of_batting_CAL)
test_pitching <- na.omit.list(list_of_pitching_CAL)

test_batting <- mapply(cbind, test_batting, "Year" = years_batting, 
                       "Team" = teams_batting, SIMPLIFY = F)
test_pitching <- mapply(cbind, test_pitching, "Year" = years_pitching,
                        "Team" = teams_pitching, SIMPLIFY = F)

bbref_batting_CAL <- bind_rows(test_batting)
bbref_pitching_CAL <- bind_rows(test_pitching)


##### RERUNNING CODE TO RETRIEVE DATA FOR MISSING TEAMS #####
teams <- "LAA"
years <- 2005:2017
list_of_batting_LAA <- list()
list_of_pitching_LAA <- list()

urls <- rep(0, length(years))
for(j in 1:length(years)) {
  urls[j] <- paste0("https://www.baseball-reference.com/teams/", teams, "/", years[j], ".shtml")
}

for(i in 1:length(urls)) {
  url <- urls[i]
  
  res <- try(readLines(url), silent = TRUE)
  
  ## check if website exists
  if(inherits(res, "try-error")) {
    list_of_batting_LAA[[i]] <- NA
    list_of_pitching_LAA[[i]] <- NA
  }
  else {
    urltxt <- readLines(url)
    urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
    doc <- htmlParse(urltxt)
    tables_full <- readHTMLTable(doc)
    tmp1 <- tables_full$players_value_batting
    tmp2 <- tables_full$players_value_pitching
    list_of_batting_LAA[[i]] <- tmp1
    list_of_pitching_LAA[[i]] <- tmp2
  }
  print(i)
  closeAllConnections()
}

ind_batting <- which(!is.na(list_of_batting_LAA))
ind_pitching <- which(!is.na(list_of_batting_LAA))

url_batting <- urls[ind_batting]
url_pitching <- urls[ind_pitching]

years_batting <- as.numeric(unlist(regmatches(url_batting, gregexpr("[[:digit:]]+", url_batting))))
years_pitching <- as.numeric(unlist(regmatches(url_pitching, gregexpr("[[:digit:]]+", url_pitching))))

teams_batting <- basename(dirname(url_batting))
teams_pitching <- basename(dirname(url_pitching))

test_batting <- na.omit.list(list_of_batting_LAA)
test_pitching <- na.omit.list(list_of_pitching_LAA)

test_batting <- mapply(cbind, test_batting, "Year" = years_batting, 
                       "Team" = teams_batting, SIMPLIFY = F)
test_pitching <- mapply(cbind, test_pitching, "Year" = years_pitching,
                        "Team" = teams_pitching, SIMPLIFY = F)

bbref_batting_LAA <- bind_rows(test_batting)
bbref_pitching_LAA <- bind_rows(test_pitching)


##### RERUNNING CODE TO RETRIEVE DATA FOR MISSING TEAMS #####
teams <- "TBR"
years <- 2008:2017
list_of_batting_TBR <- list()
list_of_pitching_TBR <- list()

urls <- rep(0, length(years))
for(j in 1:length(years)) {
  urls[j] <- paste0("https://www.baseball-reference.com/teams/", teams, "/", years[j], ".shtml")
}

for(i in 1:length(urls)) {
  url <- urls[i]
  
  res <- try(readLines(url), silent = TRUE)
  
  ## check if website exists
  if(inherits(res, "try-error")) {
    list_of_batting_TBR[[i]] <- NA
    list_of_pitching_TBR[[i]] <- NA
  }
  else {
    urltxt <- readLines(url)
    urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
    doc <- htmlParse(urltxt)
    tables_full <- readHTMLTable(doc)
    tmp1 <- tables_full$players_value_batting
    tmp2 <- tables_full$players_value_pitching
    list_of_batting_TBR[[i]] <- tmp1
    list_of_pitching_TBR[[i]] <- tmp2
  }
  print(i)
  closeAllConnections()
}

ind_batting <- which(!is.na(list_of_batting_TBR))
ind_pitching <- which(!is.na(list_of_batting_TBR))

url_batting <- urls[ind_batting]
url_pitching <- urls[ind_pitching]

years_batting <- as.numeric(unlist(regmatches(url_batting, gregexpr("[[:digit:]]+", url_batting))))
years_pitching <- as.numeric(unlist(regmatches(url_pitching, gregexpr("[[:digit:]]+", url_pitching))))

teams_batting <- basename(dirname(url_batting))
teams_pitching <- basename(dirname(url_pitching))

na.omit.list <- function(y) { 
  return(y[!sapply(y, function(x) all(is.na(x)))]) 
}

test_batting <- na.omit.list(list_of_batting_TBR)
test_pitching <- na.omit.list(list_of_pitching_TBR)

test_batting <- mapply(cbind, test_batting, "Year" = years_batting, 
                       "Team" = teams_batting, SIMPLIFY = F)
test_pitching <- mapply(cbind, test_pitching, "Year" = years_pitching,
                        "Team" = teams_pitching, SIMPLIFY = F)

bbref_batting_TBR <- bind_rows(test_batting)
bbref_pitching_TBR <- bind_rows(test_pitching)


##### RERUNNING CODE TO RETRIEVE DATA FOR MISSING TEAMS #####
teams <- "MIA"
years <- 2013:2017
list_of_batting_MIA <- list()
list_of_pitching_MIA <- list()

urls <- rep(0, length(years))
for(j in 1:length(years)) {
  urls[j] <- paste0("https://www.baseball-reference.com/teams/", teams, "/", years[j], ".shtml")
}

for(i in 1:length(urls)) {
  url <- urls[i]
  
  res <- try(readLines(url), silent = TRUE)
  
  ## check if website exists
  if(inherits(res, "try-error")) {
    list_of_batting_MIA[[i]] <- NA
    list_of_pitching_MIA[[i]] <- NA
  }
  else {
    urltxt <- readLines(url)
    urltxt <- gsub("-->", "", gsub("<!--", "", urltxt))
    doc <- htmlParse(urltxt)
    tables_full <- readHTMLTable(doc)
    tmp1 <- tables_full$players_value_batting
    tmp2 <- tables_full$players_value_pitching
    list_of_batting_MIA[[i]] <- tmp1
    list_of_pitching_MIA[[i]] <- tmp2
  }
  print(i)
  closeAllConnections()
}

ind_batting <- which(!is.na(list_of_batting_MIA))
ind_pitching <- which(!is.na(list_of_batting_MIA))

url_batting <- urls[ind_batting]
url_pitching <- urls[ind_pitching]

years_batting <- as.numeric(unlist(regmatches(url_batting, gregexpr("[[:digit:]]+", url_batting))))
years_pitching <- as.numeric(unlist(regmatches(url_pitching, gregexpr("[[:digit:]]+", url_pitching))))

teams_batting <- basename(dirname(url_batting))
teams_pitching <- basename(dirname(url_pitching))

test_batting <- na.omit.list(list_of_batting_MIA)
test_pitching <- na.omit.list(list_of_pitching_MIA)

test_batting <- mapply(cbind, test_batting, "Year" = years_batting, 
                       "Team" = teams_batting, SIMPLIFY = F)
test_pitching <- mapply(cbind, test_pitching, "Year" = years_pitching,
                        "Team" = teams_pitching, SIMPLIFY = F)

bbref_batting_MIA <- bind_rows(test_batting)
bbref_pitching_MIA <- bind_rows(test_pitching)

bbref_batting_new <- bind_rows(bbref_batting_CAL,
                               bbref_pitching_CAL,
                               bbref_batting_LAA,
                               bbref_pitching_LAA,
                               bbref_batting_MIA,
                               bbref_pitching_MIA,
                               bbref_batting_TBR,
                               bbref_pitching_TBR)

write.xlsx(bbref_batting_new, "new_teams.xlsx")


df_bbref_batting <- read.xlsx("BBRef_Batting.xlsx", na.strings = c("--", ""))
df_bbref_batting <- df_bbref_batting %>%
  mutate(Age = as.numeric(Age),
         WAR = as.numeric(WAR),
         oWAR = as.numeric(oWAR),
         dWAR = as.numeric(dWAR),
         Salary = as.numeric(gsub('\\$|,', '', Salary))) %>%
  select(Team, Year, WAR)

df_bbref_pitching <- read.xlsx("BBRef_Pitching.xlsx", na.strings = c("--", ""))
df_bbref_pitching <- df_bbref_pitching %>%
  mutate(Age = as.numeric(Age),
         WAR = as.numeric(WAR),
         Salary = as.numeric(gsub('\\$|,', '', Salary))) %>%
  select(Team, Year, WAR)

bbref <- rbind(df_bbref_batting, df_bbref_pitching)

df_bbref_new <- read.xlsx("new_teams.xlsx", na.strings = c("--", ""))
df_bbref_new <- df_bbref_new %>%
  mutate(Age = as.numeric(Age),
         WAR = as.numeric(WAR),
         oWAR = as.numeric(oWAR),
         dWAR = as.numeric(dWAR),
         Salary = as.numeric(gsub('\\$|,', '', Salary))) %>%
  select(Team, Year, WAR)

df_bbref_final <- rbind(bbref, df_bbref_new) %>%
  arrange(Year)

bbref_WAR <- df_bbref_final %>%
  group_by(Team, Year) %>%
  summarize(WAR = sum(WAR, na.rm = TRUE)) %>%
  rename(franchID = Team, yearID = Year)

merged_bbref <- Teams %>%
  mutate(franchID = as.character(franchID)) %>%
  select(franchID, yearID, W) %>%
  full_join(bbref_WAR) %>%
  mutate(source = "Baseball Reference") %>%
  arrange(yearID, franchID)


write.csv(merged_bbref, "BBReference.csv")
