setwd("/Users/Conor/Documents/BST\ 260/Project/BST260Project/Conor")


get_WAR_from_team_year=function(team, year)
{   
    
    #Batters first
    prep_jc_url(team, year, 1)
    system("phantomjs scrap_final.js")
    myHtml <- readLines("1.html")
    myHtml_sub = myHtml[ grep('LeaderBoard1_dg1_ctl00', myHtml)[1]: grep('LeaderBoard1_dg1_SharedCalendarContainer', myHtml)]
    myHtml_sub = myHtml_sub [ grep('playerid=', myHtml_sub) ]
    tmp_o=lapply(myHtml_sub, get_WAR_from_PIDrow, batting=1)
    
    #then pitchers
    prep_jc_url(team, year, 0)
    system("phantomjs scrap_final.js")
    myHtml <- readLines("1.html")
    myHtml_sub = myHtml[ grep('LeaderBoard1_dg1_ctl00', myHtml)[1]: grep('LeaderBoard1_dg1_SharedCalendarContainer', myHtml)]
    myHtml_sub = myHtml_sub [ grep('playerid=', myHtml_sub) ]
    tmp_p=lapply(myHtml_sub, get_WAR_from_PIDrow, batting=0)
    
    if(length(tmp_p) > 0)
    {
    
        tmp_o_mat = t(matrix(unlist(tmp_o), nrow  =2))
        tmp_p_mat = t(matrix(unlist(tmp_p), nrow  =2))
        
        
        #could return list, but why not just sum now and return sum? 
        df_war = data.frame(rbind(tmp_o_mat, tmp_p_mat))
        #return(df_war)
        ret = sum(as.numeric(as.character(df_war[,1])))
        return(ret)
    }
    else{return(0)}
}


prep_jc_url=function (team, year, batting)
{
     url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=', ifelse(batting==1, "bat", "pit"),'&lg=all&qual=0&type=8&season=', year, '&month=0&season1=', year, '&ind=0&team=', team, '&rost=0&age=0')
     lines <- readLines("scrap_final.js")
     lines[1] <- paste0("var url ='", url ,"';")
     writeLines(lines, "scrap_final.js")
}



get_WAR_from_PIDrow = function(string, batting)
{
    #WAR
    tmp = gregexpr('<td class="grid_line_regular" align="right">', string)
    start = tmp[[1]][(length(tmp[[1]]))]
    tmp_string = substr(string, start=start, stop = start+100)
    WAR = as.numeric(split_string_on_gele(tmp_string)[ifelse(batting==1, 3, 7)])
    
    #Name
    tmp = gregexpr('playerid', string)
    start = tmp[[1]][(length(tmp[[1]]))]
    tmp_string = substr(string, start=start, stop = start+100)
    name = split_string_on_gele(tmp_string)[2]
    
    return(c(WAR, name))
}

split_string_on_gele = function(string)
{
    matrix(strsplit(string, "[><]"))[[1]]
}



FanGraphWar = matrix(nrow=30, ncol= 115)


start.time <- Sys.time()
for (team in 30:30)
{
    for(year in 1:115)
    {
        reportedTeamWAR = get_WAR_from_team_year(team, year+1902)
        FanGraphWar[team, year] = reportedTeamWAR
        print(paste(reportedTeamWAR, team, year))
    }
    
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


FGW = as.data.frame(t(FanGraphWar))



FGW$ANA = FGW$V1 
FGW$BAL = FGW$V2 
FGW$BOS = FGW$V3 
FGW$CHW = FGW$V4 
FGW$CLE = FGW$V5 
FGW$DET = FGW$V6
FGW$KCR = FGW$V7 
FGW$MIN = FGW$V8 
FGW$NYY = FGW$V9 
FGW$OAK = FGW$V10 
FGW$SEA = FGW$V11 
FGW$TBD = FGW$V12 
FGW$TEX = FGW$V13 
FGW$TOR = FGW$V14 
FGW$ARI = FGW$V15 
FGW$ATL = FGW$V16 
FGW$CHC = FGW$V17 
FGW$CIN = FGW$V18 
FGW$COL = FGW$V19 
FGW$FLA = FGW$V20 
FGW$HOU = FGW$V21 
FGW$LAD = FGW$V22 
FGW$MIL = FGW$V23
FGW$WSN = FGW$V24 
FGW$NYM = FGW$V25 
FGW$PHI = FGW$V26
FGW$PIT = FGW$V27 
FGW$STL = FGW$V28 
FGW$SDP = FGW$V29 
FGW$SFG = FGW$V30

library(Lahman)
keeps = tail(Teams$franchID, n = 30)
FGW = FGW[, (names(FGW) %in% keeps)]
