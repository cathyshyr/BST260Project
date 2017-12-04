setwd("/Users/Conor/Documents/BST\ 260/Project/BST260Project/Conor")

year = 2016
team_num=9

teams = seq(1, 30, 1)

sapply(teams, get_WAR_from_team_year, year=2016)

FanGraphWar = matrix(nrow=30, ncol= 115)
colnames(FanGraphWar) = as.character(seq(1903:2017))

start.time <- Sys.time()
for (team in 1:30)
{
    for(year in 1:115)
    {
        reportedTeamWAR = get_WAR_from_team_year(team, year+1902)
        FanGraphWar[team, year] = reportedTeamWAR
        print(reportedTeamWAR)
    }
    
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


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
