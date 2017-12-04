h = readLines('https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2017&month=0&season1=2017&ind=0&team=9&rost=0&age=0')

g = readLines('https://www.baseball-reference.com/teams/NYY/2017.shtml')

k = readLines('http://legacy.baseballprospectus.com/team_audit.php?team=MIA')



year = 2016
team_num=9


get_WAR_from_team_year(9, 2015)


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
}


prep_jc_url=function (team, year, batting)
{
     url = paste0('https://www.fangraphs.com/leaders.aspx?pos=all&stats=', ifelse(batting==1, "bat", "pit"),'&lg=all&qual=0&type=8&season=', year, '&month=0&season1=', year, '&ind=0&team=', team_num, '&rost=0&age=0')
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
    
    return(list(WAR=WAR, name=name))
}

split_string_on_gele = function(string)
{
    matrix(strsplit(string, "[><]"))[[1]]
}
