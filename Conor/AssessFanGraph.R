library(Lahman)

#method for assessing WAR
RMSE <- function(true_wins, predicted_wins){
    sqrt(mean((true_wins - predicted_wins)^2))
}

ActualWins = subset(Teams, select=c(franchID,yearID, W, L ))

keeps = tail(Teams$franchID, n = 30)

#get Actual Wins
ActualWins = ActualWins[ActualWins$franchID %in% keeps, ]
ActualWins = ActualWins[ActualWins$yearID >= 1903, ]

#Read in predicted Fan Graph wins
FGWAR = read_csv("https://raw.githubusercontent.com/wangcathy/BST260Project/master/Conor/FanGraphWARs.csv")
FGWAR = as.matrix(subset(FGWAR, select=-c(X1)))

#form of dataframe is not the easiest to work with, changing that here
#better form for our dataframe
FanGraphWARs = df <- data.frame(franchID=character(), year=integer(), WAR=double(), stringsAsFactors=FALSE) 


for (team in colnames(FGWAR))
{
    zero_marker = 1
    
    for(year_idx in 1:115)
    {
        tmp = as.data.frame(x=list(franchID = team, year = year_idx + 1902, WAR = FGWAR[year_idx, team]))
        
        if (zero_marker == 1)
        {
            if (tmp$WAR == 0.0)
            {
                #do nothing, before team was created
            }
            else
            {
                zero_marker = 0 #we know the franchise has been created now
                FanGraphWARs = rbind(FanGraphWARs , tmp)
            }
        }
        else
        {
            FanGraphWARs = rbind(FanGraphWARs , tmp)
        }
    }
}

write.csv(FanGraphWARs, "FanGraphWARs_better_format.csv")


