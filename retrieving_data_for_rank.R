##----------------------------------------
## Step 0: Load Libraries
##----------------------------------------
library(XML)        ## Extract urls
library(httr)       ## Extract urls
library(tidyverse)  ## String and data frame Manipulation




##----------------------------------------
## Part 1 Load Rank Data
##----------------------------------------
## Years where players are active
years = c(1985:1993,1995:2019) 

## URL
totals_sites = paste0("https://www.basketball-reference.com/leagues/NBA_",years,"_totals.html")


##----------------------------------------
## Name: removeDuplicatePlayers 
## Purpose: Remove Duplicate Players in Data Frame
## Input: players df with duplicates
## Output:players df without duplicates
##----------------------------------------

removeDuplicatePlayers = function(df){
  
  
  
  ## This is the new DF that will be used to remove duplicate players
  new_df = df
  
  
  ## DF of those that played for multiple teams
  multiple_teams_df = data.frame(table(df$Player)) %>%
    filter(Freq>1) %>%
    arrange(Freq)
  
  ## DF of players that only show up once
  oneteam_df =  df %>%
    filter(!Player %in% as.character(multiple_teams_df$Var1))
  
  
  new_oneteam_df = vector()
  for(i in 1:nrow(multiple_teams_df) ){
    
    
    ## Choose team in which player played the most time
    one_table_df = new_df %>%
      group_by(Player) %>%
      filter(Player == as.character(multiple_teams_df$Var1[i]),
             MP == max(MP))
    
    
    new_oneteam_df = bind_rows(new_oneteam_df,one_table_df)
    
    
  }
  
  ## Remove any NAs
  new_oneteam_df = new_oneteam_df %>% filter(!is.na(Rk))
  
  ## Combine DFs to have players show up once
  all_players = bind_rows(oneteam_df,new_oneteam_df)
  return(all_players)
}


##----------------------------------------
## Name: changeStatistic 
## Purpose: Change Statistic to proper Name
## Input: Abbreviated statistics
## Output: Fullname statistic
##----------------------------------------
changeStatistic = function (x){
  if(x == "FG"){
    y = "Field Goals Per Game"
  } else if( x == "G"){
    y = "Number Games Played"
  } else if( x == "eFG_percent"){
    y = "Effective Field Goal Percent Per Game"
  } else if( x == "FTA"){
    y = "Free Throws Attempt"
  } else if( x == "FG_percent"){
    y = "Field Goal Percent Per Game"
  } else if( x == "FT_percent"){
    y = "Free Throw Percent Per Game"
  } else if( x == "GS"){
    y = "Number of Games Started"
  } else if( x == "FGA"){
    y = "Field Goal Attempts Per Game"
  } else if( x == "FGA_percent"){
    y = "Field Goal Perent Per Game"
  } else if ( x =="X3P"){
    y = "3-Point Field Goals Per Game"
  } else if( x =="X3PA"){
    y = "3-Point Field Goal Attempts Per Game"
  } else if( x =="X3P_percent"){
    y = "3-Point Field Goal Percent Per Game"
  } else if(x =="FT"){
    y = "Free Throws Per Game"
  }else if(x =="ORB"){
    y = "Offensive Rebounds Per Game"
  }else if(x =="DRB"){
    y = "Defensive Rebounds Per Game"
  }else if(x =="TRB"){
    y = "Total Rebounds Per Game"
  }else if(x =="AST"){
    y = "Assists Per Game"
  }else if(x =="STL"){
    y = "Steals Per Game"
  }else if(x =="BLK"){
    y = "Blocks Per Game"
  }else if(x =="TOV"){
    y = " Turnovers Per Game"
  }else if(x =="PF"){
    y = "Personal Fouls Per Game"
  }else if(x =="PTS"){
    y = "Points Per Game"
  } else if(x == 'Point_Margin'){
    y = 'Point Margin'
  } else if (x== 'MP'){
    y = 'Minutes Played'
  } else if(x =='X2P'){
    y = '2-Point Field Goals Per Game'
  } else if(x == 'X2P_percent'){
    y = '2-Point Percent Per Game'
  } else if(x == 'X2PA'){
    y = '2-Point Attempts Per Game'
  } else {
    y=x
  }
  return(y)
}














## Important Playrs
imp_players = c('Michael Jordan', 'LeBron James', 'Kobe Bryant',
                'Michael Jordan*', 'LeBron James*', 'Kobe Bryant*')

imp_player_info = vector()
for(j in 1:length(totals_sites)){
  
  
  ## Website scrapping
  url = totals_sites[j]
  tabs <- GET(url)
  tabs <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)
  
  
  ## Get Players DF for jth year
  players_df = tabs$totals_stats %>%
    filter(Rk != 'Rk')
  
  ## Obtain only one team per player based on most minutes played
  all_players_df = removeDuplicatePlayers(players_df)
  
  ## Remove Unnecessary variables to turn char to numeric variable
  info_df = all_players_df %>%
    select(-Pos, -Tm, -Rk,-Player) %>%
    mutate_if(is.character,as.numeric) 
  
 ## Combine Info 
  mod_all_players_df = all_players_df %>%
    select(Player) %>%
    bind_cols(info_df)
  
  
  ## Find Important Player(s) Rank against peers 
  mod_all_players_df = mod_all_players_df %>%
    select(Player) %>%
    bind_cols(  mutate_all(mod_all_players_df[,-1], funs("ntile"= 101 - ntile(.,100)))) %>%
    arrange(PTS_ntile) %>%
  select(Player,contains('ntile')) %>%
    mutate(PF_ntile = 101 - PF_ntile,
           TOV_ntile = 101 - TOV_ntile) %>%
  filter(Player %in% imp_players) %>%
  data.frame(year = years[j])
  
  

  imp_player_info = bind_rows(imp_player_info,mod_all_players_df)
  
  cat('Year ',years[j],' is done! \n')
  
}



## Modify Dataframe
mod_imp_player_info = imp_player_info %>%
  rename(Year = year) %>%
  arrange(Year,Player) %>%
  filter(is.na(Player) != T) %>% 
  group_by(Player) %>% 
  mutate(Season = paste0('season_',row_number())) %>%
  select(Player,Year, Season, everything())

colnames(mod_imp_player_info) = str_replace_all(colnames(mod_imp_player_info),'\\.','_percent')



## Manipulate Data Frame 
mod_imp_player_info = mod_imp_player_info %>%
  ungroup(Player) %>%
  mutate(Player = case_when(
    Player == 'Michael Jordan*' ~ 'MJ',
    Player == 'LeBron James' ~ 'LJ',
    Player == 'Kobe Bryant' ~ 'KB')) %>%
  gather(Statistic,Ranking,Age_ntile:PTS_ntile,Year) %>%
  spread(Player, Ranking) %>%
  mutate(Statistic = str_remove_all(Statistic,'_ntile')) %>%
  mutate(ss = as.numeric(str_remove(Season,'season_'))) %>%
  arrange(Statistic,ss)


## Make new vector for proper statistic name
mod_imp_player_info$full_Statistic = apply(as.matrix(mod_imp_player_info$Statistic),1,change_statistic)

## LB and KB seasons more than 15
mod_imp_player_info = mod_imp_player_info %>%
  filter(!Season %in%paste0('season_',16:20))



setwd("C:/Users/james/OneDrive/Documents/Important_Files/Stat_ed_2018_papers/paper_0_bball_data/0_basketball_data")
write.csv(mod_imp_player_info,'rank_by_year.csv',row.names = F)
