##----------------------------------------------------
## Part 0: Load Packages
##----------------------------------------------------
library(shiny)               ## To use Shiny
library(plyr)                ## Data Frame Manipulation
library(tidyverse)           ## Data Frame Manipulation
library(kableExtra)          ## Make tables look at great
library(plotly)              ## Interactive Graphics
library(readr)               ## Read in CSVs file
library(htmlTable)           ## Display HTML Tables
library(vistime)             ## Display Timeline
library(readxl)              ## Read in xlsx files



##----------------------------------------------------
## Part 1: Gather Data
##----------------------------------------------------

setwd("C:/Users/james/OneDrive/Documents/Important_Files/GATO365/Stat_ed_2018_papers/paper_1_bball_data/1_actual_PFS_app/1_app_and_data")
path <- "nba_data.xlsx"

nba_data = path %>% 
  excel_sheets() %>% 
  purrr::set_names() %>% 
  map(read_excel, path = path)

nba_df = nba_data$modern_nba_legends_08302019     ## Game data
heatmap_df = nba_data$heat_map_df                 ## Heatmap Data
radar_df = nba_data$rank_by_year                  ## Radar Plot Data
player_info = nba_data$nba_player_info            ## Table for Player Info
timeline_df = nba_data$timeline_plot_nba_players  ## Read in Timeline Information
defintions_df = nba_data$basketball_definitions   ## Basketball Definitions



##----------------------------------------------------
## Part 2: Important Functions
##----------------------------------------------------
##------------------------------------
## Name: create_rank_df
## Purpose: To create a rank and hover matrices 
## Input: 1) data frame of players data; 2) does the variables have strings
## Output: 1) matrix for rank; 2) matrix for hovering
##------------------------------------
create_rank_df = function(df,categor_var){
  
  ## Make NA 0
  df[is.na(df)] = 0
  ## Get names for new matrix
  names_players = as.character(df$Name)
  
  ## Make data matrix
  df = df %>%
    select(-Name) %>%
    as.matrix()
  ## Reverse Order
  data_nba = 4-apply(df,2,rank,ties.method = "max")
  rownames(data_nba) = names_players
  
  
  
  
  
  ## Gather information in correct format for Heatmap
  stats_name = rep(colnames(data_nba),3)
  p_name = rep(rownames(data_nba),ncol(data_nba))
  stats_name = matrix(paste0('Stat: ',stats_name,sep=''),3,ncol(data_nba),byrow = T)
  p_name = matrix(paste0('Player: ',p_name,sep=''),3,ncol(data_nba))
  r_name = matrix(paste0('Rank: ',data_nba,sep=''),3,ncol(data_nba))
  
  
  if (categor_var == T){
    ## Categorical Data
    stat_amount_categ = df[,c(3,4)]
    stat_info_categ = matrix(paste0('Total: ',stat_amount_categ,sep=''),3,2)
    
    ## Numerical Data
    stat_amount_quant = round(df[,c(1,2)],2)
    stat_info_quant = matrix(paste0('Average: ',stat_amount_quant,sep=''),3,2)
    stat_info = cbind(stat_info_quant, stat_info_categ)
    
  } else{
    
    ## Numerical Data
    stat_amount_quant = round(df,2)
    stat_info_quant = matrix(paste0('Average: ',stat_amount_quant,sep=''),3,ncol(df))
    stat_info = stat_info_quant
  }
  
  
  
  
  hover_labels = matrix(paste0(stats_name,' \n ',p_name,' \n ',r_name,' \n ',
                               stat_info),3,ncol(data_nba))
  rownames(data_nba) = apply(as.matrix(rownames(data_nba)),1,fullname_to_initials)
  
  
  
  
  return(list(data_nba = data_nba, hover_labels = hover_labels))
}


##------------------------------------
## Name: abvstat_to_statname
## Purpose: To be able to change abbreviated statistic into statistics name
## Input: string abbreviated statistic
## Output: string statistic name
##------------------------------------
abvstat_to_statname = function (x){
  if(x == "FG"){
    y = "Field Goals"
  } else if( x == "FGA"){
    y = "Field Goal Attempts Per Game"
  } else if ( x =="X3P"){
    y = "3-Point Field Goals Per Game"
  } else if( x =="X3PA"){
    y = "3-Point Field Goal Attempts Per Game"
  }else if(x =="FT"){
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
  } else if(x == 'DD'){
    y = 'Double-Double'
  } else if(x == 'TD'){
    y = 'Triple-Double'
  }
  return(y)
}

##------------------------------------
## Name: fullname_to_initials
## Purpose: To be able to change full name into initials
## Input: string full name
## Output:string initials
##------------------------------------
fullname_to_initials = function(x){
  if(x == "KB"){
    y = "Bryant"
  } else if(x == "LJ"){
    y = "James" 
  } else if(x == "MJ"){
    y = "Jordan"
  } 
  return(y)
}


##------------------------------------
## Name: convert_to_stat
## Purpose: Convert user input into the appropriate statistic
## Input: numeric value
## Output: statistic
##------------------------------------
convert_to_stat = function(x){
  if(x == 1){
    imp_stat = "FG"
  } else if (x == 2){
    imp_stat = "X3P"
  } else if (x == 3){
    imp_stat = "FT"
  } else if (x == 4){
    imp_stat = "ORB"
  } else if (x == 5){
    imp_stat = "DRB"
  } else if (x == 6){
    imp_stat = "TRB"
  } else if (x == 7){
    imp_stat = "AST"
  } else if (x == 8){
    imp_stat = "STL"
  } else if (x == 9){
    imp_stat = "BLK"
  } else if (x == 10){
    imp_stat = "TOV"
  } else if (x == 11){
    imp_stat = "PF"
  } else if (x == 12){
    imp_stat = "PTS"
  }
  return(imp_stat)
  
}


##----------------------------------------------------
## Part 3: User Input
##----------------------------------------------------
ui <- fluidPage(
  
  # Application title
  titlePanel("Plots for Shots"),
  
  # Sidebar with a slider input for number of bins 
  tabsetPanel(
    
    ##--------------------------------
    ## Panel 1: Information for the Graphics Panel
    ##--------------------------------
    tabPanel(title = 'Graphics',
             sidebarLayout(
               sidebarPanel(
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 ##***********************************
                 ## Step 1: Selection of Graph Type
                 ##***********************************
                 selectInput("grapht", h3("Select Visualization"),
                             choices = list( "Jitter Plot"= 1,
                                             "Histogram" = 3,
                                             "Box Plot" = 4,
                                             "Radar Plot" = 5,
                                             "Heatmap" = 6
                                             ), selected = 1),
                 
                 conditionalPanel(
                   ##***********************************
                   ## Step 2: Selection of Players (For Line Plot, Histogram, and Boxplot)
                   ##***********************************
                   checkboxGroupInput("players",
                                      h3("Select Player(s)"),
                                      choices = list("Kobe Bryant" = 1,
                                                     "LeBron James" = 2,
                                                     "Michael Jordan" = 3),
                                      selected = c(2,3)),
                   condition = "input.grapht != 6 & input.grapht != 5 ",
                   ##***********************************
                   ## Step 3: Selection of Season (For Line Plot, Histogram, and Boxplot)
                   ##***********************************
                   sliderInput("season", h3("Select Seasons"),
                               min = 1, max = 15, value = c(1, 1))
                   
                   
                   
                 ),
                 
                 conditionalPanel(
                   ##***********************************
                   ## Step 4: Selection of Variable (For Line Plot, Histogram, and Boxplot)
                   ##***********************************
                   condition = "input.grapht == 1 | input.grapht == 3 | input.grapht == 4",
                   
                   
                   selectInput("bp_hist_line_Var", "Choose Statistic:",
                               choices = list("Field Goals Per Game" = 1,
                                              "Three-Point Field Goals Per Game"=2,
                                              "Points Per Game"=12,
                                              "Offensive Rebounds Per Game" = 4,
                                              "Assists Per Game" = 7,
                                              "Turnovers Per Game"=10,
                                              "Defensive Rebounds Per Game"=5,
                                              "Steals Per Game"=8,
                                              "Blocks Per Game" = 9
                               ), selected = 12)
                   
                   
                 ),
                 
                 
                 ##***********************************
                 ## Step 2: Selection of Season (For Radar Plot and Heatmap)
                 ##***********************************
                 conditionalPanel(
                   
                   condition = "input.grapht == 6 | input.grapht == 5 ",
                   ## Selection of Season
                   selectInput("season_hm", h3("Select Seasons"),
                               choices = list( "Season 1"= 1,
                                               "Season 2"= 2,
                                               "Season 3"= 3,
                                               "Season 4"= 4,
                                               "Season 5"= 5,
                                               "Season 6"= 6,
                                               "Season 7"= 7,
                                               "Season 8"= 8,
                                               "Season 9"= 9,
                                               "Season 10"= 10,
                                               "Season 11"= 11,
                                               "Season 12"= 12,
                                               "Season 13"= 13,
                                               "Season 14"= 14,
                                               "Season 15"= 15), selected = 1)
                   
                   
                 ),
                 
                 
                 ##***********************************
                 ## Step 3: Selection of Variables (For Radar Plot)
                 ##***********************************
                 conditionalPanel(
                   
                   condition = "input.grapht == 5",
                   
                   
                   ## Select Type of information
                   selectInput("type_info_r", h3("Select Type of Information:"),
                               choices = list( "Offensive Variables (Points)"= 1,
                                               "Offensive Variables  (Others)"= 2,
                                               "Defensive Variables" = 3), selected = 1)
                 ),
                 
                 
                 
                 ##***********************************
                 ## Step 3: Selection of Variables (For Heatmap)
                 ##***********************************
                 conditionalPanel(
                   
                   condition = "input.grapht == 6",
                   
                   
                   ## Select Type of information
                   selectInput("type_info_hm", h3("Select Type of Information:"),
                               choices = list( "Offensive Variables"= 1,
                                               "Defensive Variables"= 2,
                                               "Both Types of Variables"= 3), selected = 1)
                 ),
                 
                 
                 
                 
                 ## end of 2nd Conditional
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 
                 ##***********************************
                 ## Step 4: Click Calculate 
                 ##***********************************
                 
                 actionButton("button1","Calculate")
               ),
               
               ##***********************************
               ## Display Content
               ##***********************************
               mainPanel(
                 plotlyOutput("distPlot"),
                 verbatimTextOutput("text1"))
               
               
             )
    ), ## Graphics Panel END
    
    
    
    
    
    ##--------------------------------
    ## Panel 2: Information for the Background Information on Players Panel
    ##--------------------------------
    tabPanel(
      title = 'Background Information on Players', 
      tableOutput('table1'),
      
      
      sidebarLayout(
        sidebarPanel(
          
          ##***********************************
          ## Step 1: Select Type of Information 
          ##***********************************
          selectInput("info_type", "Select Information:",
                      choices = list(
                        "Career Accolades" = 1,
                        "Players Timeline while in NBA" = 2),
                      selected = 1),
          
          ##***********************************
          ## Step 2: Click Calculate 
          ##***********************************
          actionButton("button2",h4("View"))
        )
        ,
        
        ## Talk about Each Player
        mainPanel(
          ##***********************************
          ## Step 3: Display All Players Accolades
          ##***********************************
          conditionalPanel( 
            "input.info_type == 1", 
            tableOutput("nba_players"),
            verbatimTextOutput("text2")
          ),
          ##***********************************
          ## Step 3: Display All Players Timeline
          ##***********************************
          conditionalPanel( 
            "input.info_type == 2", 
            plotlyOutput("myNBAtime"),
            verbatimTextOutput("text3")
          )
          
          
          
        )
      )
      
      
      
      
    ),
    ##--------------------------------
    ## Panel 3: Information for the More Information on Basketball Panel
    ##--------------------------------
    tabPanel(title = 'Glossary on Basketball',
             tableOutput('table2'),
             
             ## Talk about Basketball
             
             sidebarLayout(
               sidebarPanel(

                 ##***********************************
                 ## Step 1: Select Type of Information
                 ##***********************************
                 selectInput("info_typeb", "Select Information:",
                             choices = list(
                               "Offensive Statistics" = 1,
                               "Defensive Statistics" = 2,
                               "Other" = 3,
                               "Positions" = 4,
                               "Graphics" = 5),
                             selected = 1),


                 actionButton("button3",h4("View"))

               ),
                 mainPanel(
                   tableOutput("nba_kable"),
                   verbatimTextOutput("text4")
                 )


               
             )

             
             
             
             
             
             
             
             
             
    ) #,
    
    # tabPanel(
    #   title = 'Survey Form',
    #   tableOutput('table3'), 
    #   
    #   sidebarLayout(
    #     sidebarPanel( 
    #       
    #       uiOutput("survey")
    #       
    #       ),
    #     
    #     
    #     mainPanel(
    #      
    #     )
    #     
    #     )
    #   
    # )
    
    
    
    
    
  ) ## Tabset Panel 
  
  
) ## Fluid Page

##----------------------------------------------------
## Part 4: Server Component
##----------------------------------------------------
server <- function(input, output) {
  
  

  ## Modifications to Variables 
  nba_df$X3P = nba_df$`3P`
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Server Function 1: Get values after button pressed 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inform <- eventReactive(input$button1, {
    season = input$season
    player = input$players
    grapht = input$grapht
    
    ## HeatMap Information
    season_hm = input$season_hm
    type_info_hm = input$type_info_hm
    
    ## Radar Information
    type_info_r = input$type_info_r
    
    
    
    ## Box Plot/ Histogram/Line Plot Variable Selection
    bp_hist_line_Var = input$bp_hist_line_Var
    
    
    ## Scatter Plot Variable Selection
    sc_Vars = input$sc_2stats1
    
    return(list(num_seasons = season,
                wch_players = player,
                type_grapht = grapht,
                v_selct_bphl = bp_hist_line_Var,
                v_selct_scpl = sc_Vars,
                season_hm = season_hm, 
                type_info_r = type_info_r,
                type_info_hm = type_info_hm))
    
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Server Function 1: Plotly Graph Functions
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$distPlot <- renderPlotly({
    
    inform <-inform()
    
    w_value = 1120
    h_value = 405
    
    ## Season Information
    num_seasons = inform$num_seasons
    fst_year = num_seasons[1]; end_year = num_seasons[2]
    imp_years = paste0(rep('season_',(end_year - fst_year)+1),fst_year:end_year)
    
    
    
    ## Player and Colour Information
    sel_players = ifelse(inform$wch_players == '1','KB',ifelse(inform$wch_players =='2','LJ','MJ'))
    sel_colours = ifelse(inform$wch_players == '1','#552582',ifelse(inform$wch_players =='2','#ffb81c','#ce1141'))
    
    
    
    ## Graph Information
    type_grapht = inform$type_grapht
    
    
    
    
    ## Filter and Sort Data Frame Based Selections
    mod_nba_df = nba_df %>%
      filter(Season %in% imp_years,Name %in% sel_players) %>%
      mutate(Name = factor(case_when(
        Name == 'MJ' ~ 'Michael Jordan',
        Name == 'LJ'~ 'LeBron James',
        Name == 'KB'~ 'Kobe Bryant')),
        Season = str_replace_all(Season,'season_','Season '),
        Game_Outcome = case_when(
          Game_Outcome == 'L' ~ 'Losses',
          Game_Outcome == 'W' ~ 'Wins'))
    
    
    ## Make Season Variable in order and factor
    mod_nba_df$Season = factor(mod_nba_df$Season)
    lev = order(as.numeric(str_remove_all(levels(mod_nba_df$Season),'Season ')))
    mod_nba_df$Season = factor(mod_nba_df$Season, levels(mod_nba_df$Season)[lev])
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    if( type_grapht == 1){## Create Jitter Plot
      
      
      ## Find Variable of Interest
      imp_stat_1 = convert_to_stat(inform$v_selct_bphl)
      
  
      p = ggplot(mod_nba_df, aes_string(x='Last_Name',
                                        y= imp_stat_1,
                                        group='Season',
                                        color = 'Last_Name')) + 
        geom_jitter(size=1.75,width = 0.15)+ 
        theme_bw()  +
        scale_y_continuous(minor_breaks = seq(0 , 70, 5)) +
        stat_summary(fun.y=mean, geom="point", shape=12,
                     size=3, color="green") +
        ggtitle(paste0("Average ",abvstat_to_statname(imp_stat_1),' Across ', length(imp_years), ' Sesaons' ))+
        facet_grid(~Season) +
        xlab('Player') +
        ylab(abvstat_to_statname(imp_stat_1))+
        scale_color_manual(values=sel_colours) +
        theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
              legend.position = "none",
              strip.background =element_rect(fill="white"),
              strip.text.x = element_text(size = 13),
              strip.text.y = element_text(size = 8),
              axis.text.y = element_text(size = 8),
              axis.text.x = element_text(size = 10),
              text=element_text(family="Comic Sans MS"),
              axis.title = element_text(size = 13,vjust =2),
              strip.text = element_text(colour = 'black',face = 'bold') ) 
      
      
      ## Modify the dimension of the figure
      gp = ggplotly(p, height = h_value, width = w_value) 
      
      ## Change the label position
      gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.05
      gp %>% layout(margin = list(l = 75))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    } else if(type_grapht == 2){ ## Create Barplot
      
      
      
      ## Create Summary Data Frame
      sum_games_outcomes = mod_nba_df %>%
        dplyr::group_by(Season,Name,Game_Outcome) %>%
        dplyr::summarise(`Number Games`= n()) %>%
        dplyr::rename(`Game Outcome` = Game_Outcome)
      
      ##---------------
      ## Create Barplot
      ##---------------
      
      
      sum_games_outcomes$col = ifelse(sum_games_outcomes$`Game Outcome` == 'Losses',
                                      'white',sum_games_outcomes$Name)
      
      
      
      p = ggplot(sum_games_outcomes, aes(x = `Game Outcome`,
                                         y = `Number Games`,
                                         fill = col,
                                         color = Name)) +
        geom_bar(stat = "identity", position = position_dodge(width=0.7),width=0.5, size=0.25) +
        ggtitle(paste0('Bar Plot between by Game Outcome')) +
        theme_bw() +
        xlab('Outcome of Game') +
        ylab('Number of Games') +
        scale_fill_manual(name = "Players",values=c(sel_colours,"white"),
                          labels = c(sel_players,'')) +
        scale_color_manual(values = sel_colours,guide =F) + 
        facet_grid(~Season) +
        theme(axis.title = element_text(size = 8,face = 'bold'),
              strip.background =element_rect(fill="white"),
              text=element_text(family="Comic Sans MS"),
              strip.text = element_text(size = 15),
              axis.text = element_text(face="bold", size=10, color = 'black'),
              plot.title = element_text(hjust = 0.5,size = 15, face = "bold")) +
        geom_text(aes(label=`Number Games`, y = `Number Games`+0.1*min(`Number Games`)),
                  position = position_dodge(width = 0.7),
                  vjust=-0.40,size = 1.2/0.352777778)
      
      
      ggplotly(p, height = h_value, width = w_value)
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    }  else if(type_grapht == 3){ 
      
      
      
      ## Histogram Selection
      
      ## Find Variable of Interest
      imp_stat_1 = convert_to_stat(inform$v_selct_bphl)
      
      
      
      ## Find the Two Standard Deviations about and Below Mean
      mean.fac = ddply(mod_nba_df, .(Season,Name), function(.d)
        data.frame(Mean=mean(.d[,imp_stat_1],na.rm = T),
                   x_sd=sd(.d[,imp_stat_1],na.rm = T),
                   x_n = length(.d[,imp_stat_1]) )) %>%
        mutate(`2 SDs Below Mean` = Mean - 2 * x_sd,
               
               `2 SDs Above Mean` = Mean + 2 * x_sd)
      
      ## Determine Number of bins based on smalles number of games played by any players
      num_bins = ifelse(min(mean.fac$x_n)<70, 10,30)
      
      
      
      ## Create Histogram
      p <-  ggplot(mod_nba_df,aes_string(x = imp_stat_1,fill = 'Name')) +
        geom_histogram(bins = num_bins) +
        ggtitle(paste0('Histogram ',abvstat_to_statname(imp_stat_1))) +
        theme_bw() +
        xlab(abvstat_to_statname(imp_stat_1)) +
        ylab('Frequency') +
        geom_vline(data=mean.fac, aes(xintercept=Mean),color ='black',size = 0.4) +
        geom_vline(data=mean.fac, aes(xintercept=`2 SDs Below Mean`),color ='black',size = 0.4,linetype="dashed") +
        geom_vline(data=mean.fac, aes(xintercept=`2 SDs Above Mean`),color ='black',size = 0.4,linetype="dashed") +
        facet_grid(Name~Season)+
        scale_fill_manual(values= sel_colours) +
        scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red")) +
        theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
              legend.position = "none",
              strip.background =element_rect(fill="white"),
              strip.text.x = element_text(size = 12,face = 'bold'),
              strip.text.y = element_text(size = 9,face = 'bold'),
              axis.title = element_text(size = 13,vjust =2),
              axis.text = element_text(size = 8),
              strip.text = element_text(colour = 'black',face = 'bold',size = 13),
              plot.margin = margin(2, 4, 0.1, 8, "mm"),
              text=element_text(family="Comic Sans MS"))
      
      
      ## Modify the dimension of the figure
      gp = ggplotly(p, height = h_value, width = w_value) 
      
      ## Change the label position
      gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.05
      gp %>% layout(margin = list(l = 75))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    } else if(type_grapht == 4){ ## Create Boxplot
      
      
      ## Find Variable of Interest
      imp_stat_1 = convert_to_stat(inform$v_selct_bphl)
      
      
      
      
      ## Create Box_Plot
      p = ggplot(mod_nba_df, aes_string(x= "Last_Name",y = imp_stat_1, fill = "Last_Name")) +
        geom_boxplot() +
        ggtitle(paste0(abvstat_to_statname(imp_stat_1)," by Season")) +
        scale_fill_manual(values=sel_colours) +
        facet_grid(~Season) +
        theme_bw() +
        xlab('Player') +
        ylab(abvstat_to_statname(imp_stat_1)) +
        theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
              legend.position = "none",
              strip.background =element_rect(fill="white"),
              strip.text = element_text(size = 13),
              text=element_text(family="Comic Sans MS"),
              axis.text.x = element_text(size = 12),
              axis.title.x = element_text(size=14, face="bold"),
              axis.title.y = element_text( size=14, face="bold")) +
        xlab("Season")
      
      ## Modify the dimension of the figure
      gp = ggplotly(p, height = h_value, width = w_value) 
      
      ## Change the label position
      gp[['x']][['layout']][['annotations']][[2]][['x']] <- -0.05
      gp %>% layout(margin = list(l = 75))
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    } else if(type_grapht == 5){ ## Create Radar Plot
      
      ## Find Variable of Interest
      sc_vars = inform$v_selct_scpl; select_numbers = as.numeric(sc_vars)
      
      
      
      ## Manipulate Radar DF
      name_df = data.frame(vars_name = unique(radar_df$Statistic),
                           seq_num = 1:length(unique(radar_df$Statistic)))
      
      ## Get Selected Variables
      vars = name_df %>%
        filter(seq_num %in% select_numbers)
      
      ## Select Season and Type of Information
      season_num = inform$season_hm
      type_info_r = inform$type_info_r
      imp_season = paste0('season_',season_num)

      
      ## SET MARGINS
      m <- list( 
        l = 50,
        r = 50,
        b = 100,
        t = 80,
        pad = 4
      )
      
      
      ## CANNOT ADD ANOTHER Variable / ONLY 3 Variables
      if(type_info_r == 1) {                   ## Offense (POINTS ONLY)
        imp_vars = c('X3P','FG','PTS','Year')
        title_change = 'Offense (POINTS ONLY)'
      } else if(type_info_r == 2){             ## Offense
        imp_vars = c('ORB','AST','TOV','Year')
        title_change = 'Offense (Others)'
      }  else {                                ## Defense
        imp_vars = c('STL','BLK','DRB','Year')
        title_change = 'Defense'
      }
      
      
      
      
      
      
      
      
      m_imp_season = tools::toTitleCase(str_replace_all(imp_season,'_',' '))
      mod_df = radar_df %>%
        filter(Season == imp_season,
               Statistic  %in% imp_vars) 
      
      mod_df[nrow(mod_df)+1,] = mod_df[1,]
      
      ## CANNOT ADD ANOTHER Variable
      hover_text_KB =  paste0('Statistic: ',mod_df$Statistic,' \n ','Rank: ',
                              mod_df$KB,' in ',rep(mod_df$KB[4]-1,4),' \n Player: KB ')
      hover_text_KB = hover_text_KB[-4]
      hover_text_MJ =  paste0('Statistic: ',mod_df$Statistic,' \n ','Rank: ',
                              mod_df$MJ,' in ',rep(mod_df$MJ[4]-1,4),' \n Player: MJ ')
      hover_text_MJ = hover_text_MJ[-4]
      hover_text_LJ =  paste0('Statistic: ',mod_df$Statistic,' \n ','Rank: ',
                              mod_df$LJ,' in ',rep(mod_df$LJ[4]-1,4),' \n Player: LJ ')    
      hover_text_LJ = hover_text_LJ[-4]
      
      
      mod_df = mod_df[-4,]
      
      
      num_m = mod_df %>% select(KB,MJ,LJ) %>%max() 
      f_title <- list(
        family = "Comic Sans MS",
        color = "black"
      )
      
      
      
      mod_df %>% 
        plot_ly(
          type = 'scatterpolar',
          marker = list(color = 'black')
          
        ) %>%
        add_trace( 
          r = ~MJ,
          theta = ~Statistic,
          name = 'Michael Jordan',
          hoverinfo='text',
          text = hover_text_MJ,
          line = list(color = '#ce1141', width = 2),
          mode = 'lines'
          
        ) %>%
        add_trace(
          r = ~LJ,
          theta = ~Statistic,
          name = 'LeBron James',
          hoverinfo='text',
          text = hover_text_LJ,
          line = list(color = '#ffb81c', width = 2),
          mode = 'lines'
          
        ) %>%
        add_trace(
          r = ~KB,
          theta = ~Statistic,
          hoverinfo='text',
          text = hover_text_KB,
          name = 'Kobe Byrant',
          line = list(color = '#552582', width = 2, dash = 'dot'),
          mode = 'lines'
        )  %>%
        layout(
          title =  paste0(m_imp_season," Ranking Against Other \n Players Performance ",
                          title_change ),
          font=f_title,
          margin=m,
          polar = list(
            radialaxis = list(
              visible = T,
              range = c(0,num_m +5)
            )
          )
        ) 
      
      
      
    } else if(type_grapht == 6){ ## Create Heatmap
      
      
      
      
      
      
      ##-----------------------------------------------------------
      ## Heatmap 0: Data Curation for Heatmap
      ##-----------------------------------------------------------
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## x and y for Heatmaps Multiplayer
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Font Type for x and y tick
      f_xy_tick <- list(
        family = "Comic Sans MS",
        size = 16,
        color = "black"
      )
      f_xy_title <- list(
        family = "Comic Sans MS",
        size = 20,
        color = "black"
      )
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Font Type for title
      f_title <- list(
        family = "Comic Sans MS",
        size = 20,
        color = "black"
      )
      x_heatmaps <- list(
        title = "Statistics",
        titlefont = f_xy_title,
        tickfont = f_xy_tick
      )
      y_heatmaps <- list(
        title = "Players",
        titlefont = f_xy_title,
        tickfont = f_xy_tick
      )
      
      m <- list( 
        l = 50,
        r = 50,
        b = 100,
        t = 80,
        pad = 4
      )
      ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      ## Select Season and Type of Information
      season_num = inform$season_hm
      type_info_hm = inform$type_info_hm
      
      
      
      
      ## Gather Particular Season information
      summarize_data_nba = heatmap_df %>%
        filter(season_number == season_num) %>%
        select(-season_number) 
      
      
      
      
      
      
      ## Get Appropriate Data set Based on Selection
      
      if(type_info_hm == 3){
        sum_bot_df = summarize_data_nba %>%
          select(Name,PF,TRB,DD,TD)
        info_m = create_rank_df(sum_bot_df,T)
        hover_labels = info_m$hover_labels
        data_nba = info_m$data_nba
        specified_info = 'Both Type of Variables'
      } else if( type_info_hm == 2){
        sum_def_df = summarize_data_nba %>%
          select(Name,STL,BLK,DRB)
        info_m = create_rank_df(sum_def_df,F)
        hover_labels = info_m$hover_labels
        data_nba = info_m$data_nba
        specified_info = 'Defensive Variables'
      } else{
        sum_off_df = summarize_data_nba %>%
          select(Name,FG,X3P,ORB,AST,PTS,TOV)
        info_m = create_rank_df(sum_off_df,F)
        hover_labels = info_m$hover_labels
        data_nba = info_m$data_nba
        specified_info = 'Offensive Variables'
      }
      
      
      
      ##-----------------------------------------------------------
      ##  Plot of Heatmap
      ##-----------------------------------------------------------
      
      
      ## Create ranking for Color
      interval.cols <- c('orange','yellow','red')
      names(interval.cols) <- c('3','2','1')
      
      interval.cols2 <- rep(interval.cols, each=ncol(data_nba))
      color.df <- data.frame(range=c(0:(2*length(interval.cols)-1)),
                             colors=c(0:(2*length(interval.cols)-1)))
      color.df <- setNames(data.frame(color.df$range,color.df$colors),NULL)
      for (i in 1:(2*length(interval.cols))) {
        color.df[[2]][[i]] <- interval.cols[[(i + 1) / 2]]
        color.df[[1]][[i]] <-  i/(2*length(interval.cols))-(i %% 2)/(2*length(interval.cols))
      }
      
      ## Provide Full Name for Variables
      colnames(data_nba) = unlist(map(colnames(data_nba),abvstat_to_statname))
      
      data_nba <- data_nba[ c(3,2,1), ]
      hover_labels <-  hover_labels[c(3,2,1),]
      ## Plot Graphic  
      p <- plot_ly(x=colnames(data_nba),
                   y=rownames(data_nba),
                   z = 4 - data_nba,
                   xgap = 5,
                   ygap = 5,
                   
                   type = "heatmap",
                   hoverinfo='text',
                   text=hover_labels,
                   colors=interval.cols2,
                   colorscale=color.df,
                   colorbar=list(tickmode='array',tickvals=c(1:3),ticktext=names(interval.cols),
                                 len=0.375,outlinecolor="white",bordercolor="white",borderwidth=9,
                                 bgcolor="white"))  %>%
        layout(xaxis = x_heatmaps,
               yaxis = y_heatmaps, 
               title = paste0("Season ",season_num," Heatmap of Players Rank \n (",specified_info,")",sep=""),
               font =f_title, margin=m )
      
      p
      
      
    }
    
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## URL: For the survey
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  url <- a("Survey Form", href="https://forms.gle/oLq2XZHJ3byBTX7n9")
  output$survey <- renderUI({
    tagList("URL link:", url)
  })
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Server Function 2: Get values after button pressed 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  inform2 <- eventReactive(input$button2, {
    info_type = input$info_type
    
    return(list(info_type = info_type))
    
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Server Function 2: Table
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nba_players <- function() {
    inform2 <-inform2()
    info_type = inform2$info_type
    
    if(info_type == 1){
      
      colnames(player_info)[1]=''
      rownames(player_info) =NULL
      
      print( htmlTable(player_info,css.cell="padding-left: .5em; padding-right: .5em; align: left; align: left; vertical-align: top;",
                       rnames=FALSE))
      
      
      
    }
  }
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Server Function 2: Timeline
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$myNBAtime <- renderPlotly({
    
    p = vistime(timeline_df,
                events="Position",
                groups="Name",
                title="Players in League")
    pp <- plotly_build(p)
    for(i in grep("yaxis*", names(pp$x$layout))){
      pp$x$layout[[i]]$tickfont <- list(size = 12)
    }
    pp
  })

  
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Server Function 3: Get values after button pressed 
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    inform3 <- eventReactive(input$button3, {
      info_typeb = input$info_typeb
      
      return(list(info_typeb = info_typeb))
      
    })
  
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Server Function 3: Table
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$nba_kable <- function() {
    inform <-inform3()
    info_type = inform$info_typeb
    
    defintions_df %>%
      filter(Info_Number == info_type) %>%
      select(Abbreviation,Statistic,Definition) %>%
      kable("html") %>%
      kable_styling("striped", full_width = F) %>%
      column_spec(3,width = "10cm")
    
  }
  
  
  
  
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## General Function stating where data is coming from 
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$text1 <- renderText({
    paste("Note: Use 'More Information on Basketball' Panel for Definitions of Abbreviations", 
          "Source: www.basketball-reference.com", sep='\n') 
  })
  
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Text: For Accolades
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$text2 <- renderText({
    
    paste("Note: Use 'More Information on Basketball' Panel for Definitions of Abbreviations", 
            "Source: www.basketball-reference.com", sep='\n') 
    
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Text: For Timeline
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$text3 <- renderText({
    
    paste("* - Washington Wizards",
    "** -  Los Angles Lakers",
    "Source: www.basketball-reference.com", sep='\n') 
   
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Text: For More Information on Basketball
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output$text4 <- renderText({
    
    paste(
      "https://www.basketballforcoaches.com/basketball-terms/",
      "https://hoopsu.com/basketball-terminology/",
      "www.ducksters.com",
      "https://datavizcatalogue.com/methods/radar_chart.html",
      sep='\n') 
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

