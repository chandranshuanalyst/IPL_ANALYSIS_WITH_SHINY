#library(shiny)
teams=c("Kolkata Knight Riders","Royal Challengers Bangalore","Kings XI Punjab",            
        "Chennai Super Kings","Rajasthan Royals","Delhi Daredevils","Mumbai Indians","Deccan Chargers","Kochi Tuskers Kerala",       
        "Pune Warriors","Sunrisers Hyderabad","Rising Pune Supergiants",    
        "Gujarat Lions","Rising Pune Supergiant","Delhi Capitals"  )
inning=c(1,2,0)
library(magrittr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(tidyr)
library(bslib)
ipl=read.csv("Data/ipl.csv")
ipl_b=read.csv("Data/ipl_b.csv")
ipl_m=read.csv("Data/ipl_m.csv")
batsman_names=unique(ipl$batsman)
bowler_names=unique(ipl$bowler)
light <- bs_theme(bg = "DarkSlateBlue", 
                  fg = "#FBFCFC", 
                  primary = "#E69F00", 
                  base_font = font_google("Inter"),
                  code_font = font_google("JetBrains Mono"))
dark <- bs_theme(bg = "black", fg = "white", primary = "purple")
ui <- navbarPage(title = "IPL ANALYSIS
                 ",
                 theme = light, 
                 checkboxInput("dark_mode", "Dark mode"),
                 navbarMenu("Team_bat",
                            tabPanel("Top Batters",
                                     radioButtons("team","Select team",teams),
                                     radioButtons("inning","Select innings",inning),
                                     numericInput("num", "limit", value = 5, min = 0, max = 10),
                                     dataTableOutput("top_bat")
                            ),tabPanel("Destructive batters",
                                       radioButtons("team2","Select team",teams),
                                       radioButtons("inning2","Select innings",inning),  
                                       numericInput("num2", "limit", value = 5, min = 0, max = 10),
                                       dataTableOutput("destructive_bat")
                            ),
                            tabPanel("Top 4's ",
                                     radioButtons("team5","Select team",teams),
                                     radioButtons("inning5","Select innings",inning),  
                                     numericInput("num5", "limit", value = 5, min = 0, max = 10),
                                     dataTableOutput("four")
                            ),tabPanel("Top 6's ",
                                       radioButtons("team6","Select team",teams),
                                       radioButtons("inning6","Select innings",inning),  
                                       numericInput("num6", "limit", value = 5, min = 0, max = 10),
                                       dataTableOutput("six")
                            )),
                 navbarMenu("Team_bowl",
                            tabPanel("Top Bowlers",
                                     radioButtons("team3","Select team",teams),
                                     radioButtons("inning3","Select innings",inning),
                                     numericInput("num3", "limit", value = 5, min = 0, max = 10),
                                     dataTableOutput("top_bowl")
                            ),tabPanel("Strike Bowlers",
                                       radioButtons("team4","Select team",teams),
                                       radioButtons("inning4","Select innings",inning), 
                                       numericInput("num4", "limit", value = 5, min = 0, max = 10),
                                       dataTableOutput("strike_bowl")
                            ),tabPanel("Economical Bowlers",
                                       radioButtons("team7","Select team",teams),
                                       radioButtons("inning7","Select innings",inning), 
                                       numericInput("num7", "limit", value = 5, min = 0, max = 10),
                                       dataTableOutput("eco_bowl")),
                            tabPanel("Poor Bowlers",
                                     radioButtons("team8","Select team",teams),
                                     radioButtons("inning8","Select innings",inning), 
                                     numericInput("num8", "limit", value = 5, min = 0, max = 10),
                                     dataTableOutput("poor_bowl"))),
                 navbarMenu("Season Wise Records",
                            tabPanel("Best Batter of season",
                                     radioButtons("team9","Select team",teams),
                                     dataTableOutput("bat")),
                            tabPanel("Best Bowler of season",
                                     radioButtons("team10","Select team",teams),
                                     dataTableOutput("bowl"))
                 ),
                 navbarMenu("TEAM VS TEAM",
                            tabPanel("Batting records",
                                     radioButtons("team11","Select batting team",teams),
                                     radioButtons("team12","Select bowling team",teams),
                                     dataTableOutput("bat_bowl")),
                            tabPanel("Bowling Records",
                                     radioButtons("team13","Select batting team",teams),
                                     radioButtons("team14","Select bowling team",teams),
                                     dataTableOutput("bowl_bat"))
                 ),
                 navbarMenu("INDIVIDUAL BATSMAN PERFORMANCES",
                            tabPanel("Batsman Overall Record",
                                     selectInput(
                                         "name1", "Select Batsman",batsman_names),
                                     dataTableOutput("batsman")
                            ),
                            tabPanel("Batsman Position Wise Record",
                                     selectInput(
                                         "name2", "Select Batsman",batsman_names),
                                     dataTableOutput("batsman2")
                            ),
                            tabPanel("Batsman Venue Wise Record",
                                     selectInput(
                                         "name3", "Select Batsman",batsman_names),
                                     dataTableOutput("batsman3")
                            ),
                            tabPanel("Batsman Inning Wise Record",
                                     selectInput(
                                         "name4", "Select Batsman",batsman_names),
                                     dataTableOutput("batsman4")
                            ),
                            tabPanel("Batsman Season Wise Record",
                                     selectInput(
                                         "name5", "Select Batsman",batsman_names),
                                     dataTableOutput("batsman5")
                            ),
                            tabPanel("High Risk Bowlers",
                                     selectInput(
                                         "name6", "Select Batsman",batsman_names),
                                     dataTableOutput("batsman6")
                            ),
                            tabPanel("Low Risk Bowlers",
                                     selectInput(
                                         "name7", "Select Batsman",batsman_names),
                                     dataTableOutput("batsman7")
                            )),
                 navbarMenu("INDIVIDUAL BOWLER PERFORMANCES",
                            tabPanel("Bowler Overall Record",
                                     selectInput(
                                         "name8", "Select Bowler",bowler_names),
                                     dataTableOutput("bowler1")
                            ),
                            tabPanel("Bowler Seasonwise Record",
                                     selectInput(
                                         "name9", "Select Bowler",bowler_names),
                                     dataTableOutput("bowler2")
                            ),
                            tabPanel("Bowler Inningwise Record",
                                     selectInput(
                                         "name10", "Select Bowler",bowler_names),
                                     dataTableOutput("bowler3")
                            ),
                            tabPanel("High Risk Batters",
                                     selectInput(
                                         "name11", "Select Bowler",bowler_names),
                                     dataTableOutput("bowler4")
                            ),
                            tabPanel("Low Risk Batters",
                                     selectInput(
                                         "name12", "Select Bowler",bowler_names),
                                     dataTableOutput("bowler5")
                            ),
                            tabPanel("Top Venues",
                                     selectInput(
                                         "name13", "Select Bowler",bowler_names),
                                     dataTableOutput("bowler6")
                            )
                 )
                 
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    observe(session$setCurrentTheme(
        if (isTRUE(input$dark_mode)) dark else light
    ))
    top_bat=function(team,inn,limit){
        ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,18)]
        ipl_b3=ipl_b2[ipl_b2$batting_team==team,]
        
        if(inn ==1||inn==2){
            df3<- ipl_b3 %>% group_by(batsman,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
                as.data.frame()
            
            noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed,inning)%>%summarise(outs=n(),.groups = 'drop')
            colnames(noa)[1]="batsman"
            df3=merge(df3,noa,by=c("batsman","inning"))
            df3["average"]=df3$sum_runs/df3$outs
            df3=df3[order(df3$sum_runs,decreasing = TRUE),]
            top_inn=head(df3[df3$inning==inn,],limit)
        }else{
            df3<- ipl_b3 %>% group_by(batsman) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,.groups = 'drop')%>%
                as.data.frame()
            
            noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n(),.groups = 'drop')
            colnames(noa)[1]="batsman"
            df3=merge(df3,noa,by="batsman")
            df3["average"]=df3$sum_runs/df3$outs
            df3=df3[order(df3$sum_runs,decreasing = TRUE),]
            top_inn=head(df3,limit)
        }
        return(top_inn)
    }
    destructive_bat=function(team,inn,limit){
        ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,18)]
        ipl_b3=ipl_b2[ipl_b2$batting_team==team,-4]
        ipl_b4=ipl_b3%>%
            filter(extras_type!="wides"|is.na(extras_type))%>%
            filter(extras_type!="noballs"|is.na(extras_type))
        
        if(inn ==1||inn==2){
            df3<- ipl_b3 %>% group_by(batsman,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
                as.data.frame()
            
            noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed,inning)%>%summarise(outs=n(),.groups = 'drop')
            colnames(noa)[1]="batsman"
            df3=merge(df3,noa,by=c("batsman","inning"))
            df3["average"]=df3$sum_runs/df3$outs
            df3=df3[df3$sum_runs>200,][order(df3[df3$sum_runs>200,5],decreasing = TRUE),]
            top_inn=head(df3[df3$inning==inn,],limit)
        }else{
            df3<- ipl_b3 %>% group_by(batsman) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,.groups = 'drop')%>%
                as.data.frame()
            
            noa=ipl_b3%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n(),.groups = 'drop')
            colnames(noa)[1]="batsman"
            df3=merge(df3,noa,by="batsman")
            df3["average"]=df3$sum_runs/df3$outs
            df3=df3[df3$sum_runs>200,][order(df3[df3$sum_runs>200,4],decreasing = TRUE),]
            top_inn=head(df3,limit)
        }
        return(top_inn)
    }
    four_bat=function(team,inn,limit){
        ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,16,18)]
        ipl_b3=ipl_b2[ipl_b2$batting_team==team,-4]
        
        if(inn ==1||inn==2){
            df10<- ipl_b3 %>% group_by(batsman,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),fours=sum(batsman_runs==4),.groups = 'drop')%>%
                as.data.frame()
            df3=df10[order(df10$fours,decreasing = TRUE),]
            top_4=head(df3[df3$inning==inn,],limit)
        }else{
            df10<- ipl_b3 %>% group_by(batsman) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),fours=sum(batsman_runs==4),.groups = 'drop')%>%
                as.data.frame()
            df3=df10[order(df10$fours,decreasing = TRUE),]
            top_4=head(df3,limit)
        }
        return(top_4)
    }
    six_bat=function(team,inn,limit){
        ipl_b2=ipl_b[,-c(3,4,6,9,10,11,12,13,15,16,18)]
        ipl_b3=ipl_b2[ipl_b2$batting_team==team,-4]
        
        if(inn ==1||inn==2){
            df10<- ipl_b3 %>% group_by(batsman,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sixes=sum(batsman_runs==6),.groups = 'drop')%>%
                as.data.frame()
            df3=df10[order(df10$sixes,decreasing = TRUE),]
            top_4=head(df3[df3$inning==inn,],limit)
        }else{
            df10<- ipl_b3 %>% group_by(batsman) %>% 
                summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sixes=sum(batsman_runs==6),.groups = 'drop')%>%
                as.data.frame()
            df3=df10[order(df10$sixes,decreasing = TRUE),]
            top_4=head(df3,limit)
        }
        return(top_4)
    }
    top_bowl=function(team,inn,limit){
        ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
        ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
        ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
        if(inn ==1||inn==2){
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(balls=n(),.groups = 'drop')
            df6<- ipl_b6 %>% 
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df7<- df6 %>% 
                group_by(bowler,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
                as.data.frame()
            df7=merge(df7,sr,by=c("bowler","inning"))
            df7=merge(df7,balls,by=c("bowler","inning"))
            df7['sr']=df7$balls/df7$sum_wkts
            df7['avg']=df7$runs/df7$sum_wkts
            df7['eco']=df7$runs/(df7$balls/6)
            df7=df7[order(df7$sum_wkts,decreasing = TRUE),]
            df7=df7[order(df7$sum_wkts,decreasing = TRUE),]
            top_inn=head(df7[df7$inning==inn,],limit)
        }else{
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(balls=n(),.groups = 'drop')
            df6<- ipl_b6 %>%
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df6=df6 %>%
                group_by(bowler) %>% 
                summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
                as.data.frame()
            df6=merge(df6,sr,by="bowler")
            df6=merge(df6,balls,by="bowler")
            df6['sr']=df6$balls/df6$sum_wickets
            df6['avg']=df6$runs/df6$sum_wickets
            df6['eco']=df6$runs/(df6$balls/6)
            df6=df6[order(df6$sum_wickets,decreasing = TRUE),]
            top_inn=head(df6,limit)
        }
        return(top_inn)
    }
    strike_bowl=function(team,inn,limit){
        ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
        ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
        ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
        if(inn ==1||inn==2){
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(balls=n())
            df6<- ipl_b6 %>% 
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df7<- df6 %>% 
                group_by(bowler,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
                as.data.frame()
            df7=merge(df7,sr,by=c("bowler","inning"))
            df7=merge(df7,balls,by=c("bowler","inning"))
            df7['sr']=df7$balls/df7$sum_wkts
            df7['avg']=df7$runs/df7$sum_wkts
            df7['eco']=df7$runs/(df7$balls/6)
            df7=df7[df7$inning==inn,]
            strike=head(df7[df7$sum_wkts>25,][order(df7[df7$sum_wkts>25,7]),],limit)
        }else{
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(balls=n(),.groups = 'drop')
            df6<- ipl_b6 %>%
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df6=df6 %>%
                group_by(bowler) %>% 
                summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
                as.data.frame()
            df6=merge(df6,sr,by="bowler")
            df6=merge(df6,balls,by="bowler")
            df6['sr']=df6$balls/df6$sum_wickets
            df6['avg']=df6$runs/df6$sum_wickets
            df6['eco']=df6$runs/(df6$balls/6)
            strike=head(df6[df6$sum_wickets>25,][order(df6[df6$sum_wickets>25,6]),],limit)
        }
        return(strike)
    }
    eco_bowl=function(team,inn,limit){
        ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
        ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
        ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
        if(inn ==1||inn==2){
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(balls=n(),.groups = 'drop')
            df6<- ipl_b6 %>% 
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df7<- df6 %>% 
                group_by(bowler,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
                as.data.frame()
            df7=merge(df7,sr,by=c("bowler","inning"))
            df7=merge(df7,balls,by=c("bowler","inning"))
            df7['sr']=df7$balls/df7$sum_wkts
            df7['avg']=df7$runs/df7$sum_wkts
            df7['eco']=df7$runs/(df7$balls/6)
            df7=df7[df7$inning==inn,]
            eco=head(df7[df7$sum_wkts>25,][order(df7[df7$sum_wkts>25,9]),],limit)
        }else{
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(balls=n(),.groups = 'drop')
            df6<- ipl_b6 %>%
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df6=df6 %>%
                group_by(bowler) %>% 
                summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
                as.data.frame()
            df6=merge(df6,sr,by="bowler")
            df6=merge(df6,balls,by="bowler")
            df6['sr']=df6$balls/df6$sum_wickets
            df6['avg']=df6$runs/df6$sum_wickets
            df6['eco']=df6$runs/(df6$balls/6)
            eco=head(df6[df6$sum_wickets>25,][order(df6[df6$sum_wickets>25,8]),],limit)
        }
        return(eco)
    }
    poor_bowl=function(team,inn,limit){
        ipl_b5=ipl_b[,-c(3,4,5,6,8,9,11,14,15,17)]
        ipl_b6=ipl_b5[ipl_b5$bowling_team==team,]
        ipl_b6$is_wicket=replace_na(ipl_b6$is_wicket,0)
        if(inn ==1||inn==2){
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler,inning)%>%
                summarise(balls=n(),.groups = 'drop')
            df6<- ipl_b6 %>% 
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df7<- df6 %>% 
                group_by(bowler,inning) %>% 
                summarise(matches= sum(table(unique(id))),sum_wkts=sum(is_wicket),.groups = 'drop')%>%#,sr=mean(batsman_runs)*100,.groups = 'drop')%>%
                as.data.frame()
            df7=merge(df7,sr,by=c("bowler","inning"))
            df7=merge(df7,balls,by=c("bowler","inning"))
            df7['sr']=df7$balls/df7$sum_wkts
            df7['avg']=df7$runs/df7$sum_wkts
            df7['eco']=df7$runs/(df7$balls/6)
            df7=df7[df7$inning==inn,]
            poor=head(df7[df7$sum_wkts>1,][order(df7[df7$sum_wkts>1,7],decreasing = TRUE),],limit)
        }else{
            sr=ipl_b6 %>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(runs=sum(total_runs),.groups = 'drop')
            balls=ipl_b6 %>%
                filter(extras_type!="wides"|is.na(extras_type))%>%
                filter(extras_type!="noballs"|is.na(extras_type))%>%
                group_by(bowler)%>%
                summarise(balls=n(),.groups = 'drop')
            df6<- ipl_b6 %>%
                filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
                filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
                filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
                filter(extras_type!="byes"|is.na(extras_type))%>%
                filter(extras_type!="legbyes"|is.na(extras_type))
            df6=df6 %>%
                group_by(bowler) %>% 
                summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
                as.data.frame()
            df6=merge(df6,sr,by="bowler")
            df6=merge(df6,balls,by="bowler")
            df6['sr']=df6$balls/df6$sum_wickets
            df6['avg']=df6$runs/df6$sum_wickets
            df6['eco']=df6$runs/(df6$balls/6)
            poor=head(df6[df6$sum_wickets>1,][order(df6[df6$sum_wickets>1,6],decreasing = TRUE),],limit)
        }
        return(poor)
    }
    top_bat_season=function(name){
        df=ipl[ipl$batting_team==name,]
        tt=df%>%
            group_by(season,batsman)%>%
            summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),.groups = 'drop')
        tt=tt%>%
            group_by(season)%>%
            filter(runs==max(runs))%>%
            as.data.frame()
        return(tt)
    }
    top_bowl_season=function(name){
        df=ipl[ipl$bowling_team==name,]
        tt2=df%>%
            filter(dismissal_kind!="run out")%>%
            group_by(season,bowler)%>%
            summarise(matches=sum(table(unique(id))),wkts=sum(is_wicket),.groups = 'drop')
        tt2=tt2%>%
            group_by(season)%>%
            filter(wkts==max(wkts))%>%
            as.data.frame()
        return(tt2)
    }
    batting_vsteam=function(bat,bowl){
        df1=ipl[ipl$batting_team==bat&ipl$bowling_team==bowl,]
        df2=df1%>%group_by(batsman)%>%
            summarise(matches=sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100,.groups = 'drop')%>%
            arrange(desc(runs))%>%
            head(5)%>%
            as.data.frame()
        return (df2)
    }
    bowling_vsteam=function(bat,bowl){
        df1=ipl[ipl$batting_team==bat&ipl$bowling_team==bowl,]
        df2=df1%>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            group_by(bowler)%>%
            summarise(matches=sum(table(unique(id))),wickets=sum(is_wicket),.groups = 'drop')%>%
            arrange(desc(wickets))%>%
            head(5)%>%
            as.data.frame()
        return (df2)
    }
    batsman_performance=function(b){
        bat=bat2[bat2$batsman==b ,]
        df3<- bat %>% group_by(batsman) %>% 
            summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
            as.data.frame()
        
        noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(player_dismissed)%>%summarise(outs=n(),.groups = 'drop')
        colnames(noa)[1]="batsman"
        df3=merge(df3,noa,by="batsman")
        df3["average"]=df3$sum_runs/df3$outs
        df3['not_outs']=df3$matches-df3$outs
        f<- bat %>% group_by(batsman) %>% 
            summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
            as.data.frame()
        #6's
        s<- bat %>% group_by(batsman) %>% 
            summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(df3,f,s)
        df3 <- list_df %>% reduce(right_join, by='batsman')
        ducks= bat %>% group_by(date) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs==0)
        hundred<- bat %>% group_by(date) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs>=100)
        fifty<- bat %>% group_by(date) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs>=50&runs<100)
        df3['fifty']=max(length(fifty$runs),0)
        df3['hundred']=max(length(hundred$runs),0)
        df3['hs']=max(max(hundred$runs,0),max(fifty$runs,0))
        df3['ducks']=length(ducks$runs)
        df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
        mom=bat%>%
            group_by(player_of_match)%>%
            filter(player_of_match==b)%>%
            summarise(mom=sum(table(unique(id))),.groups = 'drop')%>%
            as.data.frame()
        df3["mom"]=max(mom$mom,0)
        return(df3)
    }
    position_wise=function(b){
        bat=bat2[bat2$batsman==b ,]
        df4=bat%>%
            group_by(position)%>%
            summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
            as.data.frame()
        
        noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(position,player_dismissed)%>%summarise(outs=n(),.groups = 'drop')
        colnames(noa)[2]="batsman"
        noa=noa%>%filter(!is.na(batsman))
        noa=noa[noa$batsman==b,]
        df4=merge(df4,noa,by=c("position"),all.x = TRUE)
        df4["average"]=df4$sum_runs/df4$outs
        df4['not_outs']=df4$matches-df4$outs
        f<- bat %>% group_by(position,batsman) %>% 
            summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
            as.data.frame()
        #6's
        s<- bat %>% group_by(position,batsman) %>% 
            summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(df4,f,s)
        df4 <- list_df %>% reduce(left_join, by=c('position','batsman'))
        
        hundred<- bat %>% group_by(date,position) %>% 
            summarise(runs=sum(batsman_runs))%>%
            filter(runs>=100)
        hundred<-hundred%>%
            group_by(position)%>%
            summarise(hundred=n())
        if(dim(hundred)[1]==0){
            hundred=matrix(c(df4$position,rep(0,length(df4$position))),nrow = length(df4$position),ncol=2)
            colnames(hundred)=c("position","hundred")
        }
        df4=merge(df4,hundred,by="position",all.x=TRUE)
        
        fifty<- bat %>% group_by(date,position) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs>=50&runs<100)
        fifty<-fifty%>%
            group_by(position)%>%
            summarise(fifty=n(),.groups = 'drop')
        
        df4=merge(df4,fifty,by="position",all.x = TRUE)
        
        hs=bat%>%
            group_by(position,id)%>%
            summarise(runs=sum(batsman_runs),.groups = 'drop')
        hs=hs%>%
            group_by(position)%>%
            filter(runs==max(runs))
        hs=hs[,-2]
        colnames(hs)[2]="hs"
        df4=merge(df4,hs,by="position",all.x = TRUE)
        
        ducks=bat%>%
            group_by(position,date)%>%
            summarise(runs=sum(batsman_runs),.groups = 'drop')
        ducks=ducks[,-2]
        ducks=ducks%>%
            group_by(position)%>%
            filter(runs==0)
        ducks=ducks%>%
            group_by(position)%>%
            summarise(ducks=n(),.groups = 'drop')
        if(dim(ducks)[1]==0){
            ducks=data.frame(matrix(c(df4$position,rep(0,length(df4$position))),ncol=2,nrow=length(df4$position)))
            colnames(ducks)=c("position","ducks")
        }
        df4=merge(df4,ducks,by="position",all.x = TRUE)
        
        df4["boundary_percent"]=((df4$fours+df4$sixes)/df4$balls_faced)*100
        
        mom=bat%>%
            group_by(position,player_of_match)%>%
            filter(player_of_match==b)%>%
            summarise(mom=sum(table(unique(id))),.groups = 'drop')%>%
            as.data.frame()
        
        mom=mom[,-2]
        if(dim(mom)[1]==0){
            mom=data.frame(matrix(c(df4$position,rep(0,length(df4$position))),ncol=2,nrow=length(df4$position)))
            colnames(mom)=c("position","mom")
        }
        df4=merge(df4,mom,by="position",all.x = TRUE)
        df4[is.na(df4)]=0
        df4=select(df4,-"batsman")
        return(df4)
    }
    venue_wise=function(b){
        bat=bat2[bat2$batsman==b ,]
        ven=bat%>%
            group_by(venue,date)%>%
            summarise(matches= sum(table(unique(id))),runs=sum(batsman_runs),sr=runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
            as.data.frame()
        
        f<- bat %>% group_by(venue,date) %>% 
            summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
            as.data.frame()
        #6's
        s<- bat %>% group_by(venue,date) %>% 
            summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(ven,f,s)
        ven <- list_df %>% reduce(left_join, by=c('venue','date'))
        mom=bat%>%
            group_by(venue,date,player_of_match)%>%
            filter(player_of_match==b)%>%
            summarise(mom=sum(table(unique(id))),.groups = 'drop')%>%
            as.data.frame()
        
        ven=merge(ven,mom,by=c("venue","date"),all.x = TRUE)
        ven[is.na(ven)]=0
        ven=select(ven,-"player_of_match")
        ven=ven%>%
            arrange(desc(runs))%>%
            head(10)
    }
    inning_wise=function(b){
        bat=bat2[bat2$batsman==b ,]
        df3<- bat %>% group_by(inning,batsman) %>% 
            summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
            as.data.frame()
        
        noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(inning,player_dismissed)%>%summarise(outs=n())
        colnames(noa)[2]="batsman"
        df3=merge(df3,noa,by=c("batsman","inning"))
        df3["average"]=df3$sum_runs/df3$outs
        df3['not_outs']=df3$matches-df3$outs
        f<- bat %>% group_by(batsman,inning) %>% 
            summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
            as.data.frame()
        #6's
        s<- bat %>% group_by(batsman,inning) %>% 
            summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(df3,f,s)
        df3 <- list_df %>% reduce(right_join, by=c('batsman','inning'))
        df3
        ducks= bat %>% group_by(date,inning) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs==0)
        hundred<- bat %>% group_by(date,inning,batsman) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs>=100)
        fifty<- bat %>% group_by(date,inning,batsman) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs>=50&runs<100)
        fifty<-fifty%>%
            group_by(batsman,inning)%>%
            summarise(fifty=n(),.groups = 'drop')
        hundred<-hundred%>%
            group_by(batsman,inning)%>%
            summarise(hundred=n(),.groups = 'drop')
        list_df = list(df3,fifty,hundred)
        df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))
        
        hs=bat%>%
            group_by(date,batsman,inning)%>%
            summarise(runs=sum(batsman_runs),.groups = 'drop')
        hs<-hs%>%
            group_by(inning,batsman)%>%
            summarise(hs=max(runs),.groups = 'drop')
        list_df = list(df3,hs)
        df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))
        
        ducks<- bat %>% group_by(date,inning,batsman) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs==0)
        ducks<-ducks%>%
            group_by(batsman,inning)%>%
            summarise(ducks=n(),.groups = 'drop')
        list_df = list(df3,ducks)
        df3 <- list_df %>% reduce(left_join, by=c('batsman','inning'))
        df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
        mom=bat%>%
            group_by(inning,player_of_match)%>%
            filter(player_of_match==b)%>%
            summarise(mom=sum(table(unique(id))),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(df3,mom)
        df3 <- list_df %>% reduce(left_join, by=c('inning'))
        df3=select(df3,-c("batsman","player_of_match"))
        return(df3)
    }
    season_wise=function(b){
        bat=bat2[bat2$batsman==b ,]
        df3<- bat %>% group_by(season,batsman) %>% 
            summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
            as.data.frame()
        
        noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(season,player_dismissed)%>%summarise(outs=n())
        colnames(noa)[2]="batsman"
        df3=merge(df3,noa,by=c("batsman","season"))
        df3["average"]=df3$sum_runs/df3$outs
        df3['not_outs']=df3$matches-df3$outs
        f<- bat %>% group_by(batsman,season) %>% 
            summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
            as.data.frame()
        #6's
        s<- bat %>% group_by(batsman,season) %>% 
            summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(df3,f,s)
        df3 <- list_df %>% reduce(right_join, by=c('batsman','season'))
        df3
        ducks= bat %>% group_by(date,season) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs==0)
        hundred<- bat %>% group_by(date,season,batsman) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs>=100)
        fifty<- bat %>% group_by(date,season,batsman) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs>=50&runs<100)
        fifty<-fifty%>%
            group_by(batsman,season)%>%
            summarise(fifty=n(),.groups = 'drop')
        hundred<-hundred%>%
            group_by(batsman,season)%>%
            summarise(hundred=n(),.groups = 'drop')
        list_df = list(df3,fifty,hundred)
        df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))
        
        hs=bat%>%
            group_by(date,batsman,season)%>%
            summarise(runs=sum(batsman_runs),.groups = 'drop')
        hs<-hs%>%
            group_by(season,batsman)%>%
            summarise(hs=max(runs),.groups = 'drop')
        list_df = list(df3,hs)
        df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))
        
        ducks<- bat %>% group_by(date,season,batsman) %>% 
            summarise(runs=sum(batsman_runs),.groups = 'drop')%>%
            filter(runs==0)
        ducks<-ducks%>%
            group_by(batsman,season)%>%
            summarise(ducks=n(),.groups = 'drop')
        list_df = list(df3,ducks)
        df3 <- list_df %>% reduce(left_join, by=c('batsman','season'))
        df3["boundary_percent"]=((df3$fours+df3$sixes)/df3$balls_faced)*100
        mom=bat%>%
            group_by(season,player_of_match)%>%
            filter(player_of_match==b)%>%
            summarise(mom=sum(table(unique(id))),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(df3,mom)
        df3 <- list_df %>% reduce(left_join, by=c('season'))
        df3=select(df3,-c("batsman","player_of_match"))
        df3[is.na(df3)]=0
        return(df3)
    }
    high_risk=function(b){
        bat=bat2[bat2$batsman==b ,]
        high<-bat%>%
            group_by(bowler)%>%
            summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
            as.data.frame()
        
        noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(bowler,player_dismissed)%>%summarise(outs=n())
        noa=noa%>%
            filter(player_dismissed==b)
        high=merge(high,noa,by=c("bowler"))
        high["average"]=high$sum_runs/high$outs
        high['not_outs']=high$matches-high$outs
        f<- bat %>% group_by(bowler) %>% 
            summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
            as.data.frame()
        #6's
        s<- bat %>% group_by(bowler) %>% 
            summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(high,f,s)
        high<- list_df %>% reduce(right_join, by=c('bowler'))
        
        high["boundary_percent"]=((high$fours+high$sixes)/high$balls_faced)*100
        
        high=select(high,-c("player_dismissed"))
        
        high[is.na(high)]=0
        high=high%>%
            arrange(desc(outs))%>%
            head(10)
        
    }
    low_risk=function(b){
        bat=bat2[bat2$batsman==b ,]
        low<-bat%>%
            group_by(bowler)%>%
            summarise(matches= sum(table(unique(id))),sum_runs=sum(batsman_runs),sr=sum_runs/n()*100,balls_faced=n(),.groups = 'drop')%>%
            as.data.frame()
        
        noa=bat%>%filter(extras_type!="noballs"|is.na(extras_type))%>%group_by(bowler,player_dismissed)%>%summarise(outs=n())
        noa=noa%>%
            filter(player_dismissed==b )
        low=merge(low,noa,by=c("bowler"))
        low["average"]=low$sum_runs/low$outs
        low['not_outs']=low$matches-low$outs
        f<- bat %>% group_by(bowler) %>% 
            summarise(fours=sum(batsman_runs==4),.groups = 'drop')%>%
            as.data.frame()
        #6's
        s<- bat %>% group_by(bowler) %>% 
            summarise(sixes=sum(batsman_runs==6),.groups = 'drop')%>%
            as.data.frame()
        list_df = list(low,f,s)
        low<- list_df %>% reduce(right_join, by=c('bowler'))
        
        low["boundary_percent"]=((low$fours+low$sixes)/low$balls_faced)*100
        
        low=select(low,-c("player_dismissed"))
        
        low[is.na(low)]=0
        low=low%>%
            arrange(desc(average))%>%
            head(10)
    }
    bowler_season=function(b){
        bowler=ipl[ipl$bowler==b,]
        sr=bowler %>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))%>%
            group_by(season,bowler)%>%
            summarise(runs=sum(total_runs),.groups = 'drop')
        balls=bowler %>%
            filter(extras_type!="wides"|is.na(extras_type))%>%
            filter(extras_type!="noballs"|is.na(extras_type))%>%
            group_by(season,bowler)%>%
            summarise(balls=n(),.groups = 'drop')
        df6<- bowler %>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        df6=df6 %>%
            group_by(season,bowler) %>% 
            summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
            as.data.frame()
        df6=merge(df6,sr,by=c("season","bowler"))
        df6=merge(df6,balls,by=c("season","bowler"))
        df6['sr']=df6$balls/df6$sum_wickets
        df6['avg']=df6$runs/df6$sum_wickets
        df6['eco']=df6$runs/(df6$balls/6)
        hs=bowler%>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        hs=hs%>%
            group_by(season,date)%>%
            summarise(wkts=sum(is_wicket),runs=sum(total_runs),.groups = 'drop')%>%
            arrange(desc(wkts))
        hs=hs%>%
            group_by(season)%>%
            filter(wkts==max(wkts))
        hs=hs%>%
            group_by(season)%>%
            filter(runs==min(runs))%>%
            arrange(season)
        df6['bbf']=paste0(hs$wkts,"/",hs$runs)
        return(df6)
        
    }
    bowling_perf=function(b){
        bowler=ipl[ipl$bowler==b,]
        sr=bowler %>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))%>%
            group_by(bowler)%>%
            summarise(runs=sum(total_runs),.groups = 'drop')
        balls=bowler %>%
            filter(extras_type!="wides"|is.na(extras_type))%>%
            filter(extras_type!="noballs"|is.na(extras_type))%>%
            group_by(bowler)%>%
            summarise(balls=n(),.groups = 'drop')
        df6<- bowler %>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        df6=df6 %>%
            group_by(bowler) %>% 
            summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
            as.data.frame()
        df6=merge(df6,sr,by="bowler")
        df6=merge(df6,balls,by="bowler")
        df6['sr']=df6$balls/df6$sum_wickets
        df6['avg']=df6$runs/df6$sum_wickets
        df6['eco']=df6$runs/(df6$balls/6)
        hs=bowler%>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        hs=hs%>%
            group_by(date)%>%
            summarise(wkts=sum(is_wicket),runs=sum(total_runs),.groups = 'drop')%>%
            arrange(desc(wkts))%>%
            head(1)
        df6['bbf']=paste0(hs$wkts,"/",hs$runs)
        return(df6)
    }
    bowler_inning=function(b){
        bowler=ipl[ipl$bowler==b,]
        sr=bowler %>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))%>%
            group_by(inning,bowler)%>%
            summarise(runs=sum(total_runs),.groups = 'drop')
        balls=bowler %>%
            filter(extras_type!="wides"|is.na(extras_type))%>%
            filter(extras_type!="noballs"|is.na(extras_type))%>%
            group_by(inning,bowler)%>%
            summarise(balls=n(),.groups = 'drop')
        df6<- bowler %>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        df6=df6 %>%
            group_by(inning,bowler) %>% 
            summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
            as.data.frame()
        df6=merge(df6,sr,by=c("inning","bowler"))
        df6=merge(df6,balls,by=c("inning","bowler"))
        df6['sr']=df6$balls/df6$sum_wickets
        df6['avg']=df6$runs/df6$sum_wickets
        df6['eco']=df6$runs/(df6$balls/6)
        hs=bowler%>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        hs=hs%>%
            group_by(inning,date)%>%
            summarise(wkts=sum(is_wicket),runs=sum(total_runs),.groups = 'drop')%>%
            arrange(desc(wkts))
        hs=hs%>%
            group_by(inning)%>%
            filter(wkts==max(wkts))
        hs=hs%>%
            group_by(inning)%>%
            filter(runs==min(runs))%>%
            arrange(inning)
        df6['bbf']=paste0(hs$wkts,"/",hs$runs)
        return(df6)
    }
    danger_batter=function(b){
        bowler=ipl[ipl$bowler==b,]
        sr=bowler %>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))%>%
            group_by(bowler,batsman)%>%
            summarise(runs=sum(total_runs),.groups = 'drop')
        balls=bowler %>%
            filter(extras_type!="wides"|is.na(extras_type))%>%
            filter(extras_type!="noballs"|is.na(extras_type))%>%
            group_by(bowler,batsman)%>%
            summarise(balls=n(),.groups = 'drop')
        df6<- bowler %>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        df6=df6 %>%
            group_by(bowler,batsman) %>% 
            summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
            as.data.frame()
        df6=merge(df6,sr,by=c("bowler","batsman"))
        df6=merge(df6,balls,by=c("bowler","batsman"))
        df6['sr']=df6$balls/df6$sum_wickets
        df6['avg']=df6$runs/df6$sum_wickets
        df6['eco']=df6$runs/(df6$balls/6)
        df6=df6%>%
            filter(matches>=3&balls>10)%>%
            arrange(desc(eco))%>%
            head(10)
        if(dim(df6)[1]!=0){
            return(df6)
        }else{
            return("Donot have atleast 10 balls and 3 matches")
        }
    }
    easy_batter=function(b){
        bowler=ipl[ipl$bowler==b,]
        sr=bowler %>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))%>%
            group_by(bowler,batsman)%>%
            summarise(runs=sum(total_runs),.groups = 'drop')
        balls=bowler %>%
            filter(extras_type!="wides"|is.na(extras_type))%>%
            filter(extras_type!="noballs"|is.na(extras_type))%>%
            group_by(bowler,batsman)%>%
            summarise(balls=n(),.groups = 'drop')
        df6<- bowler %>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        df6=df6 %>%
            group_by(bowler,batsman) %>% 
            summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
            as.data.frame()
        df6=merge(df6,sr,by=c("bowler","batsman"))
        df6=merge(df6,balls,by=c("bowler","batsman"))
        df6['sr']=df6$balls/df6$sum_wickets
        df6['avg'] =df6$runs/df6$sum_wickets
        df6['eco']=df6$runs/(df6$balls/6)
        df6=df6%>%
            filter(matches>=3&balls>10)%>%
            arrange((eco))%>%
            head(10)
        if(dim(df6)[1]!=0){
            return(df6)
        }else{
            return("Donot have atleast 10 balls and 3 matches")
        }
    }
    best_venue=function(b){
        bowler=ipl[ipl$bowler==b,]
        sr=bowler %>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))%>%
            group_by(bowler,date,venue)%>%
            summarise(runs=sum(total_runs),.groups = 'drop')
        balls=bowler %>%
            filter(extras_type!="wides"|is.na(extras_type))%>%
            filter(extras_type!="noballs"|is.na(extras_type))%>%
            group_by(bowler,date,venue)%>%
            summarise(balls=n(),.groups = 'drop')
        df6<- bowler %>%
            filter(dismissal_kind !="run out"|is.na(dismissal_kind))%>%
            filter(dismissal_kind !="retired hurt"|is.na(dismissal_kind))%>%
            filter(dismissal_kind!="obstructing the field"|is.na(dismissal_kind))%>%
            filter(extras_type!="byes"|is.na(extras_type))%>%
            filter(extras_type!="legbyes"|is.na(extras_type))
        df6=df6 %>%
            group_by(bowler,date,venue) %>% 
            summarise(matches= sum(table(unique(id))),sum_wickets=sum(is_wicket),.groups = 'drop')%>%#,sr=sum_runs/n()*100,inn= sum(table(unique(id))),avg=sum_runs/(sum(table(player_dismissed))),.groups = 'drop')%>%
            as.data.frame()
        df6=merge(df6,sr,by=c("bowler","date","venue"))
        df6=merge(df6,balls,by=c("bowler","date","venue"))
        df6['sr']=df6$balls/df6$sum_wickets
        df6['avg']=df6$runs/df6$sum_wickets
        df6['eco']=df6$runs/(df6$balls/6)
        df6=df6%>%
            arrange(desc(sum_wickets))%>%
            head(10)
        return(df6)
    }
    output$top_bat <- renderDataTable(top_bat(input$team,input$inning,input$num))
    output$destructive_bat <- renderDataTable(destructive_bat(input$team2,input$inning2,input$num2))
    output$four=renderDataTable(four_bat(input$team5,input$inning5,input$num5))
    output$six=renderDataTable(six_bat(input$team6,input$inning6,input$num6))
    output$top_bowl <- renderDataTable(top_bowl(input$team3,input$inning3,input$num3))
    output$strike_bowl <- renderDataTable(strike_bowl(input$team4,input$inning4,input$num4))
    output$eco_bowl <- renderDataTable(eco_bowl(input$team7,input$inning7,input$num7))
    output$poor_bowl <- renderDataTable(poor_bowl(input$team8,input$inning8,input$num8))
    output$bat<-renderDataTable(top_bat_season(input$team9))
    output$bowl<-renderDataTable(top_bowl_season(input$team10))
    output$bat_bowl<-renderDataTable(batting_vsteam(input$team11,input$team12))
    output$bowl_bat<-renderDataTable(bowling_vsteam(input$team13,input$team14))
    output$batsman<-renderDataTable(batsman_performance(input$name1))
    output$batsman2<-renderDataTable(position_wise(input$name2))
    output$batsman3<-renderDataTable(venue_wise(input$name3))
    output$batsman4<-renderDataTable(inning_wise(input$name4))
    output$batsman5<-renderDataTable(inning_wise(input$name5))
    output$batsman6<-renderDataTable(high_risk(input$name6))
    output$batsman7<-renderDataTable(low_risk(input$name7))
    output$bowler1<-renderDataTable(bowling_perf(input$name8))
    output$bowler2<-renderDataTable(bowler_season(input$name9))
    output$bowler3<-renderDataTable(bowler_inning(input$name10))
    output$bowler4<-renderDataTable(danger_batter(input$name11))
    output$bowler5<-renderDataTable(easy_batter(input$name12))
    output$bowler6<-renderDataTable(best_venue(input$name13))
}

# Run the application 
shinyApp(ui = ui, server = server)
