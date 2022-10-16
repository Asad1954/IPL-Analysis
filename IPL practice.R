#read the file 
#data sets

delivery_df <- read.csv("deliveries.csv", stringsAsFactors = FALSE)
matches_df <- read.csv("matches.csv" , stringsAsFactors = FALSE)

unique(matches_df$toss_decision)
unique(matches_df$win_by_wickets)

#Package

install.packages("tidyverse")
install.packages(DT)
library("tidyverse")

# Total Number of matches
matches_df %>% 
  summarise(total_matches = n())


------------------------------------------------------

# which team has won most matches

 most_won_matches = 
  matches_df %>% 
  group_by(winner) %>% 
  summarise(total_win = n()) %>% 
  arrange(desc(total_win))
datatable( most_won_matches)  

view(most_won_matches)
write.table(most_won_matches, file = "most won matches", sep = ",")
# Plot a graph

  matches_df %>% 
  group_by(winner) %>% 
  summarize(total_win = n()) %>%
  arrange(desc(total_win)) %>% 
  ggplot(aes(x=winner, y=total_win, fill=winner)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous("Total Matches Won")


------------------------------------------------------
    
# Top 10 Player with most Man of the match awards
    most_man_of_the_matches =
    matches_df %>% 
    group_by(player_of_match) %>%   
    filter(season == 2019) %>% 
    summarise(man_of_the_match = n()) %>% 
    top_n(10) 
    datatable(most_man_of_the_matches)
    
    write.table(most_man_of_the_matches, file = "most man of the maches", sep = ",")
  
    ggplot(aes(x= reorder(player_of_match, man_of_the_match), y= man_of_the_match)) +
    geom_bar(stat = "identity", fill= "orange") +
    coord_flip() +
    xlab("Players") +
    ggtitle("Player with most Man of the Match ")+
    geom_text(aes(label= man_of_the_match), hjust= 1.25)

  
  ------------------------------------------------------
    
  
#In most matches, the team wins by batting first. (TOP 10) 

most_win_by_runs=matches %>%
  filter(win_by_runs!=0)%>%
  group_by(winner) %>%
  dplyr::summarise(Total_win=n()) %>%
  arrange(desc(Total_win))%>%
  top_n(10)
datatable(most_win_by_runs)  
write.table(most_win_by_runs, file = "most win by run", sep = ",")

ggplot(most_win_by_runs,aes(reorder(winner,Total_win),Total_win,fill=winner,label=Total_win))+
  geom_bar(stat = "identity", width = 0.5, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = most_win_by_runs[order(most_win_by_runs$Total_win),]$winner) +
  theme_classic()+
  geom_text(aes(winner,Total_win, label = Total_win))+
  labs(x = "Team", y = "Win", title = "Highest match win by run", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 14, face = "bold"),title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")


------------------------------------------------------
  

#Teams winning by highest margin of runs

highest_win_margin = matches %>%
  select(winner, season,win_by_runs) %>%
  arrange(desc(win_by_runs)) %>%
  top_n(10) %>%
  ungroup() %>% 
  mutate(Rank = as.factor(1:10))
datatable(highest_win_margin)

ggplot(highest_win_margin)+
  aes(reorder(Rank, win_by_runs), win_by_runs,fill=winner,label=win_by_runs)+
  geom_bar(stat = "identity", width = 0.3, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = highest_win_margin[order(highest_win_margin$win_by_runs),]$winner) +
  theme_classic()+
  geom_text(aes(Rank, win_by_runs, label = win_by_runs))+
  labs(x = "", y = "", title = "Highest win by runs", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")



------------------------------------------------------
  


#In most matches, the team wins by highest wickets.
most_win_by_Wickets=matches %>%
  filter(win_by_wickets!=0)%>%
  group_by(winner) %>%
  dplyr::summarise(Total_win=n()) %>%
  arrange(desc(Total_win))%>%
  top_n(10)
datatable(most_win_by_Wickets)  

ggplot(most_win_by_Wickets,aes(reorder(winner,Total_win),Total_win,fill=winner))+
  geom_bar(stat = "identity", width = 0.5, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = most_win_by_Wickets[order(most_win_by_Wickets$Total_win),]$winner) +
  theme_classic()+
  geom_text(aes(winner,Total_win, label = Total_win))+
  labs(x = "Team", y = "Win", title = "Highest match win by run", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")


------------------------------------------------------
  


# How many teams won matches after winning the toss

matches_df$toss_matches =
  ifelse(matches_df$toss_winner == matches_df$winner, "Yes", "No")  
matches_df %>% 
  filter(toss_matches == "Yes") %>% 
  group_by(winner) %>% 
  summarise(Win_after_winning_match = n()) %>% 
  arrange(desc(Win_after_winning_match)) %>% 
  ggplot(aes(x=winner, y=Win_after_winning_match, fill=winner)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous("Total Matches Won")


------------------------------------------------------


#Most successful batsman runs scored(TOP 10)
batsman_total_run=deliveries%>%
  group_by(batsman)%>%
  dplyr::summarise(Run=sum(batsman_runs))%>%
  arrange(desc(Run))%>%
  top_n(10)
datatable(batsman_total_run)

ggplot(batsman_total_run)+
  aes(reorder(batsman,Run), Run,fill=batsman)+
  geom_bar(stat = "identity", width = 0.3, alpha = 0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = batsman_total_run[order(batsman_total_run$Run),]$batsman) +
  theme_classic()+
  geom_text(aes(batsman, Run, label = Run))+
  labs(x = "Batsman Name", y = "", title = "Highest Runs by batsman in IPL", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")


------------------------------------------------------
  


# Which batsman score more sixes in the tournament
delivery_df %>% 
  group_by(batsman) %>% 
  filter(batsman_runs == 6) %>% 
  summarize(Sixes = n()) %>% 
  top_n(30) %>% 
  ggplot(aes(x= reorder(batsman, Sixes), y= Sixes)) +
  geom_bar(stat = "identity", fill="green") +
  coord_flip() +
  xlab("Players") +
  ggtitle("Top Batsman score most of the Sixes") +
  geom_text(aes(label= Sixes), hjust= 1.25)



------------------------------------------------------
  


#Batsman with Top Strike Rate(min.1000 Run faced)

Batsman_with_Top_Strike= deliveries %>%
  group_by(batsman)%>%
  filter(length(batsman_runs)>1000)%>%
  summarise(strikerate=mean(batsman_runs)*100)%>%
  arrange(desc(strikerate))%>%
  head(10)%>%
  ungroup()%>%
  mutate(Rank=as.factor(1:10))


datatable(Batsman_with_Top_Strike)

ggplot(Batsman_with_Top_Strike)+
  aes(reorder(Rank,strikerate), strikerate,fill=batsman)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Batsman_with_Top_Strike[order(Batsman_with_Top_Strike$strikerate),]$batsman) +
  theme_classic()+
  geom_text(aes(Rank,strikerate,label = strikerate))+
  labs(x = "Batsman Name", y = "Strike Rate", title = "Batsman with Top Strike Rate in IPL", subtitle = "")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")


------------------------------------------------------
  


#Highest Wicket Taker of a team

Highest_Wicket_Taker_of_a_team=deliveries %>%
  filter(dismissal_kind != "run out",player_dismissed !="") %>%
  group_by(bowling_team, bowler) %>% 
  summarise(Total_wickets = sum(table(dismissal_kind))) %>%
  group_by(bowling_team) %>% 
  slice(which.max(Total_wickets))%>%
  arrange(desc(Total_wickets)) %>% 
  head(10)

datatable(Highest_Wicket_Taker_of_a_team)

ggplot(Highest_Wicket_Taker_of_a_team)+
  aes(reorder(bowler,Total_wickets), Total_wickets,fill=bowling_team)+
  geom_bar(stat = "identity", width = 0.5,alpha=0.5)+
  geom_point(size=9, color="black",alpha=0.07)+
  scale_x_discrete(labels = Highest_Wicket_Taker_of_a_team[order(Highest_Wicket_Taker_of_a_team$Total_wickets),]$bowler) +
  theme_classic()+
  geom_text(aes(bowler,Total_wickets,label = Total_wickets))+
  labs(x = "Batsman Name", y = "", title = "Most successful bowler wickets taken in IPL")+
  coord_flip()+
  theme(axis.text = element_text(size = 12, face = "bold"), title  =  element_text(size = 16))+
  theme(legend.title = element_text(size=10),legend.text = element_text(size=10),legend.position="bottom")