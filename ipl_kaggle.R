
# Tasks Completed:
#   1) Most Player of matches
#   2) Confidence interval of win margins (at both wicket and runs level)
#   3) Proportion of teams that won both the toss and the match.
#   4)Batsman with most sixes/fours
#   5)Teams with most sixes/fours
#   6)Most Active fielders
#   7)Bowler with most extras
#   8)Players with highest strike rate
#   9)Most expensive overs
#   10)Bowlers with highest wickets
#   11)Batsman who rule during specific overs
#   12)Most time spent on the strike
# 


library(tidyverse)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(viridis)

# 
# Data from:
#   https://www.kaggle.com/manasgarg/ipl

path = "C:/Users/user/Desktop/SDA Proj/deliveries.csv"
del = read.csv(path)

path = "C:/Users/user/Desktop/SDA Proj/matches.csv"
raw = read.csv(path) 


raw = raw[raw$result=="normal",1:ncol(raw)]

refactoring <- function(col) {
  return(fct_collapse(col,
                     "Sunrisers Hyderabad" = c("Sunrisers Hyderabad","Deccan Chargers") ,
                     "Chennai Super Kings" = c("Rising Pune Supergiants","Rising Pune Supergiant", "Chennai Super Kings"),
                     "Rajasthan Royals" = c("Rajasthan Royals", "Gujarat Lions"),
                     "Others" = c("Pune Warriors","Kochi Tuskers Kerala","") #Since these teams were for very less seasons.
  ))
}

theme_set(theme_light())

raw$team1 = refactoring(raw$team1)
raw$team2 = refactoring(raw$team2)
raw$winner = refactoring(raw$winner)
raw$toss_winner = refactoring(raw$toss_winner)

del$batting_team = refactoring(del$batting_team)
del$bowling_team = refactoring(del$bowling_team)


#Most Man of the Matches
raw %>%
  count((player_of_match), sort = T) %>%
  mutate(
    player = fct_reorder(`(player_of_match)`,n)
  ) %>%
  head(20) %>%
  ggplot() +
  geom_col(aes(x = player,y = n, fill = player)) +
  scale_fill_manual(values = rev(viridis_pal(option = "B")(20))) +
  coord_flip() +
  ylim(c(0,20)) +
  labs(x = "Player of the match", y = "Number of times awarded", title = "Awarded as Player of the Match most number of times")


pop_toss = raw %>%
  filter(result == "normal") %>%
 mutate(
   wintoss = ((raw$winner == raw$toss_winner))
 )%>%
  group_by(toss_winner) %>%
  summarise(
    p = mean(wintoss)
  )


samp_toss = raw %>%
  mutate(
    wintoss = ((raw$winner == raw$toss_winner))
  )%>%
  group_by(toss_winner) %>%
  filter(toss_winner!="Others") %>%
  sample_n(35) %>%
  summarise(
    phat = mean(wintoss),
    up = phat + 1.96*(phat*(1-phat)/25)^0.5,
    low = phat - 1.96*(phat*(1-phat)/25)^0.5
  ) 

toss_prop = inner_join(pop_toss,samp_toss)

plot_wintoss = toss_prop %>%
  mutate(
    Team = fct_reorder(toss_winner,p)
  ) %>%
  ggplot(aes(x = Team)) +
  geom_point(aes(y = p, col = Team)) +
  geom_point(aes(y = up)) + 
  geom_point(aes(y = low)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Probability", title = "Does Winning the toss WIN the match?", 
       caption = "Test conducted at 5% level of significance and 35 samples for each team were chosen.", x = "Team",
       subtitle = "Checking if the captain's decision based on the pitch helps the team in winning the match")


pop_byruns = raw %>%
  filter(win_by_runs != 0, winner!="Others") %>%
  group_by(winner) %>%
  summarise(
    m = mean(win_by_runs),
    n()
  )

set.seed(2020)
samp_byruns = raw %>%
  filter(win_by_runs != 0, winner!="Others") %>%
  group_by(winner) %>%
  sample_n(15) %>%
  summarise(
    lower = t.test(win_by_runs)$conf.int[1],
    upper = t.test(win_by_runs)$conf.int[2]
  )

byruns = inner_join(pop_byruns,samp_byruns)  
byruns

plot_byruns = byruns %>%
  mutate(
    Team = fct_reorder(winner,m)
  ) %>%
  ggplot(aes(x = Team)) +
  geom_point(aes(y = m, col = Team, size = `n()`)) +
  geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
  # geom_point(aes(y = upper)) + 
  # geom_point(aes(y = lower)) +
  ylim(0,80) + 
  scale_size(guide = "none") + 
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Average Run Margins", title = "What are the Run Margins by which teams Win their matches?", 
       subtitle = "This plot contains actual average win margins and sample confidence intervals(black). 
This analysis takes into account team's wins only (not all of its matches). The size of the circles indicates number of matches team has actually won.", 
       caption = "Conducted T test at 5% level of significance and 15 samples for each team were chosen.")

pop_bywickets = raw %>%
  filter(win_by_wickets != 0, winner!="Others") %>%
  group_by(winner) %>%
  summarise(
    m = mean(win_by_wickets),
    n()
  )

samp_bywickets = raw %>%
  filter(win_by_wickets != 0, winner!="Others") %>%
  group_by(winner) %>%
  sample_n(15) %>%
  summarise(
    lower = t.test(win_by_wickets)$conf.int[1],
    upper = t.test(win_by_wickets)$conf.int[2]
  )

bywickets = inner_join(pop_bywickets,samp_bywickets)  


plot_bywickets = bywickets %>%
  mutate(
    Team = fct_reorder(winner,m)
  ) %>%
  ggplot(aes(x = Team)) +
  geom_point(aes(y = m, col = Team, size = `n()`)) +
  geom_errorbar(width=.1, aes(ymin=lower, ymax=upper)) +
  ylim(4,9) + 
  scale_size(guide = "none") + 
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Average Wicket Margins", title = "What are the Wicket Margins by which teams Win their matches?", 
       subtitle = "This plot contains actual average win by wicket margins and sample confidence intervals(black). 
This analysis takes into account team's wins only (not all of its matches). The size of the circles indicates number of matches team has actually won.", 
       caption = "Conducted T test at 5% level of significance and 15 samples for each team were chosen.")


#Boundary count (Player wise)
del %>%
  filter(batsman_runs>=4) %>%
  group_by(batsman) %>%
  summarise(
    total = sum(batsman_runs),
    Fours = sum(batsman_runs==4),
    Sixes = sum(batsman_runs==6),
    count = n()
  ) %>%
  arrange(desc(total)) %>%
  head(10) %>%
  pivot_longer(c(Fours,Sixes),"Boundaries",values_to = "Number of Boundaries") %>%
  mutate(
    batsman = fct_reorder(batsman,count)
  ) %>%
  ggplot(aes(x=batsman,y=`Number of Boundaries`, fill = Boundaries)) + 
  geom_col() +
  coord_flip() + 
  scale_fill_manual(values = rev(viridis_pal(option = "B")(3))) +
  xlab("Batsman") +
  labs(title = "Players with Highest Number of Boundaries") 


#Teamwise boundaries
del %>%
  filter(batsman_runs>=4) %>%
  group_by(batting_team) %>%
  summarise(
    Fours = sum(batsman_runs==4),
    Sixes = sum(batsman_runs==6),
    count = n()
      ) %>%
  arrange(desc(count)) %>%
  head(10) %>%
  pivot_longer(c(Fours,Sixes),"Boundaries",values_to = "Number of Boundaries") %>%
  mutate(
    Teams = fct_reorder(batting_team,count)
  ) %>%
  ggplot() + 
  geom_col(aes(x=Teams,y=`Number of Boundaries`, fill = Boundaries)) +
  coord_flip() + 
  scale_fill_manual(values = rev(viridis_pal(option = "A")(5))) +
  labs(title = "Teams with Highest Number of Boundaries") +
  geom_text(aes(x=Teams,y=`Number of Boundaries`, label = `Number of Boundaries`), nudge_y = -150)


#No. of Matches per batsman

del %>%
  select(batsman,match_id) %>%
  unique() %>%
  group_by(batsman) %>%
  count(sort=T) %>%
  head(10) %>%
  ungroup() %>%
  mutate(
    batsman = fct_reorder(batsman,n)
  ) %>%
  ggplot() +
  geom_col(aes(x = batsman, y = n, fill = batsman), show.legend = F) +
  scale_fill_manual(values = rev(viridis_pal(option = "C")(20))) +
  coord_flip() + 
  labs(title = "Batsman with Highest Number of Matches", x = "Batsman", y = "No. of Matches") 


#Most active fielders
del %>%
  select(fielder,dismissal_kind) %>%
  filter(fielder != "") %>%
  group_by(dismissal_kind) %>%
  count(fielder, sort=T) %>%
  top_n(9,n) %>%
  ungroup() %>%
  mutate(
    dismissal_kind = factor(dismissal_kind, 
                                labels = c("Caught in Style!",
                                "Run Out!",
                                "Stunning Stumping!")),
    fielder = fct_reorder(fielder,desc(n))
    ) %>%
  # arrange(dismissal_kind,desc(n)) %>%
  ggplot(aes(fill = dismissal_kind)) +
  geom_col(aes(x=fielder,y=n), position = "dodge", show.legend = F) + 
  scale_fill_manual(values = (viridis_pal(option = "D")(3))) +
  facet_wrap(.~dismissal_kind, scales = "free", ncol = 3) +
  labs(y="Number of Matches", x = "Fielder", title = "Who are the Best fielders?") +
  coord_flip()

#Tried my best to reorder by frequency, but couldn't :( 

#Most time spent on strike
del%>%
  count(batsman, sort=T) %>%
  head(15) %>%
  mutate(
    batsman = fct_reorder(batsman,n)
  ) %>%
  ggplot() +
  geom_col(aes(x=batsman,y=n, fill=batsman)) +
  labs(y="No. of balls played", x = "Batsman", title = "Player's who have spent longest time on the pitch") +
  scale_fill_manual(values = rev(viridis_pal(option = "C")(20)))



#Most balls bowled
del %>%
  count(bowler,sort=T) %>%
  head(10) %>%
  mutate(
    bowler = fct_reorder(bowler,n)
  ) %>%
  ggplot() +
  geom_col(aes(x=bowler,y=n, fill=bowler), show.legend = F) +
  labs(y="No. of balls bowled", x = "Bowler", title = "Most balls bowled") +
  scale_fill_manual(values = rev(viridis_pal(option = "C")(20)))


#Highest wicket taker
del %>%
  select(bowler,dismissal_kind) %>%
  filter(dismissal_kind %in% levels(del$dismissal_kind)[-c(1,7,8,10)]) %>%
  count(bowler, sort=T) %>%
  head(15) %>%
  mutate(
    bowler = fct_reorder(bowler,n)
  ) %>%
  ggplot() +
  geom_col(aes(x=bowler,y=n, fill=bowler)) +
  labs(y="No. of Wickets taken", x = "Bowler", title = "Highest Wicket Taker") +
  scale_fill_manual(values = rev(viridis_pal(option = "C")(20)))


#Most economic bowlers
del %>%
  group_by(bowler) %>%
  summarise(
    runs = sum(total_runs),
    overs = (n()/6),
    eco = (runs/overs)
  ) %>%
  filter(overs>50) %>%
  arrange(eco) %>%
  head(15) %>%
  mutate(
    bowler = fct_reorder(bowler,desc(eco))
  ) %>%
  ggplot() +
  geom_bar(aes(x=bowler,y=eco, fill=bowler), stat = "identity", show.legend = F) +
  labs(y="Bowling Economy", x = "Bowler", title = "Bowlers with Best Bowling Economy", 
       subtitle = "Only bowlers who have bowled more than 50 overs are considered for reliability.") +
  scale_fill_manual(values = rev(viridis_pal(option = "C")(20))) +
  coord_flip() +
  ylim(0,7.5) +
  geom_label(aes(x = bowler, y = eco, label = round(eco,2))) 



#Bowlers with most extras
del %>%
  mutate(
    bowler_extras = noball_runs + wide_runs
  ) %>%
  group_by(bowler) %>%
  summarise(
    sum = sum(bowler_extras),
    count = n()
  ) %>%
  filter(count>50) %>%
  arrange(desc(sum)) %>%
  head(10) %>%
  mutate(
    bowler = fct_reorder(bowler,desc(sum))
  ) %>%
  ggplot(aes(x = bowler, fill = bowler)) +
  geom_col(aes(y = sum), show.legend= F) + 
  geom_text(aes(y=sum,label=count), nudge_y = 5) +
  labs(x="Bowler",y = "Total extras conceeded", title = "Bowlers who gave highest Runs Conceeded in form of extras. 
(Number above the bar shows total number of balls bowled.)")
  



#Batsmen with highest strike rates  
del %>%
  group_by(batsman,over) %>%
  summarise(
    runs = sum(batsman_runs),
    count = n(),
    sr = (runs/count)*100
  ) %>% 
  filter(
    count>100
  ) %>%
  ungroup(batsman) %>%
  group_by(over) %>%
  top_n(1,sr) %>%
  arrange(over) %>%
  ggplot(aes(x=over)) +
  geom_col(aes(y = sr, fill = batsman)) + 
  geom_label(aes(y=(sr),label=paste0(batsman,",",count)), nudge_y = 20) +
  coord_flip() +
  scale_fill_manual(values = brewer.pal(n = 10, name = "Spectral")) +
  ylim(0,300) +
  scale_x_continuous(breaks = 0:20) +
  labs(y = "Batting Strike Rate",x="Over",title = "Top Batsman who rule over specific Overs", subtitle = "Label includes total number of balls played in that over in all of his matches. (Strike rate given below on the axis)", 
       caption = "Only players who have played more than 100 balls of that over are included for reliable results.")


    
  
#Finding a team's highest run scorer
del %>%
  group_by(batsman,batting_team) %>%
  summarise(
    runs = sum(batsman_runs)
  ) %>%
  ungroup(batsman)%>%
  group_by(batting_team) %>%
  arrange(desc(runs)) %>%
  top_n(1,runs) %>%
  ungroup() %>%
  mutate(
    Team = fct_reorder(batting_team,runs)
  ) %>%
  filter(Team!="Others") %>%
  ggplot() +
  geom_col(aes(x=Team, y=runs, fill = batsman), show.legend = F) +
  geom_label(aes(x=Team, y=runs, label=batsman)) +
  labs(y="No. of Runs",title = "Top Run-Scorer of each of the Teams")
  
  
#Finding a team's highest wicket taker

del %>%
  group_by(bowler,bowling_team) %>%
  filter(dismissal_kind %in% levels(del$dismissal_kind)[-c(1,7,8,10)]) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  group_by(bowling_team) %>%
  arrange(desc(count)) %>%
  top_n(1,count) %>%
  ungroup() %>%
  mutate(
    Team = fct_reorder(bowling_team,count)
  ) %>%
  filter(Team!="Others") %>%
  ggplot() +
  geom_col(aes(x=Team, y=count, fill = bowler), show.legend = F) +
  geom_label(aes(x=Team, y=count, label=bowler)) +
  labs(y="No. of Wickets",title = "Top Wicket-takers of each of the Teams")


  

