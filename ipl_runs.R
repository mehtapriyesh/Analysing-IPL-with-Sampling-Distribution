
# Things done:
#   1) Testing for win rates for each team
#   2) Testing for mean score of each team
#   3) Contingency test on whether ther exists association between scoring high runs in first innings and winning.
#   4) Goodness of Fit on whether scores of all the matches follow a specific distribution.
#   5) Comparing average scores of winning teams and losing teams.


library(tidyverse)
library(scales)
library(ggplot2)
path = "C:/Users/user/Desktop/SDA Proj/IPL_Matches(2008-18).csv"
raw = read.csv(path)

refactoring <- function(col) {
  return(fct_collapse(col,
                      "Sunrisers Hyderabad" = c("Sunrisers Hyderabad","Deccan Chargers") ,
                      "Chennai Super Kings" = c("Rising Pune Supergiant", "Chennai Super Kings"),
                      "Rajasthan Royals" = c("Rajasthan Royals", "Gujarat Lions"),
                      "Others" = c("Pune Warriors India","Kochi Tuskers Kerala") #Since these teams were for very less seasons.
  ))
}

raw$T1 = refactoring(raw$T1)
raw$T2 = refactoring(raw$T2)



#Converting data to usable format
ipl = raw %>%
  mutate(
    winner = if_else(score_1>score_2,T1,T2),
    win_score = apply(raw[1:nrow(raw),4:5],1,max),
    lose_score = apply(raw[1:nrow(raw),4:5],1,min),
    loser = if_else(score_1<score_2,T1,T2),
    opening_won = if_else((win_score - lose_score)>5,T,F), #Since we dont have access to which team actually won, we will be using difference data.If difference is less than 5, we can say that team that won batted second. There might be cases where teams won by less than 5 runs but let's ignore that for understanding purposes.
    opening_s = if_else(opening_won, win_score, lose_score),
    bins = cut(opening_s, breaks=c(0,120,155,190,300), labels=c("<120","120-155","155-190","190+")),
    good_bins = cut(win_score, breaks=c(0,120,150,175,200,300), labels=c("<120","120-150","150-175","175-200","200+")) #used for goodness of fit
  )

colnames(ipl)[1] <- "id" #Setting a unique id for each match. Will play a major role in the analysis.

#Calculating Average scores of winning teams and losing teams.
avg_runs = ipl %>%
  group_by(year) %>%
  summarise(
    avg_l = mean(lose_score),
    avg_w = mean(win_score)
  )

plot_wl = ggplot(data = avg_runs,aes(x=year)) + 
  geom_point(aes(y=avg_l, col = "Losing")) + 
  geom_point(aes(y=avg_w, col = "Winning")) +
  geom_line(aes(y = avg_w, group = 1, col = "Winning"), lty = 2) +
  geom_line(aes(y = avg_l, group = 1, col = "Losing"), lty = 2) +
  xlim(2006,2020) +
  ylim(120,200) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_continuous("year", labels = as.character(avg_runs$year), breaks = avg_runs$year) +
  labs(y = "Average Scores of Winning and Losing teams", title = "Comparing trend of average scores of winning and losing teams from 2008-2018")

matches = pivot_longer(ipl, cols = c(winner,loser), names_to = "res", values_to = "Team") 

pop_p = matches %>%
  group_by(Team) %>%
  summarize(
    n = n(),
    p = mean(res=="winner")
  )


set.seed(2020)
sample_p = matches %>%
  group_by(Team) %>%
  filter(Team != "Others") %>%
  sample_n(35) %>%
  summarize(
    n = n(),
    phat = mean(res=="winner"),
    var = phat*(1-phat),
    upper = phat + sqrt(var/n)*1.96,
    lower = phat - sqrt(var/n)*1.96
  )


team_prop = inner_join(pop_p,sample_p, by = "Team")


plot_prop = team_prop %>%
  mutate(
    Team = fct_reorder(Team,p)
  ) %>%
  ggplot(aes(x = Team)) +
  geom_point(aes(y = p, col = Team)) +
  geom_point(aes(y = upper)) + 
  geom_point(aes(y = lower)) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "Winning Probability of all the Teams", title = "Comparing population winning % with sample confidence interval", 
       subtitle = "Test conducted at 5% level of significance and 35 samples for each team were chosen.")

set.seed(2020)

sample = ipl %>%
  sample_n(200)

contingency_s = sample%>%
  select(bins,opening_won) %>%
  group_by(bins) %>%
  count(opening_won) %>%
  pivot_wider(values_from = n, names_from = opening_won)

contingency_s[is.na(contingency_s)] = 0
contingency = chisq.test(contingency_s[,2:3])

#Getting a p value of less than 0.05, rejecting the null hypothesis. There exists association between runs scored batting first and winning.


df1 = raw[,c(2,4)]
df2 = raw[,c(3,5)]
colnames(df2) = colnames(df1)

allscores = rbind(df1,df2)


pop_m = allscores %>%
  group_by(T1) %>%
  summarise(
    mean = mean(score_1)
  )

set.seed(2020)
samp_m = allscores %>%
  group_by(T1) %>%
  sample_n(35) %>%
  summarise(
    lower = t.test(score_1)$conf.int[1],
    upper = t.test(score_1)$conf.int[2]
  )


team_mean = inner_join(pop_m,samp_m)

plot_score = team_mean %>%
  mutate(
    Team = fct_reorder(T1,mean)
  ) %>%
  ggplot(aes(x = Team)) +
  geom_point(aes(y = mean, col = Team)) +
  geom_point(aes(y = upper)) + 
  geom_point(aes(y = lower)) + 
  lims(y = c(120,190)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(y = "Average Score / Game of all the teams", title = "Comparing population mean scores of each team with sample confidence interval", 
       subtitle = "Conducted T test at 5% level of significance and 35 samples for each team were chosen.")



set.seed(2020)
good_sample = sample(ipl$win_score,150)

mu = mean(good_sample)
var = var(good_sample)

#Using the intervals we use to make goodbins
x1 = c(120,150,175,200,300)
x2 = c(0,120,150,175,200)

fit1 = (ppois(x1,mu) - ppois(x2,mu))*nrow(ipl)
fit2 = (pnorm(x1,mu,var^0.5) - pnorm(x2,mu,var^0.5))*nrow(ipl)

obs = cbind(fct_count(ipl$good_bins),fit1,fit2)

sum((obs[2]-obs[3])^2/obs[3])
sum((obs[2]-obs[4])^2/obs[4])

plot_good = ggplot(data = obs, aes(x=f)) +
  geom_col(aes(y=n)) +
  geom_line(aes(y = fit1, group = 1, col = "Poisson"), lty = 2) +
  geom_line(aes(y = fit2, group = 1, col = "Normal"), lty = 2) +
  geom_point(aes(y = fit1, group = 1, col = "Poisson")) +
  geom_point(aes(y = fit2, group = 1, col = "Normal")) +
  labs(x="Score Group", y = "Number of Matches", title = "Checking fit of distributions on the population data using parameters calculated from a sample of 150 values")

qqnorm(raw$score_1)
qqline(raw$score_1, col = "red", lwd = 2)

# plot_good
# plot_score
# plot_prop
# plot_wl
# contingency


