library(readr)
library(dplyr)

week_dat <- read_csv('finalWeeklyData.csv')

# inputs
league_size = 12
scoring = 'PPR'
scoring_str <- paste0('^',scoring,'$')


# multiplier
avg_qb = league_size * 1
avg_rb = league_size * 2
avg_wr = league_size * 2.5
avg_te = league_size * 1

szn_dat <- week_dat %>% group_by(Player,Position,Year) %>% 
  select(PPR,HalfPPR,Standard,FourPtTD,SixPtTD) %>% 
  summarize_all(funs(sum(., na.rm = TRUE)))

vbd_pos = function(dat,position,avg_range,scoring){
  scoring_str = paste0('^',scoring,'$')
  pos = dat %>% filter(Position==position) %>% 
    select(Year,matches(scoring_str)) %>% 
    group_by(Year) %>% 
    mutate(rank=rank(-PPR))
  
  avg_rank_pos = pos %>% filter(rank == avg_range) %>% select(Year,matches(scoring_str)) 
  colnames(avg_rank_pos)[2] = "score"
  # calculate VBD now
  
  joins = pos %>% inner_join(avg_rank_pos) %>% 
    mutate(VBD = PPR - score)
  return(joins)
}

qb = vbd_pos(szn_dat,"QB",avg_qb,"PPR")
rb = vbd_pos(szn_dat,"RB",avg_rb,"PPR")
wr = vbd_pos(szn_dat,"WR",avg_wr,"PPR")
te = vbd_pos(szn_dat,"TE",avg_te,"PPR")

total_players = bind_rows(qb,rb,wr,te) %>% select(Player,Position,Year,VBD)


final_dat = year_df %>% inner_join(total_players,by=c("name"="Player","yrs"="Year"))

library(ggplot2)

ggplot(final_dat,aes(x=adp,y=VBD)) + 
  geom_point() +
  theme_bw(base_size = 16) + 
  labs(x= "Draft Pick (ADP)",y="VBD",
       title = "Value Based Drafting",
      subtitle = "2010-2018 (except 2012)")

linreg <- lm(VBD ~ adp, data = final_dat)

adp = 1:180
predict(linreg,newdata = data.frame(adp))
