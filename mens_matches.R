library(dplyr)
library(tidyr)
library(ggplot2)

getwd()
results = read.csv(file = './tennis/wta_matches_2019.csv', header = TRUE)
head(results)

matchstats = read.csv(file = './tennis/charting-m-stats-Overview.csv')
head(matchstats)

head(matchstats)

############ REFORMATTING MATCHSTATS DATASET
matchstats = matchstats %>% 
  separate(., col = match_id,
           into = c('match_date',
                    'mens',
                    'tourney_name',
                    'round',
                    'player_1',
                    'player_2'),
           sep = '-') %>% 
  filter(., set == 'Total')

########### improving format of players and tournament names
########### only include year
matchstats$player_1 =
  sapply(matchstats$player_1, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$player_2 =
  sapply(matchstats$player_2, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$tourney_name =
  sapply(matchstats$tourney_name, function(x) {
    gsub(x, pattern = '_', replacement = ' ')
  })

matchstats$tourney_name = 
  sapply(matchstats$tourney_name, tolower)

matchstats$year = sapply(matchstats$match_date, function(x) {
  substr(x, 1, 4)
})


player1stats = matchstats %>% 
  filter(., player == 1) %>% 
  select(year, tourney_name,
         player_1, player_2,
         player_1_winners = winners,
         player_1_unforced = unforced)
head(player1stats)

player2stats = matchstats %>% 
  filter(., player == 2) %>% 
  select(year, tourney_name,
         player_1, player_2,
         player_2_winners = winners,
         player_2_unforced = unforced)
head(player2stats)

playerstats

playerstats =
  inner_join(player1stats, player2stats, by = c('year',
                                                'tourney_name',
                                                'player_1',
                                                'player_2'))
head(playerstats)




#########################################
######## FUNCTION TO JOIN RESULTS OF DESIRED YEAR WITH MATCHSTATS

stats_by_year = function(x) {
  
  x$tourney_name = 
    sapply(x$tourney_name, tolower)
  
  x$year = sapply(x$tourney_date, function(x) {
    substr(x, 1, 4)
  })
  
  df1 = inner_join(x,
                   playerstats,
                   by = c('year',
                          'tourney_name',
                          'winner_name' = 'player_1',
                          'loser_name' = 'player_2')) %>% 
    select(year,
           tourney_name,
           winner_name,
           loser_name,
           winner_winners = player_1_winners,
           winner_unforced = player_1_unforced,
           loser_winners = player_2_winners,
           loser_unforced = player_2_unforced)
  
  df2 = inner_join(x,
                   playerstats,
                   by = c('year',
                          'tourney_name',
                          'winner_name' = 'player_2',
                          'loser_name' = 'player_1')) %>% 
    select(year,
           tourney_name,
           winner_name,
           loser_name,
           winner_winners = player_2_winners,
           winner_unforced = player_2_unforced,
           loser_winners = player_1_winners,
           loser_unforced = player_1_unforced)
  
  df = bind_rows(df1, df2)
  
  return(df)
}

results2018 = read.csv(file = './tennis/wta_matches_2018.csv', header = TRUE)
results2018 = stats_by_year(results2018)

results2017 = read.csv(file = './tennis/wta_matches_2017.csv', header = TRUE)
results2017 = stats_by_year(results2017)
results2017

results2020 = stats_by_year(read.csv(file = './tennis/atp_matches_2020.csv', header = TRUE))
results2019 = stats_by_year(read.csv(file = './tennis/atp_matches_2019.csv', header = TRUE))
results2018 = stats_by_year(read.csv(file = './tennis/atp_matches_2018.csv', header = TRUE))
results2017 = stats_by_year(read.csv(file = './tennis/atp_matches_2017.csv', header = TRUE))
results2016 = stats_by_year(read.csv(file = './tennis/atp_matches_2016.csv', header = TRUE))
results2015 = stats_by_year(read.csv(file = './tennis/atp_matches_2015.csv', header = TRUE))
results2014 = stats_by_year(read.csv(file = './tennis/atp_matches_2014.csv', header = TRUE))
results2013 = stats_by_year(read.csv(file = './tennis/atp_matches_2013.csv', header = TRUE))
results2012 = stats_by_year(read.csv(file = './tennis/atp_matches_2012.csv', header = TRUE))
results2011 = stats_by_year(read.csv(file = './tennis/atp_matches_2011.csv', header = TRUE))
results2010 = stats_by_year(read.csv(file = './tennis/atp_matches_2010.csv', header = TRUE))

lastdecade_WL = bind_rows(results2020, results2019,
                          results2018, results2017,
                          results2016, results2015,
                          results2014, results2013,
                          results2012, results2011,
                          results2010)
lastdecade_WL

W_2019














ggplot(data = lastdecade_WL,
       aes(x = loser_winners, y = loser_unforced)) + 
  geom_point()

lastdecade_W = lastdecade_WL %>% 
  summarise(., name = winner_name,
            winners = winner_winners,
            unforced = winner_unforced)

lastdecade_L = lastdecade_WL %>% 
  summarise(., name = loser_name,
            winners = loser_winners,
            unforced = loser_unforced)

lastdecade_nonWL = bind_rows(lastdecade_W, lastdecade_L)
head(lastdecade_W)
lastdecade_nonWL

means2010s = lastdecade_nonWL %>%
  summarise(., mean(winners), mean(unforced))
means2010s
meanwinners2010s = means2010s[1,1]
meanunforced2010s = means2010s[1,2]

head(lastdecade_nonWL)  

player_means2010s = lastdecade_nonWL %>% 
  group_by(., name) %>%
  summarise(., mean_W = mean(winners), mean_U = mean(unforced))

lastdecade_WL

player_means2010s$player_style = 
  case_when(player_means2010s$mean_U >= meanunforced2010s &
              player_means2010s$mean_W >= meanwinners2010s ~ 'HRI_HRE',
            player_means2010s$mean_U >= meanunforced2010s &
              player_means2010s$mean_W < meanwinners2010s ~ 'HRI_LRE',
            player_means2010s$mean_U < meanunforced2010s &
              player_means2010s$mean_W >= meanwinners2010s ~ 'LRI_HRE',
            player_means2010s$mean_U < meanunforced2010s &
              player_means2010s$mean_W < meanwinners2010s ~ 'LRI_LRE')

player_means2010s

styles = player_means2010s %>% 
  select(name, player_style)
styles

styles %>% 
  filter(., name == 'Marin Cilic')

styles %>% 
  group_by(., player_style) %>% 
  summarise(., n())

#---------------------

lastdecade_w_styles =  inner_join(lastdecade_WL, styles, by = c('winner_name' = 'name')) %>% 
  rename(., winner_style = player_style) %>% 
  inner_join(., styles, by = c('loser_name' = 'name')) %>% 
  rename(., loser_style = player_style)

lastdecade_grouped = lastdecade_w_styles %>% 
  group_by(., winner_style, loser_style) %>% 
  summarise(., wins = n()) %>% 
  filter(., winner_style != loser_style)

win_percent = function(x, y) {
  res = c()
  for (i in 1:12) {
    for (j in 1:12) {
      if (x[i] == y[j] & x[j] == y[i]) {
        res[i] = lastdecade_grouped$wins[i]/(lastdecade_grouped$wins[i] + lastdecade_grouped$wins[j])
      }
    }
  }
  return(res)
}

lastdecade_grouped$win_percent =
  win_percent(lastdecade_grouped$winner_style, lastdecade_grouped$loser_style)

lastdecade_grouped %>%
  ggplot(aes(x = winner_style, y = win_percent)) +
  geom_col(aes(fill = loser_style), position = 'dodge') +
  scale_fill_brewer(palette = 'BrBG')


samestyles = lastdecade_w_styles %>% 
  filter(., winner_style == loser_style) %>% 
  select(., -winner_style, style = loser_style)

wins_against_same = samestyles %>%
  group_by(., style, winner_name) %>% 
  summarise(., wins = n())

losses_to_same = samestyles %>%
  group_by(., style, loser_name) %>% 
  summarise(., losses = n())

WL_vs_same = full_join(wins_against_same,
                       losses_to_same,
                       by = c('winner_name' = 'loser_name')) %>% 
  select(., name = winner_name,
         style.x,
         style.y,
         wins,
         losses)
WL_vs_same
WL_vs_same$style =
  case_when(is.na(WL_vs_same$style.x) == TRUE ~ WL_vs_same$style.y,
            is.na(WL_vs_same$style.y) == TRUE ~ WL_vs_same$style.x,
            WL_vs_same$style.x == WL_vs_same$style.y ~ WL_vs_same$style.x)

WL_vs_same = WL_vs_same %>% 
  select(., name, style, wins, losses)

WL_vs_same[is.na(WL_vs_same)] = 0

WL_vs_same %>%
  filter(., wins + losses >= 2) %>% 
  arrange(., losses) %>% 
  mutate(., win_percent = wins/(losses+wins)) %>% 
  group_by(style) %>% 
  arrange(., desc(win_percent))
