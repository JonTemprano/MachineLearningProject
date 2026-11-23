library(tidyverse)

data <- read.csv("data.csv")

filtered_data = data %>%
  filter(league %in% c("FRA-Ligue 1", "ESP-La Liga", "ENG-Premier League")) %>% 
  
  select(-contains("fifa")) %>%
  select(player, team, league, pos, age,
  playing_time_min, playing_time_mp,
  fb_gls, fb_ast, fb_sot_pct, fb_g_sh,
  np_xg_p90, xa_p90, xg_chain_p90, shots_p90,
  key_passes_p90, np_g_minus_xg,
  ss_bigChancesCreated, ss_bigChancesMissed, us_xg_buildup,
  ss_accuratePassesPercentage, ss_accurateOppositionHalfPasses,
  ss_accurateFinalThirdPasses, ss_accurateLongBallsPercentage,
  ss_accurateCrossesPercentage,
  ss_successfulDribbles, ss_successfulDribblesPercentage,
  ss_groundDuelsWonPercentage, ss_aerialDuelsWonPercentage,
  ss_totalDuelsWonPercentage,
  interceptions_p90, tackles_won_p90, ss_tacklesWonPercentage,
  ss_clearances, ss_blockedShots, ss_ballRecovery, ss_dribbledPast,
  ss_possessionWonAttThird, ss_possessionLost, ss_touches,
  ss_wasFouled, fouls_p90
  ) %>%
  
  filter(playing_time_min>=900 & pos != "GK")

#filter just 3 out of top 5 leagues, so that we can reduce amount of instances.
#these give us enough variation in players and styles, and we have a dataset with a manageable size.
#we select just the variables related to the actual identification and performance in real life, and 
#take away the variables related to the FIFA video game, considering that they do not contribute to our analysis


#For this specific analysis, we will only take into account outfield players (Goalkeepers not included), therefore,
#the variables selected will be related to outfield aspects, excluding variables such as saves or goals prevented.
#Similarly, we do not want data or insights to be altered by players with low or no contribution to its team. 
#Therefore, we remove those who have less than 900 minutes played during the season.

#Possible ways to direct this analysis:
# Classification:
#   - Take into account certain variables, understand how they work / influence and try to predict positions of
#     the players (train/test split, understanding which have more importance than the other ones, f. ex.)
# 
# Clustering:
#   - Understanding different profiles/playstyles of players based on the stats. Does 'el ojímetro' or what's seen
#     by the human eye (scouts, coaches, GMs...) match what the data shows?
#   - Up to which point do these identified profiles match the traditional football positions?
#   - Calculate no. of clusters based on elbow method.
#   - Or use hierarchical and compare both methods.

# PCA:
#   - Create components to identify which variables/dimensions explain differences among players.
#   - Representing players in 2D graphs, based on the components.
#   - Analyse possible patterns related to positions.


#pasar estadísticas acumuladas a _p90.

cleaned_data <- filtered_data %>%
  rename_with(~ sub("^[a-z]{2}_", "", .x))

variables <- colnames(cleaned_data)

variables

#Eliminate the prefixes related with the source of the concrete statistic obtained

