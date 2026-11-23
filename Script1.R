library(tidyverse)

data <- read.csv("data_25_26.csv")

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
  filter(pos != "GK")

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


cleaned_data <- filtered_data %>%
  rename_with(~ sub("^[a-z]{2}_", "", .x))

variables <- colnames(cleaned_data)

variables

#Eliminate the prefixes related with the source of the concrete statistic obtained

cleaned_data <- cleaned_data %>%
  mutate(
    gls_p90 = gls / playing_time_min * 90,
    ast_p90 = ast / playing_time_min * 90,
    bigChancesCreated_p90 = bigChancesCreated / playing_time_min * 90,
    bigChancesMissed_p90 = bigChancesMissed / playing_time_min * 90,
    accurateOppositionHalfPasses_p90 = accurateOppositionHalfPasses / playing_time_min * 90,
    accurateFinalThirdPasses_p90 = accurateFinalThirdPasses / playing_time_min * 90,
    successfulDribbles_p90 = successfulDribbles / playing_time_min * 90,
    clearances_p90 = clearances / playing_time_min * 90,
    blockedShots_p90 = blockedShots / playing_time_min * 90,
    ballRecovery_p90 = ballRecovery / playing_time_min * 90,
    dribbledPast_p90 = dribbledPast / playing_time_min * 90,
    possessionWonAttThird_p90 = possessionWonAttThird / playing_time_min * 90,
    possessionLost_p90 = possessionLost / playing_time_min * 90,
    touches_p90 = touches / playing_time_min * 90,
    wasFouled_p90 = wasFouled / playing_time_min * 90
  )

cleaned_data <- cleaned_data %>%
  select(-gls, -ast, -bigChancesCreated, -bigChancesMissed,
         -accurateOppositionHalfPasses, -accurateFinalThirdPasses,
         -successfulDribbles, -clearances, -blockedShots,
         -ballRecovery, -dribbledPast, -possessionWonAttThird,
         -possessionLost, -touches, -wasFouled)

# Here we convert cumulative statistics (which takes the aggregate, and not proportional values)
# into per 90 minute values, so that the performances can be compared without having to take into account 
# the minutes played. Hence, after creating the per90 variables, we take away the original ones (avoid redundancy0)

performance_vars <- c(
  "sot_pct", "g_sh", "xg_p90", "p90", "chain_p90",
  "shots_p90", "key_passes_p90", "g_minus_xg", "xg_buildup",
  "accuratePassesPercentage", "accurateLongBallsPercentage",
  "accurateCrossesPercentage", "successfulDribblesPercentage",
  "groundDuelsWonPercentage", "aerialDuelsWonPercentage",
  "totalDuelsWonPercentage", "interceptions_p90",
  "tackles_won_p90", "tacklesWonPercentage", "fouls_p90",
  "gls_p90", "ast_p90", "bigChancesCreated_p90",
  "bigChancesMissed_p90", "accurateOppositionHalfPasses_p90",
  "accurateFinalThirdPasses_p90", "successfulDribbles_p90",
  "clearances_p90", "blockedShots_p90", "ballRecovery_p90",
  "dribbledPast_p90", "possessionWonAttThird_p90",
  "possessionLost_p90", "touches_p90", "wasFouled_p90"
)

cleaned_data <- cleaned_data %>%
  mutate(original_min = playing_time_min) %>%
  group_by(player) %>%
  summarise(
    team = paste(unique(team), collapse = ", "),
    league = paste(unique(league), collapse = ", "),
    pos = last(pos),
    age = first(age),
    playing_time_min = sum(original_min),
    playing_time_mp = sum(playing_time_mp),
    across(
      all_of(performance_vars),
      ~ weighted.mean(.x, w = original_min, na.rm = TRUE)
    ),
    .groups = "drop"
  ) 

#As the dataset takes a whole season, there is the possibility for a player to change clubs during it. 
#Originally, the dataset has a row for each player-club instance, which resulted in some players appearing in 
#two separate rows (for example, Angel Gomes, who played for Marseille and Wolves during the 25/26 season.)
#These players had some of the statistics kept equal for both instances, but some others changed across clubs.
#Therefore, it is necessary to combine those. The statistics are combined by the minutes played at each club.
#For example, if a player has played 1000 minutes in club A, and 2000 in club B, the statistics for club B
#will have double the weight. 
#As for the clubs, both are saved (same with the leagues), in order to keep as much information as possible, 
#and due to the fact that it is not detrimental to our analysis.


cleaned_data = cleaned_data %>% filter(playing_time_min>=900)
#We wanted to filter players with more than 900 minutes so that we can analyse contrasted performance,
#not stats biased by short playing time (a player who has scored a goal in the only game they have played, f.ex.)
#however, we needed to solve the multiple-club dilemma so as not to lose data (had we filtered earlier, 
#a player with 500 minutes in club A and 500 minutes in club B wouldn't be considered, even if the aggregate
#minute count exceeds our threshold of 900).



#Observing NA values
sum(!complete.cases(cleaned_data))
nrow(na.omit(cleaned_data))

cleaned_data %>%
  filter(if_any(everything(), is.na)) %>%
  select(player, team, pos) %>% print(n=73)

players_most_na <- cleaned_data %>%
  mutate(NA_count = rowSums(is.na(across(everything())))) %>%
  arrange(desc(NA_count)) %>% filter(NA_count>0) %>% select(player, team, pos, NA_count)

