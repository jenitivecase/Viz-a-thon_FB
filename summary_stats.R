source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

play_data <- read.csv("Vizathon sample data.csv")
names(play_data)[1] <- gsub("ï..", "", names(play_data)[1])

#things to think about
#play_type
#sacks/QB hits
#fumbles - forced and unforced?
#two-point conversions
#replay or challenges

game_plays <- play_data %>%
  group_by(game_id, home_team, away_team, play_type) %>%
  summarise(play_n = n()) %>%
  spread(key = play_type, value = play_n)

game_data <- play_data %>%
  group_by(game_id, home_team, away_team) %>%
  arrange(desc(NFL_PlayID)) %>%
  summarise(#passing yds
            passing_yds_home = sum(yards_gained[play_type == "pass" & 
                                                  OffensiveTeam == home_team], na.rm = TRUE),
            passing_yds_away = sum(yards_gained[play_type == "pass" & 
                                                  OffensiveTeam == away_team], na.rm = TRUE),
            #rushing yds
            rushing_yds_home = sum(yards_gained[play_type == "run" & 
                                                 OffensiveTeam == home_team], na.rm = TRUE),
            rushing_yds_away = sum(yards_gained[play_type == "run" & 
                                                  OffensiveTeam == away_team], na.rm = TRUE),
            #points for each team
            td_points_home = sum(touchdown[td_team == home_team], na.rm = TRUE) * 7,
            td_points_away = sum(touchdown[td_team == away_team], na.rm = TRUE) * 7,
            extra_points_home = sum(extra_point_attempt[OffensiveTeam == home_team & 
                                                          extra_point_result == "good"], 
                                    na.rm = TRUE),
            extra_points_away = sum(extra_point_attempt[OffensiveTeam == away_team & 
                                                          extra_point_result == "good"], 
                                    na.rm = TRUE),
            FG_points_home = sum(field_goal_attempt[OffensiveTeam == home_team & 
                                                      field_goal_result == "made"], 
                                    na.rm = TRUE) * 3,
            FG_points_away = sum(field_goal_attempt[OffensiveTeam == away_team & 
                                                      field_goal_result == "made"], 
                                    na.rm = TRUE) * 3,
            #punts
            punts_home = sum(punt_attempt[OffensiveTeam == home_team], na.rm = TRUE),
            punts_away = sum(punt_attempt[OffensiveTeam == away_team], na.rm = TRUE)) %>%
  mutate(home_score = sum(td_points_home, extra_points_home, FG_points_home),
         away_score = sum(td_points_away, extra_points_away, FG_points_away))
