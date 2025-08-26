library(nflreadr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggtext)
library(espnscrapeR)
library(nflplotR)
library(shadowtext)
library(ggchicklet)
library(prismatic)
library(paletteer)
library(hrbrthemes)
library(gt)
library(gtExtras)
# Using load_participation -> returns every player ID number for
# play by play data
participation <- nflreadr::load_participation(2022, include_pbp = TRUE)
rosters <- nflreadr::load_rosters(2022) %>%
  select(full_name, gsis_id, depth_chart_position)

oline_participation <- participation %>%
  filter(play_type %in% c("pass", "run")) %>%
  group_by(nflverse_game_id, possession_team, fixed_drive) %>%
  filter(fixed_drive == 1 | fixed_drive == 2) %>%
  filter(row_number() == 1) %>%
  select(
    nflverse_game_id, play_id, possession_team,
    offense_personnel, offense_players
  ) %>%
  dplyr::mutate(gsis_id = stringr::str_split(offense_players, ";")) %>%
  tidyr::unnest(c(gsis_id)) %>%
  left_join(rosters, by = c("gsis_id" = "gsis_id"))

oline_participation <- oline_participation %>%
  filter(depth_chart_position %in% c("T", "G", "C")) %>%
  group_by(nflverse_game_id, possession_team) %>%
  mutate(starting_line = toString(full_name)) %>%
  select(
    nflverse_game_id, possession_team,
    offense_personnel, starting_line
  ) %>%
  distinct()

oline_participation %>%
  group_by(offense_personnel) %>%
  summarize(total = n())

# Using load_snap_counts() -> returns percentage of snaps each plalyer
# was on the field for.
oline_snap_counts <- nflreadr::load_snap_counts(seasons = 2022)

oline_snap_counts <- oline_snap_counts %>%
  select(game_id, week, player, position, team, offense_pct) %>%
  filter(position %in% c("T", "G", "C")) %>%
  group_by(game_id, team) %>%
  arrange(-offense_pct) %>%
  dplyr::slice(1:5) %>%
  ungroup()
# View the data
oline_snap_counts

# get starting five using snap counts
oline_snap_counts <- oline_snap_counts %>%
  group_by(game_id, team) %>%
  arrange(player, .by_group = TRUE)

oline_final_data <- oline_snap_counts %>%
  group_by(game_id, week, team) %>%
  mutate(starting_line = toString(player)) %>%
  select(game_id, week, team, starting_line) %>%
  distinct(game_id, .keep_all = TRUE)

# Get total number of unique offesnive line combonations
total_combos <- oline_final_data %>%
  group_by(team) %>%
  summarize(combos = n_distinct(starting_line)) %>%
  arrange(-combos)

# get win pct from espnscrapeR and combine with total_combos
records <- espnscrapeR::get_nfl_standings(season = 2024) %>%
  select(team_abb, win_pct)
records$team_abb <- nflreadr::clean_team_abbrs(records$team_abb)
total_combos <- total_combos %>%
  left_join(records, by = c("team" = "team_abb"))
# Visualize the combos
nfl_analytics_theme <- function(..., base_size = 12) {
  
    theme(
      text = element_text(family = "Roboto", size = base_size),
      axis.ticks = element_blank(),
      axis.title = element_text(color = "black",
                                face = "bold"),
      axis.text = element_text(color = "black",
                               face = "bold"),
      plot.title.position = "plot",
      plot.title = element_text(size = 16,
                                face = "bold",
                                color = "black",
                                vjust = .02,
                                hjust = 0.5),
      plot.subtitle = element_text(color = "black",
                                   hjust = 0.5),
      plot.caption = element_text(size = 8,
                                  face = "italic",
                                  color = "black"),
      panel.grid.minor = element_blank(),
      panel.grid.major =  element_line(color = "#d0d0d0"),
      panel.background = element_rect(fill = "floralwhite"),
      plot.background = element_rect(fill = "floralwhite"),
      panel.border = element_blank())
}

ggplot(data = total_combos, aes(x = combos, y = win_pct)) +
  geom_line(
    stat = "smooth", method = "lm",
    linewidth = 0.7, color = "blue",
    alpha = 0.25
  ) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = win_pct),
    color = "black", size = 0.8
  ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  nfl_analytics_theme() +
  scale_x_reverse(breaks = scales::breaks_pretty(n = 12)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    labels = scales::label_number(accuracy = 0.001)
  ) +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Win Percentage") +
  labs(
    title = "**Unique Offensive Line Combinations vs. Win Percentage**",
    subtitle = "*Through Week 18",
    caption = "@apwell_analytics"
  )

# Exploring explosive plays per touchdown drive
pbp <- nflreadr::load_pbp(2024)
# explosive <- pbp %>%
#   filter(!is.na(posteam) & !is.na(yards_gained) &
#     fixed_drive_result == "Touchdown") %>%
#   filter(special == 0 & fumble == 0 & interception == 0) %>%
#   group_by(posteam, game_id, drive) %>%
#   summarize(max_yards = max(yards_gained)) %>%
#   mutate(explosive_play = if_else(max_yards >= 20, 1, 0)) %>%
#   ungroup() %>%
#   group_by(posteam) %>%
#   summarize(
#     tds_no_explosive = sum(explosive_play == 0),
#     tds_explosive = sum(explosive_play == 1),
#     total_drives = sum(tds_no_explosive + tds_explosive),
#     percent_no_exp = tds_no_explosive / total_drives,
#     percent_w_exp = tds_explosive / total_drives
#   ) %>%
#   select(posteam, percent_w_exp, percent_no_exp)

# ggplot(explosive, aes(y = reorder(posteam, percent_w_exp), x = percent_w_exp)) +
#   geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
#   nflplotR::scale_color_nfl(type = "secondary") +
#   nflplotR::scale_fill_nfl(alpha = 0.5) +
#   nfl_analytics_theme() +
#   theme(axis.text.y = element_nfl_logo(size = 0.65)) +
#   scale_x_continuous(
#     expand = c(0, 0),
#     breaks = scales::breaks_pretty(n = 5),
#     label = scales::percent_format()
#   ) +
#   labs(
#     title = "**Highest % of Explosive Plays per TD Drive**",
#     subtitle = "*2024 Season*",
#     caption = "@apwell_analytics"
#   ) +
#   xlab("Percent of TD Drives with an Explosive Play (20+ Yards)") +
#   ylab("")

# show how often teams get into the redzone on offense
redzone_trips <- pbp %>%
  filter(!is.na(posteam) & !is.na(drive_inside20) & season_type == "REG") %>%
  filter(special == 0, fumble == 0, interception == 0) %>%
  group_by(posteam, game_id, drive, fixed_drive_result) %>%
  summarize(
    is_redzone = max(drive_inside20 == 1)
    ) %>%
  # filter(fixed_drive_result == "Touchdown" & is_redzone == 1) %>%
  ungroup() %>%
  group_by(posteam, game_id) %>%
  # mutate(num_redzone_td_game = sum(is_redzone_td))
  summarize(
    num_drives = n(),
    num_redzone = sum(is_redzone)
  ) %>%
  ungroup() %>%
  group_by(posteam) %>%
  # Get total amount of redzone attemps for that team by adding all values in column
  summarize_at(vars(num_drives:num_redzone), sum) %>%
  mutate(percent_redzone_trips =  num_redzone / num_drives)

redzone_trips_plot <- redzone_trips %>%
  ggplot(aes(y = reorder(posteam, percent_redzone_trips), x = percent_redzone_trips)) +
  geom_col(aes(color = posteam, fill = posteam), width = 0.5) +
  nflplotR::scale_color_nfl(type = "secondary") +
  nflplotR::scale_fill_nfl(alpha = 0.5) +
  nfl_analytics_theme() +
  theme(axis.text.y = element_nfl_logo(size = 0.6)) +
  scale_x_continuous(
    expand = c(0, 0),
    breaks = scales::breaks_pretty(n = 5),
    label = scales::percent_format()
  ) +
  labs(
    title = "Highest Percent of Drives that Resulted in Redzone Appearances",
    subtitle = "*2024-25 Regular Season*",
    caption = "@apwellanalytics.substack.com"
  ) +
  xlab("Redzone Appearance Percentage") +
  ylab("")

ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/redzone_trips.png", redzone_trips_plot, w = 13, h = 13, dpi = 300)

# show the amount of plays teams score touchdown with goal-to-go range (10 or fewer yards from goal line) and redzone success
# do the same for field goals
# vairables in load_pbp() -> drive_inside20 & goal_to_go
redzone <- pbp %>%
  filter(!is.na(posteam) & !is.na(drive_inside20) & drive_inside20 == 1 & season_type == "REG") %>%
  filter(special == 0 & fumble == 0 & interception == 0) %>%
  group_by(posteam, game_id, drive, fixed_drive_result) %>%
  summarize(
    is_redzone_td = max(fixed_drive_result == "Touchdown")
    ) %>%
  # filter(fixed_drive_result == "Touchdown" & is_redzone == 1) %>%
  ungroup() %>%
  group_by(posteam, game_id) %>%
  summarize(
    num_redzone = n(),
    num_redzone_td = sum(is_redzone_td)
  ) %>%
  ungroup() %>%
  group_by(posteam) %>%
  summarize_at(vars(num_redzone:num_redzone_td), sum) %>%
  mutate(percent_redzone_succ = num_redzone_td / num_redzone) %>%
  arrange(desc(percent_redzone_succ))

gtg <- pbp %>%
  filter(!is.na(posteam) & !is.na(goal_to_go) & goal_to_go == 1 & season_type == "REG") %>%
  filter(special == 0 & fumble == 0 & interception == 0) %>%
  group_by(posteam, game_id, drive, fixed_drive_result) %>%
  summarize(
    is_gtg_td = max(fixed_drive_result == "Touchdown")
  ) %>%
  ungroup() %>%
  group_by(posteam, game_id) %>%
  summarize(
    num_gtg = n(),
    num_gtg_td = sum(is_gtg_td)
  ) %>%
  ungroup() %>%
  group_by(posteam) %>%
  summarize_at(vars(num_gtg:num_gtg_td), sum) %>%
  mutate(percent_gtg_succ = num_gtg_td / num_gtg) %>%
  arrange(desc(percent_gtg_succ))

## Theme from the F5
theme_f5 <- function(font_size = 9) {
  theme_minimal(base_size = font_size, base_family = "Roboto") %+replace%
    theme(
      plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0, size = 14, face = "bold"),
      plot.subtitle = element_text(color = "gray65", hjust = 0, margin = margin(2.5, 0, 10, 0), size = 11),
      plot.caption = element_text(color = "gray65", margin = margin(-5, 0, 0, 0), hjust = 1, size = 6)
    )
}

scoring_cards <- left_join(redzone_trips, redzone, by = "posteam") %>%
  left_join(gtg, by = "posteam") %>%
  # Rank is technically backwards from a logical standpoint here
  # #32 is the best and #1 is the worst. This is done to preserve
  # the card plot ranking axis below
  mutate(
    Trips_rk = min_rank(percent_redzone_trips),
    Success_rk = min_rank(percent_redzone_succ),
    GTG_rk = min_rank(percent_gtg_succ)
  ) %>%
  rename(Trips = percent_redzone_trips, Success = percent_redzone_succ, GTG = percent_gtg_succ) %>%
  pivot_longer(
    cols = c(Trips, Success, GTG),
    names_to = "stat_type",
    values_to = "value"
  ) %>%
  pivot_longer(
    cols = c(Trips_rk, Success_rk, GTG_rk),
    names_to = "rank_type",
    values_to = "rank"
  ) %>%
  # keep only matching stat types (e.g Trips with Trips_rk)
  filter(gsub("_rk", "", rank_type) == stat_type) %>%
  mutate(rank = ceiling((rank / 32) * 100)) %>%
  select(posteam, stat_type, value, rank) %>%
  group_by(posteam) %>%
  mutate(total_rk = sum(rank)) %>%
  mutate(posteam = as.factor(posteam)) %>%
  mutate(stat_type = factor(stat_type, levels = c("Trips", "Success", "GTG")))

p <- scoring_cards %>%
  ggplot(aes(fct_rev(stat_type), y = rank)) +
  theme_f5() +
  # ~ creates a formula. Used in this context, we are saying use output of fct_reorder
  facet_wrap(~ fct_reorder(posteam, -total_rk), ncol = 4, nrow = 8)

p <- p +
  coord_flip(clip = "off") +
  # Draw square around each team
  geom_rect(
    xmin = 8.7, xmax = 0.45, ymin = 0, ymax = 100, fill = "#F9E5DA",
    alpha = 0.15, color = "black", linewidth = 0.25
  ) +
  # Draw bars
  geom_chicklet(aes(fill = stat_type, color = after_scale(clr_darken(fill, 0.3))),
    size = 0.5,
    alpha = 0.75
  ) +
  # Add background color
  geom_col(aes(fill = stat_type, y = 100), alpha = 0.1, color = "black", linewidth = 0.05)

p <- p +
  # add percentile value labels
  geom_shadowtext(
    data = . %>%
      filter(rank >= 15), aes(
      y = rank - 7, x = stat_type, label = rank,
      family = "Roboto"
    ), bg.r = 0.15, fontface = "bold", bg.colour = "black",
    color = "white", hjust = 0, size = 2.85
  ) +
  geom_shadowtext(
    data = . %>%
      filter(rank < 15), aes(
      y = rank + 1.5, x = stat_type, label = rank,
      family = "Roboto"
    ), bg.r = 0.15, fontface = "bold", bg.colour = "black",
    color = "white", hjust = 0, size = 2.85
  ) +
  # set color palette
  scale_fill_paletteer_d("suffrager::CarolMan") +
  # format y axis
  scale_y_continuous(expand = c(0, 0))

p <- p +
  theme(
    # get rid of axis text
    axis.text = element_blank(),
    # Uses element_markdown to color words in title
    plot.title =
      element_markdown(hjust = .5, size = 19, lineheight = 1.15),
    plot.subtitle =
      element_text(margin = margin(5, 0, 45, 0), hjust = 0.5, size = 10),
    # remove gridlines
    panel.grid = element_blank(),
    # adjust facet spacing
    panel.spacing.y = unit(3.25, "lines"),
    panel.spacing.x = unit(1, "lines"),
    # adjust margins
    plot.margin = margin(1, 1, -.5, 0, "lines"),
    # remove legend
    legend.position = "none",
    # reference https://nflplotr.nflverse.com/reference/element.html
    # make wordmarks of team abbreviations
    strip.text = element_nfl_wordmark(size = 1),
    plot.caption = element_text(colour = 'black', margin = margin(5, 0, 45, 0), hjust = 1, size = 8)
  )

p <- p +
  # add title and subtitle
  labs(
    title = "2024 - 25 NFL Teams Ranked By Their <br>
    <span
      style='color:#EE2617FF'>**Redzone Trips**
    </span>,
    <span
      style='color:#F2A241FF'>**Redzone TDs**
    </span>, and
    <span
      style='color:#4E7F2F'>**Goal-To-Go TDs**
    </span>",
    subtitle = "Values displayed as percentiles compared to all 32
      teams in the 2024-25 Season (Regular Season Only)",
    x = "",
    y = "",
    caption = "apwellanalytics.substack.com"
  )
ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/cards.png", p, w = 13, h = 13, dpi = 300)

team_info <- load_teams()
atl_colors <- team_info %>%
  filter(team_abbr == "ATL") %>%
  select(team_abbr, contains("team_color"))
# get falcons pbp for the past 10 years
pbp_10_years <- load_pbp(2015:2024)
# Amount of Explosive Plays on offense in past 10 years 
explosive_10_year <- pbp_10_years %>%
  filter(posteam == "ATL" & !is.na(yards_gained) & season_type == "REG") %>%
  # filter out only offensive plays (pass or run)
  filter(play_type %in% c("pass", "run")) %>%
  filter(special == 0 & fumble == 0 & interception == 0) %>%
  # select(posteam, season, game_id, drive, play_id)
  group_by(season, game_id) %>%
  summarize(
    num_plays = n(),
    num_explosive_plays = if_else(yards_gained >= 20, 1, 0)
  ) %>%
  mutate_at(vars(num_explosive_plays),sum) %>%
  distinct() %>%
  ungroup() %>%
  group_by(season) %>%
  summarize(
    num_plays = sum(num_plays),
    num_explosive_plays = sum(num_explosive_plays)
  ) %>%
  mutate(season = as.factor(season)) %>%
  mutate(explosive_play_rate = (num_explosive_plays / num_plays))
# SHOW like on nflplotR quick setup and then show compared to league like reddit post. Probaly get rid of group = 1
explosive_plot <- explosive_10_year %>%
  ggplot(aes(x = season, y = explosive_play_rate, group = 1)) +
  geom_line(size = 2, color = atl_colors$team_color2) +
  geom_point(shape = 21, color = atl_colors$team_color3, fill = atl_colors$team_color4, size = 6) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent_format()) +
  annotate("label", x = "2016", y = .086, label = "Super Bowl Apperance", fill = "yellow", size = 3, family = "serif", hjust = -0.4) +
  nfl_analytics_theme() +
  xlab("Season") +
  ylab("Explosive Play Rate") +
  ggtitle("Atlanta Falcons 10 Year Offensive Explosiveness Trends",
    subtitle = "Explosive Play Rate (+20 yards) from 2015 - 2024 (Regular Season Only)") 

ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/explosive.png", explosive_plot, w = 13, h = 13, dpi = 300)

explosive_all <- pbp %>%
  filter(!is.na(posteam) & !is.na(yards_gained) & season_type == "REG") %>%
  # filter out only offensive plays (pass or run)
  filter(play_type %in% c("pass", "run")) %>%
  filter(special == 0 & fumble == 0 & interception == 0) %>%
  # select(posteam, season, game_id, drive, play_id)
  group_by(posteam, game_id) %>%
  summarize(
    num_plays = n(),
    num_explosive_plays = if_else(yards_gained >= 20, 1, 0),
    num_good_plays = if_else(epa > 0, 1, 0)
  ) %>%
  mutate_at(vars(num_explosive_plays:num_good_plays),sum) %>%
  distinct() %>%
  ungroup() %>%
  group_by(posteam) %>%
  summarize(
    num_plays = sum(num_plays),
    num_explosive_plays = sum(num_explosive_plays),
    num_good_plays = sum(num_good_plays)
  ) %>%
  mutate(explosive_play_rate = (num_explosive_plays / num_plays)) %>%
  mutate(succ_rate = num_good_plays / num_plays)

min_x <- min(explosive_all$explosive_play_rate)
max_x <- max(explosive_all$explosive_play_rate)
min_y <- min(explosive_all$succ_rate)
max_y <- max(explosive_all$succ_rate)
# SHOW like on nflplotR quick setup and then show compared to league like reddit post. Probaly get rid of group = 1
explosive_all_plot <- explosive_all %>%
  ggplot(aes(x = explosive_play_rate, y = succ_rate)) +
  geom_mean_lines(aes(x0 = explosive_play_rate, y0 = succ_rate),
                  color = "black", size = 0.8) +
  # Q1 text
  annotate("text", x = .045, y = .523, label = "Efficient\nLow Risk Takers", size = 6, fontface = 2) +
  # Q2 text
  annotate("text", x = .08, y = .523, label = "High Risk\nHigh Reward", size = 6, fontface = 2) +
  # Q3 text
  annotate("text", x = .045, y = .375, label = "Hard to Watch", size = 6, fontface = 2) +
  # Q4 text
  annotate("text", x = .08, y = .375, label = "Risk Takers\nthat Don't Captialize", size = 6, fontface = 2) +
  # Color fill for Q2
  annotate("rect", xmin = .0595, xmax = max_x + .002  , ymin = .449, ymax = max_y + .005 , fill = "#80EF80", alpha = 0.5 ) +
  # Color fill for Q3
  annotate("rect", xmin = min_x - .002 , xmax = .059,  ymin = min_y - .0025, ymax = .448, fill = "#FF7F7F", alpha = 0.5 ) +
  geom_nfl_logos(aes(team_abbr = posteam), width = 0.065) + 
  nfl_analytics_theme() +
  scale_x_continuous(breaks = scales::breaks_pretty(),
                     labels = scales::percent_format()) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent_format()) +
  xlab("Explosive Play Rate") +
  ylab("Success Rate") +
  ggtitle("Offensive Explosive Play Rate vs Offensive Success Rate",
    subtitle = "Explosive Play (+20 yards) Rate from 2024-25 (Regular Season Only)") 
ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/explosive_all.png", explosive_all_plot, w = 13, h = 13, dpi = 300)

# Amount of Drives with Explosive Plays on Offense  Resulting in TD in past 10 years for Falcons

# explosive_10_year_td <- pbp_10_years %>% 
#   filter(posteam == "ATL" & !is.na(yards_gained) & season_type == "REG") %>%
#   filter(special == 0 & fumble == 0 & interception == 0) %>%
#   filter(fixed_drive_result == "Touchdown") %>% 
#   group_by(game_id, drive) %>%
#   mutate(max_yards = max(yards_gained)) %>% 
#   mutate(explosive_play = if_else(max_yards >= 20, 1, 0)) %>%
#   ungroup() %>%
#   group_by(season) %>%
#   summarize(num_explosive_plays = sum(explosive_play == 1)) %>%
#   mutate(season = as.factor(season))

# explosive_plot_td <- explosive_10_year_td %>%
#   ggplot(aes(x = season, y = num_explosive_plays, group = 1)) +
#   geom_line(color = "grey") +
#   geom_point(shape = 21, color = "black", fill = "red", size = 6) +
#   ggtitle("Atlanta Falcons 10 Year Offensive Explosiveness Trends",
#     subtitle = "Showcasing the Amount of Touchdown Drives including Explosive Plays from 2015 - 2024 (Regular Season Only)") 
# ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/explosive_td.png", explosive_plot_td, w = 13, h = 13, dpi = 300)

# explosive plays resulting in TD drives 
# explosive_td_all <- pbp %>%
#   filter(!is.na(posteam) & !is.na(yards_gained) & season_type == "REG") %>%
#   filter(special == 0 & fumble == 0 & interception == 0) %>%
#   filter(fixed_drive_result == "Touchdown") %>%
#   group_by(posteam, game_id, drive) %>%
#   mutate(max_yards = max(yards_gained)) %>%
#   mutate(explosive_play = if_else(max_yards >= 20, 1, 0)) %>%
#   ungroup() %>%
#   group_by(posteam) %>%
#   summarize(num_explosive_plays = sum(explosive_play == 1)) 

# explosive_plot_all <- explosive_td_all %>%
#   ggplot(aes(x = num_explosive_plays, y = reorder(posteam, num_explosive_plays))) +
#   geom_col(aes(color = posteam, fill = posteam)) +
#   scale_color_nfl(type = "secondary") +
#   scale_fill_nfl(alpha = 0.5) +
#   nfl_analytics_theme() +
#   theme(axis.text.y = element_nfl_logo()) +
#   labs(
#     title = "2024-25 NFL Teams Explosive Play Count Resulting in Touchdown",
#     subtitle = "Showcasing the Amount of Touchdown Drives Each Team Had including an Explosive Play",
#     x = "# of Plays",
#     y = "Teams"
#   )
# ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/explosive_all.png", explosive_plot_all, w = 13, h = 13, dpi = 300)

# Red zone results all teams as a scatterplot
# drive_inside20 == 1
redzone_to_succ <- pbp %>%
  filter(!is.na(posteam) & !is.na(drive_inside20) & season_type == "REG") %>%
  filter(special == 0, fumble == 0, interception == 0) %>%
  group_by(posteam, game_id, drive, fixed_drive_result) %>%
  summarize(
    is_redzone = max(drive_inside20 == 1),
    is_redzone_td = max(fixed_drive_result == "Touchdown" & is_redzone == 1),
    num_pass = sum(play_type == "pass"),
    num_run = sum(play_type == "run"),
    num_plays = drive_play_count
    ) %>%
  # filter(fixed_drive_result == "Touchdown" & is_redzone == 1) %>%
  ungroup() %>%
  group_by(posteam, game_id, drive) %>%
  # select(posteam, game_id, drive, is_redzone, is_redzone_td)
  # mutate(num_redzone_td_game = sum(is_redzone_td))
  summarize(
    drive_in_redzone = max(is_redzone),
    drive_in_redzone_td = max(is_redzone_td),
  ) %>%
  summarize_at(vars(drive_in_redzone:drive_in_redzone_td), sum) %>%
  ungroup() %>%
  group_by(posteam) %>%
  summarize(
    num_trips = sum(drive_in_redzone),
    num_tds = sum(drive_in_redzone_td),
    percent_redzone_succ =  if_else(num_trips == 0, 0, num_tds / num_trips)
  )

min_x <- min(redzone_to_succ$percent_redzone_succ)
max_x <- max(redzone_to_succ$percent_redzone_succ)
min_y <- min(redzone_to_succ$num_trips)
max_y <- max(redzone_to_succ$num_trips)
redzone_succ_plot <- redzone_to_succ %>%
  ggplot(aes(x = percent_redzone_succ, y = num_trips)) +
  nflplotR::geom_mean_lines(aes(x0 = percent_redzone_succ, y0 = num_trips),
    color = "black", size = 0.8
  ) +
  # Color fill for Q2
  annotate("rect", xmin = .57, xmax = max_x + .012 , ymin = 55.4, ymax = max_y + 0.8 , fill = "#80EF80", alpha = 0.5 ) +
  # Color fill for Q3
  annotate("rect", xmin = min_x - .01, xmax = .568, ymin = min_y - 0.9, ymax = 55.25, fill = "#FF7F7F", alpha = 0.5 ) +
  nflplotR::geom_nfl_logos(aes(team_abbr = posteam), width = 0.065) +
  # Q1
  annotate("text", x = .44, y = 74, label = "Can't Capitilize", size = 6, fontface = 2) +
  # Q2 
  annotate("text", x = .73, y = 74, label = "Very Likely\nto Score Touchdown", size = 6,  fontface = 2) +
  # Q3
  annotate("text", x= .44, y = 40, label = "Send Field\nGoal Team Out", size = 6, fontface = 2) +
  # Q4
  annotate("text", x= .73, y = 40, label = "Efficient\nWithout Seeing\nRedzone Often", size = 6, fontface = 2) +
  nfl_analytics_theme() +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12)) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6)
  ) +
  xlab("Redzone Touchdown Percentage") +
  ylab("Redzone Trips") +
  labs(
    title = "Redzone Trips vs. Redzone Touchdowns",
    subtitle = "2024-25 Regular Season Only",
    caption = "apwellanalytics.substack.com"
  )
ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/redzone_comparison.png", redzone_succ_plot, w = 13, h = 13, dpi = 300)


redzone_to_succ_play_type <- pbp %>%
  filter(!is.na(posteam) & !is.na(drive_inside20) & season_type == "REG") %>%
  filter(special == 0, fumble == 0, interception == 0) %>%
  filter(posteam == "ATL" & drive_inside20 == 1) %>%
  # select(posteam, game_id, play_id, drive)
  group_by(posteam, game_id, drive) %>%
  summarize(
    is_redzone_td = max(fixed_drive_result == "Touchdown"),
    num_pass = sum(play_type == "pass"),
    num_run = sum(play_type == "run"),
    num_plays = n(),
    opp = defteam
    ) %>%
  distinct() %>%
  ungroup() %>%
  group_by(posteam, game_id) %>%
  summarize(
    num_pass = sum(num_pass),
    num_run = sum(num_run),
    num_plays = sum(num_plays),
    tds = sum(is_redzone_td),
    trips = n(),
    td_to_trips = if_else(trips == 0, 0, tds / trips),
    run_ratio = num_run / num_plays,
    pass_ratio = num_pass / num_plays,
    opp = opp
  ) %>%
  arrange(-td_to_trips) %>%
  distinct()

redzone_opp_plot <- redzone_to_succ_play_type %>%
  ggplot(aes(x = run_ratio, y = td_to_trips)) +
  geom_smooth(method = "lm", se = FALSE,
                color = "black",
                linetype = "dashed", 
                size = .8) +
  geom_nfl_logos(aes(team_abbr = opp), width = 0.065) +
  scale_x_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent_format()) +
  scale_y_continuous(breaks = scales::pretty_breaks(),
                     labels = scales::percent_format()) +
  labs(title = "Atlanta Falcons Redzone Run vs Success Rate",
       subtitle = "Highlights Success Rate vs Run Usage against Opponents in the 2024-25 Regular Season") +
  xlab("Run Percentage") +
  ylab("Success Rate") +
  nfl_analytics_theme()
ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/redzone_succ.png", redzone_opp_plot, w = 13, h = 13, dpi = 300)

bijan_gis = falcons_rbs %>%
            filter(first_name == "Bijan") %>%
            pull(gsis_id)
tyler_gis = falcons_rbs %>%
            filter(first_name == "Tyler") %>%
            pull(gsis_id)
rusher_succ <- pbp %>%
  filter(!is.na(posteam) & !is.na(drive_inside20) & season_type == "REG") %>%
  filter(special == 0, fumble == 0, interception == 0) %>%
  filter(posteam == "ATL" & drive_inside20 == 1) %>%
  filter(!is.na(rusher_id) & play_type == "run") %>%
  # select(posteam, game_id, play_id, drive, rusher_id)
  group_by(game_id) %>%
  summarize(
    rusher_id,
    num_run = n(),
    is_bijan = as.numeric(bijan_gis == rusher_id),
    is_tyler = as.numeric(tyler_gis == rusher_id),
    rush_seq = if_else(rusher_id %in% bijan_gis, list(is_bijan),
                       if_else(rusher_id %in% tyler_gis, list(is_tyler), list(NA)))
  ) %>%
  ungroup() %>%
  group_by(game_id, rusher_id) %>%
  summarize(
    touches = n(),
    num_run,
    rush_seq,
    touches_pct = scales::percent(touches / num_run, accuracy = 0.01)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(game_id) %>%
  # mutate(majority = if_else(touches == max(touches), 1, 0)) %>%
  mutate(week = str_extract(game_id, "_(\\d+)_") %>% 
  str_remove_all("_") %>% 
  as.integer() %>%
  paste("Week", .)) %>%
  mutate(week = as.factor(week)) %>%
  relocate(week) %>%
  ungroup() %>%
  select(-game_id) %>%
  filter(rusher_id %in% c(bijan_gis, tyler_gis)) 

  # mutate_at(vars(touches_per_run), sum) %>%
  # mutate(avg_touches = touches_per_run / n())
  # distinct() %>%
  # ungroup() %>%
  # group_by(rusher_id) %>%
  # summarize(
  #   avg_touches_per_run = sum(touches_per_run) / n()
  # ) %>%
  # distinct()

colors <- c('#A71930', '#000000', '#A5ACAF', '#A30D2D', 'white')
rusher_succ_table <- rusher_succ %>%
  select(-c(touches, num_run)) %>%
  gt(groupname_col = "week") %>%
  cols_label(rusher_id = "",
             rush_seq = "Rush Plays",
             touches_pct = "Touch Percentage") %>%
  cols_align("left", week) %>%
  gt_plt_winloss(rush_seq, type = "pill") %>% 
  gt_nfl_headshots(columns = rusher_id, height = 20) %>%
  gt_theme_538() %>%
  tab_header(
    title = gtExtras::add_text_img(
      "2024-25 Redzone Usage",
      url = "https://raw.githubusercontent.com/nflverse/nflverse-pbp/master/wordmarks/ATL.png",
      height = 30
    ),
    subtitle = "Highlighting the Robinson & Allgier Runningback Duo"
  ) %>%
  tab_options(data_row.padding = px(2))
gtsave(rusher_succ_table, filename = "rusher_table.html", path = "C:/Users/apowell72/Pictures/NFL_Analytics")
rusher_succ_plot <- rusher_succ %>%
  ggplot(aes(x = week, y = touches_per_run)) +
  geom_bar(aes(colour = "black", fill = rusher_id), position = "dodge",
               stat = "identity", show.legend = FALSE) +
  geom_nfl_headshots(aes(player_gsis = rusher_id), width = 0.075, vjust = 0.45) +
  facet_wrap(~week) +
  scale_fill_manual(values = colors) +
  nfl_analytics_theme() +
  scale_y_continuous(labels = scales::percent_format(),
                     breaks = scales::pretty_breaks()) +
  labs(
    title = "Atlanta Falcons 2024-25 Running Usage in Redzone",
    subtitle = "Shows the average percent of redzone touches per the amount of run plays in a featured redzone drive by each runner"
  ) +
  # theme(
  #   axis.title.x = element_blank(),
  #   axis.text.x = element_nfl_headshot(size = 4)
  # ) +
  ylab("Usage Percent")
  # geom_text(aes(label = scales::percent(touches_per_run), vjust = -0.5), size = 5)

ggsave("C:/Users/apowell72/Pictures/NFL_Analytics/rb_comparison.png", rusher_succ_plot, w = 13, h = 13, dpi = 300)
