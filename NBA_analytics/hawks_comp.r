library(dplyr)
library(ggplot2)
library(tidyverse)
library(janitor)
library(jsonlite)
library(httr)
library(hablar)
library(glue)
library(hoopR)
library(stringr)
library(cluster)
library(factoextra)
library(gghighlight)
library(ggrepel)
library(FactoMineR)
library(mclust)
library(paletteer)
library(cowplot)
library(nbastatR)
library(rvest)
library(pvclust)

theme_analytics <- function(font_size = 12) {
  theme_minimal(base_size = font_size, base_family = "Consolas") %+replace%
    theme(
      plot.background = element_rect(
        fill = '#F0FFF0', 
        color = "#F0FFF0"
      ),
      panel.grid.major = element_line(color = 'darkgray'),
      panel.grid.minor = element_blank(),

      axis.ticks = element_blank(),
      axis.title = element_text(color = "black",
                                face = "bold"),
      axis.text = element_text(color = "black",
                               face = "bold"),
      plot.title.position = "plot",
      plot.margin = margin(10, 10, 15, 10),
      plot.title = element_text(size = 18,
                                face = "bold",
                                color = "black",
                                vjust = 2,
                                hjust = 0),
      plot.subtitle = element_text(
        # color = 'gray65',
        hjust = 0,
        vjust = 1,
        margin = margin(2.5, 0, 10, 0),
        size = 11
      ),
      plot.caption = element_text(
        color = 'gray65',
        margin = margin(-5, 0, 0, 0),
        hjust = 1,
        size = 6
      )
    )
}

team_abbr <- c("ATL", "IND", "NYK")

url <- "https://www.basketball-reference.com/leagues/NBA_2025_advanced.html"

adv_page <- read_html_live(url)

df_adv <- adv_page %>%
    html_element("table") %>%
    html_table() %>%
    select(where(~!all(is.na(.)))) %>%
    clean_names() %>%
    mutate(across(g:vorp, as.numeric)) %>%
    select(player,team, mp, per:vorp)

url <- "https://www.basketball-reference.com/leagues/NBA_2025_shooting.html"

shooting_page <- read_html_live(url)

df_shooting <- shooting_page %>%
    html_element("table") %>%
    html_table() %>%
    row_to_names(1) %>%
    clean_names() %>%
    mutate(across(fg_percent:x3p_percent, as.numeric)) %>%
    select(player, fg_percent:x3p_percent)

url <- "https://www.basketball-reference.com/leagues/NBA_2025_per_minute.html"

per_min_page <- read_html_live(url)

df_per_min <- per_min_page %>%
    html_element("table") %>%
    html_table() %>%
    clean_names() %>%
    mutate(across(fg:pts, as.numeric)) %>%
    select(player, fg:pts) %>%
    select(-pf)

url <- "https://www.nba.com/stats/team/1610612737"

page <- read_html_live(url)

hawks_25 <- page %>%
    html_node(xpath = '//*[@id="__next"]/div[2]/div[2]/main/div[3]/section[3]/div/div[2]/div[3]/table') %>%
    html_table()

hawks_25_list <- hawks_25 %>%
    row_to_names(1) %>%
    clean_names() %>%
    hablar::retype() %>%
    pull(player) %>%
    as.list()

team_abbr_all <- espn_nba_teams() %>%
    pull(abbreviation) %>%
    as.list() 
team_abbr_all <- replace(team_abbr_all, team_abbr_all == "GS", "GSW") 
team_abbr_all <- replace(team_abbr_all, team_abbr_all == "UTAH", "UTA") 
team_abbr_all <- replace(team_abbr_all, team_abbr_all == "WSH", "WAS") 
team_abbr_all <- replace(team_abbr_all, team_abbr_all == "SA", "SAS") 
team_abbr_all <- replace(team_abbr_all, team_abbr_all == "NY", "NYK") 
team_abbr_all <- replace(team_abbr_all, team_abbr_all == "NO", "NOP")

clean_data <- function(col, abbrs) {
    result <- gsub("\\|.*", "", col)
    result <- gsub("([a-z])([A-Z])", "\\1 \\2", result)
    result <- gsub("(?<=Mc)\\s(?=[A-Z])", "", result, perl = TRUE)
    for (abbr in abbrs) {
        result <- gsub(abbr, "", result, fixed = TRUE)
    }
    return(result)
}

convert_measurment <- function(meas) {
    # convert either wingspan or height fromf feet/inches to cm
    meas <- str_trim(meas)
    parts <- unlist(strsplit(meas, "'|\""))
    feet <- as.numeric(parts[1])
    inches <- as.numeric(parts[2])
    meas_cm <- feet * 30.48 + inches * 2.54
    return(meas_cm)
}
url <- "https://craftednba.com/player-traits/length"
player_measurments_page <- read_html_live(url)

player_measurments <- player_measurments_page %>%
    html_node(xpath = '//*[@id="__nuxt"]/div/div[2]/div[3]/div[1]/div[2]') %>%
    html_table() %>%
    clean_names() %>%
    hablar::retype() %>%
    select(!number) %>%
    mutate(name = str_trim(clean_data(name, team_abbr_all)))

hawks_no_more <- c('Clint Capela', 'Caris LeVert', "Larry Nance Jr.", "Georges Niang", "Terance Mann", "Garrison Mathews", "De'Andre Hunter", "Bogdan Bogdanović")
hawks_non_8 <- c('Vít Krejčí', 'Jacob Toppin')
pacers_non_8 <- c('Ben Sheppard', 'Jarace Walker')
gsw_non_8 <- c('Damion Lee')
nyk_non_8 <- c('P.J. Tucker', 'Precious Achiuwa', 'Landry Shamet', 'Jericho Sims', 'Tyler Kolek')
df_player_comp <- df_adv %>%
    left_join(df_shooting, by = join_by(player == player), suffix = c('_adv', '_shooting')) %>%
    left_join(df_per_min, by = join_by(player == player), suffix = c('_adv', '_per36')) %>%
    left_join(player_measurments, by = join_by(player == name)) %>%
    # fill in Mitchell Robinson 
    # Length is the absolute difference (in inches) between the height and wingspan of a player
    mutate(height = if_else(player == "Mitchell Robinson", r"(6'11.5")", height),
           wingspan = if_else(player == "Mitchell Robinson", r"(7'4")", wingspan),
           length = if_else(player == "Mitchell Robinson", 4.5, length)) %>%
    mutate(team = if_else(player %in% hawks_25_list, "ATL", team)) %>%
    mutate(height = sapply(height, convert_measurment)) %>%
    mutate(wingspan = sapply(wingspan, convert_measurment)) %>%
    filter(team %in% team_abbr) %>%
    filter(!player %in% hawks_no_more & !player %in% hawks_non_8 & !player %in% pacers_non_8 & !player %in% gsw_non_8 & !player %in% nyk_non_8) %>%
    group_by(team) %>%
    slice_max(order_by = mp, n = 8) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), ~replace_na(.,0)))

X <- df_player_comp %>%
    column_to_rownames(var = "player") %>%
    select(-team) %>%
    scale()
X_mod <- as_tibble(X)
pca_players <- prcomp(X)
fviz_eig(pca_players)
fviz_pca_ind(
    pca_players, 
    col.ind = "cos2", # Color by the quality of representation
    gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
    repel = TRUE)

fviz_pca_var(
    pca_players,
    repel = TRUE
)

data_sum_ind <- facto_summarize(pca_players, element = "ind", axes = 1:2)
# data_sum_var <- facto_summarize(pca_players, element = "var", axes = 1:2, result = 'coord')
p <- fviz_pca_biplot(pca_players, 
                repel = TRUE,
                geom.ind  = "point",
                # col.ind = "cos2", # Color by the quality of representation
                # gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                col.var = "#2E9FDF", # Variables color
                alpah.var = "contrib"
                ) + 
    geom_label_repel(data = data_sum_ind, 
                aes(x=Dim.1,y=Dim.2, label = rownames(data_sum_ind)), 
                ) +
    theme_analytics() +
    theme(
    # plot.title = element_text(size = 10),
    # plot.subtitle = element_text(size = 8),
    # turn the logo text urls into images
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    # remove legend
    legend.position = 'none',
    plot.margin = unit(c(1, 2, 1, 1), "lines")
  ) + 
  labs(
        title = "Player Similarity",
        subtitle = "Shows the relationship between advanced stats and players on the 25-26 Hawks, 24-25 Pacers, and 24-25 Knicks",
        x = "PC 1",
        y = "PC 2"
    ) 
  
  ggsave("player_biplot.png", p, w = 10, h = 10, dpi = 600)

set.seed(222)
d <- dist(X, method = "euclidean")
hc <- hclust(dist(d), method = "complete")
png(filename = "dendrogram.png", width = 600, height = 600)
plot(hc)
rect.hclust(hc, k = 10)
dev.off() 

result_pvclust <- pvclust(X, method.hclust="complete",
                              method.dist="euclidean", nboot=1000)
plot(result_pvclust)
# Highlight clusters with high AU p-values (e.g., > 95%)
pvrect(result_pvclust, alpha=0.95, print.num=TRUE)

significant_clusters <- pvpick(result_pvclust, alpha=0.95, pv="au")
print(significant_clusters)

cluster_memberships <- cutree(result_pvclust$hclust, k=10)
plot_features <- names(cluster_memberships)
## START HERE
plot_features <- c("per", "ts_percent", "x3p_ar","orb_percent", 
    "drb_percent", "trb_percent", "ast_percent", "stl_percent", "blk_percent", "x3p_percent_per36", "x2p_per36", "x2pa", 
"x2p_percent", "e_fg_percent", "height", "wingspan")

X_mod <- X_mod %>%
    select(plot_features) %>%
    rename(c("PER" = "per", "%TS" = "ts_percent", "3PAr" = "x3p_ar","%ORB" = "orb_percent", 
            "%DRB" = "drb_percent", "%TRB" = "trb_percent", "%AST" = "ast_percent", "%STL" = "stl_percent", 
            "%BLK" = "blk_percent", "%3P_36" = "x3p_percent_per36", "%2P_36" = "x2p_per36", "2PA" = "x2pa", 
            "%2P" = "x2p_percent", "%EFG" = "e_fg_percent", "HT." = "height", "W.S." = "wingspan"))

clusters <- cutree(hc, k = 10) %>%
    enframe(name = "Player", value = "Type") 
clusters$Type <- as.factor(clusters$Type)

X_mod$clusters <- clusters$Type
data_long <- pivot_longer(X_mod, cols = -clusters, names_to = "feature", values_to = "value") 

p <- ggplot(data_long, aes(x = feature, y = value, color = clusters)) +
  geom_point() + # plot points
  scale_color_brewer(palette="Paired") + # color points
  gghighlight(use_direct_label = FALSE) + # highlight each cluster
  facet_wrap(~ clusters, ncol=3) + # create seperate plots for each cluster
  labs(x = "Predictor", y = "Cluster Center", 
       title = "Player Strengths by Cluster",
       subtitle = "Showing the feature distribution from hierarchical clustering model") + 
  theme_analytics() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8), # alter axis text
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank())

ggsave("player_stengths.png", p, w = 10, h = 10, dpi = 600)

clusters <- clusters %>%
    left_join(df_player_comp, by = join_by("Player" == "player")) %>%
    select(Player, Type, team) 

star_players <- c('Trae Young', 'Tyrese Haliburton', 'Jalen Brunson')
desired_order <- list(
        ATL = c('Trae Young', 'Dyson Daniels', 'Zaccharie Risacher', 'Jalen Johnson', 'Kristaps Porziņģis', 'Onyeka Okongwu', 'Nickeil Alexander-Walker', 'Luke Kennard'),
        IND = c('Tyrese Haliburton', 'Andrew Nembhard', 'Aaron Nesmith', 'Pascal Siakam', 'Myles Turner', 'T.J. McConnell', 'Obi Toppin', 'Bennedict Mathurin'),
        NYK = c('Jalen Brunson', 'Josh Hart', 'Mikal Bridges', 'OG Anunoby', 'Karl-Anthony Towns', 'Mitchell Robinson', 'Miles McBride', 'Cameron Payne')
)
mod <- clusters %>%
  mutate(
    first_init = str_sub(Player, 1, 1),
    last_name = word(Player, -1),
    short_name = paste0(first_init, ".", last_name)
  ) %>%
  mutate(star_label = if_else(Player %in% star_players, "\n★", NA_character_)) %>%
  mutate(short_name = if_else(!is.na(star_label), paste0(short_name, star_label), short_name)) %>%
  # create an index within each team to order players
  rowwise() %>%
  mutate(player_index = match(Player, desired_order[[team]])) %>% 
  ungroup() 

p <- mod %>%
  mutate(team = if_else(team == "ATL", paste0(team, " ", "25-26"), paste(team, "24-25"))) %>%
  mutate(short_name = if_else(short_name == "K.Towns", "K.A.T", short_name)) %>%
  mutate(short_name = if_else(short_name == "N.Alexander-Walker", "N.A.W", short_name)) %>%
  ggplot(aes(x = player_index, y = team,  fill = as.factor(Type))) +
  geom_tile(width = 0.8, height = 0.4, color = "black") +  # Create squares for each player
  geom_text(aes(label = short_name), family = 'Consolas', size = 1.8, show.legend = FALSE) +
  coord_fixed(ratio = 1, clip = "off") +
  scale_fill_manual(values = c("1" = "#abdda4", "2" = "#d53e4f", "3" = "#f46d43", "4" = "#fdae61", "5" = "#fee08b", 
                               "6" = "#ffffbf", "7" ="#e6f598", "8" = "#abdda4", "9" = "#66c2a5", "10" = "#3288bd", "11" = "#DAB1DA"),
                    labels = c("Star PG", 'Hustle Def.', 'High % 3pt', 'Finisher/Reb.', '3 and D', 'Athletic Big', 
                               'Game Changer', 'Tweener Guards', 'High Reb. Big'),
                    breaks = c("1", "2", "3", "4", "5", "6", "7", "9", "10")) +
  # Replace x-axis ticks with player names
  scale_x_continuous(breaks = mod$player_index, labels = mod$Player, expand = c(0, 0)) +
#   scale_y_discrete(expand = c(0, 0)) +
  guides(fill=guide_legend(
    keywidth= .2,
    keyheight= .2,
    default.unit="inch", 
    title.position = 'top',
    label.position = 'bottom', 
    title.vjust = .5,
    label.vjust = -.25,
    nrow = 1) 
  ) +
  theme_analytics() +
  theme(plot.title = element_text(face = 'bold', size = 12), 
        plot.subtitle = element_text(size = 6), 
        plot.title.position = "plot", 
        legend.position = 'top', 
        aspect.ratio = 1/2,
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.text = element_text(size = 4),
        legend.title = element_text(size = 6, hjust = .5, vjust = -2),
        # plot.margin = margin(0, 10, 0, 10),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 6, vjust = -4.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()
        ) +
    labs(
        title = "Hawks Roster Construction Compared to Rival Teams",
        subtitle = "Tiles represent the player type assigned for each constructed roster",
        fill = "Player Type"
    )

ggsave("roster_comparison.png", p, w = 6, h = 4, dpi = 600)

