library(dplyr)
library(ggplot2)
library(rvest)
library(stringr)
library(tidyverse)
library(janitor)
library(hoopR)
library(ggimage)
library(scales)
library(gtExtras)
library(gtUtils)
library(extrafont)
library(ggtext)
library(ggpath)

font_import()
loadfonts(device = "win")

formatted_date <- format(Sys.Date(), "%B %d, %Y")
years <- c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025)

teams <- hoopR::nba_teams() %>%
    clean_names() %>%
    select(team_abbreviation, color, alternate_color, logo) %>%
    rename("Team" = "team_abbreviation")

get_data <- function(row) {
  cells <- row %>%
    # Access table data
    html_nodes("td") %>%
    html_text(trim = TRUE)
  return(cells)
}
get_tables <- function(year) {
  url <- paste0("https://www.spotrac.com/nba/cap/_/year/", year)
  print(url)
  sal_cap_page <- read_html_live(url)
#   sal_cap_page$wait_for_selector("#DataTables_Table_0_wrapper")

  sal_cap_table <- sal_cap_page %>%
    # Get Sal Cap table
    html_node(xpath = '//*[@id="DataTables_Table_0_wrapper"]')

  data <- sal_cap_table %>%
    # Get all rows from Sal Cap table
    html_nodes("tbody tr") %>%
    lapply(get_data)

  # convert to dataframe
  df <- data %>%
    do.call(rbind, .) %>%
    as_tibble(stringsAsFactors = FALSE)

  colnames(df) <- sal_cap_table %>%
    html_nodes("thead th") %>%
    html_text(trim = TRUE)
  df$Year <- year
  return(df)
}

clean_data <- function(column) {
    data <- gsub("\\$", "", column)
    result <- as.numeric(gsub(",", "", data))
    return(result)
}
df_list <- years %>%
    lapply(get_tables)

df <- bind_rows(df_list)

df <- df %>%
    # Match first word (duplicate team name from table) and replace with empty string
    mutate(Team = str_trim(str_replace(Team, "^[\\w]+\\s", ""))) %>%
    rename_with(str_squish) %>%
    mutate(`Dead Cap` = dplyr::na_if(`Dead Cap`, "-")) %>%
    mutate(across(`Total Cap Allocations`:`Dead Cap`, ~ clean_data(.x))) %>%
    mutate(Year = as.factor(paste0(Year, "-", as.integer(str_sub(as.character(Year), -2, -1)) + 1))) %>%
    select(-Rank) %>%
    group_by(Year) %>%
    mutate(`Active Spend Rank` = rank(desc(Active), ties.method = "min")) %>%
    mutate(`Age Rank` = rank(`Avg Age Team`, ties.method = "min")) 

df <- left_join(df, teams, by = "Team") %>%
    mutate(across(color:alternate_color, ~replace_na(.x, "808080"))) %>%
    mutate(color = paste0("#", color)) %>%
    mutate(alternate_color = paste0("#", alternate_color))

write_csv(df, "team_salaries.csv")

url <- "https://www.basketball-reference.com/playoffs/"

champs_page <- read_html_live(url)

champs_tbl <- champs_page %>%
    html_element("table") %>%
    html_table() %>%
    row_to_names(1) %>%
    # getting rid of random NA column
    select(Year, Champion, `Runner-Up`) %>%
    # Get rid of empty string rows
    filter(if_any(everything(), ~ . !="")) %>%
    filter(as.numeric(Year) >= 2011 & as.numeric(Year) <= 2025) %>%
    mutate(Year = as_factor(paste0(as.integer(Year) - 1, "-", str_sub(as.character(Year), -2, -1))))

# creating a data frame to hold team names to abbreviations
team_abbreviations <- data.frame(
    TeamName = c("Atlanta Hawks", "Boston Celtics", "Brooklyn Nets", 
            "Charlotte Hornets", "Chicago Bulls", "Cleveland Cavaliers", 
            "Dallas Mavericks", "Denver Nuggets", "Detroit Pistons", 
            "Golden State Warriors", "Houston Rockets", "Indiana Pacers", 
            "Los Angeles Clippers", "Los Angeles Lakers", "Memphis Grizzlies", 
            "Miami Heat", "Milwaukee Bucks", "Minnesota Timberwolves", 
            "New Orleans Pelicans", "New York Knicks", "Oklahoma City Thunder", 
            "Orlando Magic", "Philadelphia 76ers", "Phoenix Suns", 
            "Portland Trail Blazers", "Sacramento Kings", "San Antonio Spurs", 
            "Toronto Raptors", "Utah Jazz", "Washington Wizards"),
    Team = c("ATL", "BOS", "BKN", "CHA", "CHI", "CLE", 
                    "DAL", "DEN", "DET", "GSW", "HOU", "IND", 
                    "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", 
                    "NOP", "NYK", "OKC", "ORL", "PHI", "PHX", 
                    "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
)

df_champ <- left_join(df, team_abbreviations, by = "Team") 
df_champ <- rename(df_champ, "Champion" = "TeamName") 
df_champ <- champs_tbl %>%
    inner_join(df_champ, by = c("Champion", "Year")) %>%
    arrange(desc(Year))

write_csv(df_champ, "champion_salaries_2011_2025.csv")

# Insipration for showing teams active spending per year and highlighting the champion: https://thef5.substack.com/p/how-to-player-and-team-usage-chart
df <- read_csv("team_salaries.csv")
df_champ <- read_csv("champion_salaries_2011_2025.csv")

df_champ_mod <- df_champ %>%
  select(Year, Champion, Team)

merged_df <- left_join(df, df_champ_mod, by = c("Team", "Year")) %>%
  mutate(champions = if_else(!is.na(Champion), TRUE, FALSE)) %>%
  select(-Champion)
filtered_df <- merged_df %>%
  filter(Year != "2025-26") 

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

theme_analytics_markdown <- function(font_size = 12) {
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
      plot.subtitle = element_markdown(
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

p <- ggplot(filtered_df, aes(x = Year, y = Active)) +
  geom_point(
    data = filter(filtered_df, champions == TRUE),
    aes(color = alternate_color, fill = color),
    shape = 21,
    color = 'black',
    size = 3.5,
    alpha = .95
  ) +
  geom_text(
    data = filter(filtered_df, champions == TRUE),
    aes(color = color, label = Team),
    fontface = 'bold',
    family = 'Roboto',
    hjust = 0,
    nudge_x = 0.1,
    nudge_y = 0.0075,
    angle = 45,
    size = 3,
    show.legend = FALSE
  ) +
  geom_point(
    data = filter(filtered_df, champions == FALSE),
    aes(color = 'gray', fill = 'gray'),
    shape = 21,
    color = 'black',
    size = 3.5,
    alpha = .2
  ) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_fill_identity() +
  scale_color_identity() +
  labs(title = "Active Cap Spending ",
      subtitle = "Shows the active money spent for each team from 2011-2024 with the championship teams highlighted.\nDoes not account for dead cap a given team may have", 
      x = "Year",
      y = "Active Cap Allocation") +
  theme_analytics()

ggsave("active_cap.png", p, w = 11, h = 8, dpi = 600)
# Show any correlation between championship, age, and active money spent with rankings : https://thef5.substack.com/p/how-to-win-totals-tables-and-more (Each teams Highest ranked palyer vs vegas win total)

# get salary caps from 2011 - 2025 seasons

url <- "https://www.basketball-reference.com/contracts/salary-cap-history.html"

cap_page <- read_html_live(url)

cap_tbl <- cap_page %>%
  html_element("table") %>%
  html_table()

cap_merged <- left_join(df_champ, cap_tbl, by = "Year") %>%
  mutate(`Salary Cap` = clean_data(`Salary Cap`)) %>%
  select(-`2022 Dollars`)

p  <- cap_merged %>%
  ggplot(aes(Year, abs(`Cap Space All`))) +
  geom_col(aes(color = alternate_color, fill = color)) + 
  geom_image(
    aes(image = logo, y = abs(`Cap Space All`))) +
  geom_label(
    aes(label = paste0(round(`Active Top 3`/ 1e6, 2), "M")),
    hjust = -0.4,
    size = 5,
    color = "#AA6C39") +
  scale_y_continuous(
    labels = unit_format(unit = "M", scale = 1e-6),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_flip(clip = "off") +
  labs(
    title = "Over the Cap",
    subtitle = "How far over the salary cap each championship team went along with
    <span
      style='color:#AA6C39'>**Top 3 Players Salaries**
    </span>",
    x = "Year",
    y = "Deficit",
    caption = "apwellanalytics.substack.com"
  ) +
    theme_analytics_markdown()

ggsave("champ_cap.png", p, w = 10, h = 10, dpi = 600)


df_champ %>%
  select(c("Year", "logo", "Avg Age Team", "Active", "Active Spend Rank", "Age Rank")) %>%
  mutate(Active = paste0(round(Active / 1e6, 2), "M")) %>%
  arrange(`Active Spend Rank`, `Age Rank`) %>%
  gt() %>%
  tab_header(title = md("**Past Champions at a Glance**"), 
             subtitle = "General overview of the makeup of NBA Champions over the past 13 years"
  ) %>%
  gt_img_rows(columns = logo) %>%
  cols_label(logo = "",
             `Avg Age Team` = "Avg. Age",
             `Active Spend Rank` = "Active Rank") %>%
  gt_highlight_rows(
    rows = 1, # rows to highlight
    target_col = c(5, 6), # which column to focus on
    bold_target_only = TRUE, # highlight target column 
    fill='darkred', # background color
    font_color = "black", # text color,
    alpha=0.5, # controls color opacity
  ) %>%
  gt_highlight_rows(
    rows = 13, # rows to highlight
    target_col = c(5, 6), # which column to focus on
    bold_target_only = TRUE, # highlight target column 
    fill='lightblue', # background color
    font_color = "black", # text color,
    alpha=0.5, # controls color opacity
  ) %>%
  tab_options(data_row.padding = '0px', 
              # table.font.names = "Roboto",
              # table_body.hlines.color = "transparent",
              # column_labels.border.top.color = 'black',
              # column_labels.border.top.width = px(1),
              # column_labels.border.bottom.style = 'none',
              # column_labels.font.weight = "strong",
              # row_group.border.top.style = "none",
              # row_group.border.top.color = "black",
              # row_group.border.bottom.width = px(1),
              # row_group.border.bottom.color = "black",
              # row_group.border.bottom.style = "solid",
              row_group.padding = px(1.5),
              # heading.align = 'center',
              # heading.border.bottom.style = "none",
              # table_body.border.top.style = "none",
              # table_body.border.bottom.color = "white",
              # table.border.bottom.style = 'none',
              table.border.top.style = 'none',
              table.background.color = "#F0FFF0"
              # source_notes.border.lr.style = "none")
  ) %>%
  gt_save_crop("champs_tbl.png", bg = "#F0FFF0")

df <- read_csv("team_salaries.csv")

df_team <- df %>%
  filter(Year == '2025-26') %>%
  mutate(Spent = if_else(!is.na(`Dead Cap`), `Dead Cap` + Active, Active)) %>%
  select(Team, Spent, color, alternate_color, logo)

cap <- 154647000
luxary_tax <- 187895000
first_apron <- 195945000
second_apron <- 207824000
p <- df_team %>%
  ggplot(aes(x = logo, y = Spent)) +
  geom_col(aes(color = alternate_color, fill = color)) +
  geom_hline(yintercept = cap, color = 'pink', size = 0.4, linetype = 'dotted') +
  geom_hline(yintercept = luxary_tax, color = 'purple', size = 0.4, linetype = 'dotted') +
  geom_hline(yintercept = first_apron, color = 'orange', size = 0.4, linetype = 'dotted') +
  geom_hline(yintercept = second_apron, color = 'red', size = 0.4, linetype = 'dotted') +
  geom_text(aes(x = Inf, y = cap + 1, label = "Cap"), 
            hjust = -.1, color = "pink", size = 1.8) +
  geom_text(aes(x = Inf, y = luxary_tax + 1, label = "Tax"), 
            hjust = -.1, color = "purple", size = 1.8) +
  geom_text(aes(x = Inf, y = first_apron + 1, label = "1st Apron"), 
            hjust = -.1, color = "orange", size = 1.8) +
  geom_text(aes(x = Inf, y = second_apron + 1, label = "2nd Apron"), 
            hjust = -.1, color = "red", size = 1.8) +
  coord_cartesian(clip = "off") +
  scale_y_continuous(
    labels = unit_format(unit = "M", scale = 1e-6)
  ) +
  # scale_x_continuous(
  #   expand = expansion(mult = c(0.05, 0.15))
  # ) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_analytics() +
  theme(
    plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 8),
    # turn the logo text urls into images
    axis.text.x = element_path(size = .5),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    # remove vertical gridlines
    panel.grid.major.x = element_blank(),
    # remove legend
    legend.position = 'none',
    plot.margin = unit(c(1, 2, 1, 1), "lines")
  ) +
  labs(title = "Cap Space Going into 2025-26",
      subtitle = paste0("Shows the team salary as of ", formatted_date), 
      x = "",
      y = "Cap Allocated")

ggsave("sal_teams_2025.png", p, w = 6.5, h = 3, dpi = 600)

get_odds <- function(col) {
  team <- gsub(".*?([A-Z]{2,3})\\+.*", "\\1", col)
  data <- gsub(".*(\\+.*)", "\\1", col)
  result <- paste0(team, "_", data)
  return(result)
}
url <- "https://www.espn.com/nba/futures"
espn_odds_page <- read_html_live(url)

df_odds <- espn_odds_page %>%
  html_element("#fittPageContainer > div.pageContent > div.page-container.cf > div > div.layout__column.layout__column--1 > section > div > div > section") %>%
  html_table() %>%
  rename(odds = TEAMODDS) %>%
  mutate(odds = get_odds(odds)) %>%
  separate(odds, into = c("Team", "Odds"), sep = "_", remove = TRUE) %>%
  # Manually cleaning abbreviations I noticed to match
  filter(Team != "TEAMODDS" & Odds != "TEAMODDS") %>%
  mutate(Team = replace(Team, Team == "SA", "SAS")) %>%
  mutate(Team = replace(Team, Team == "TAH", "UTA")) %>%
  mutate(Team = replace(Team, Team == "NY", "NYK")) %>%
  mutate(Team = replace(Team, Team == "WSH", "WAS")) %>%
  mutate(Team = replace(Team, Team == "GS", "GSW")) %>%
  mutate(Team = replace(Team, Team == "NO", "NOP"))

df_team_odds <- left_join(df_team, df_odds, by = "Team")

df_top_10_odds <- df_team_odds %>%
  mutate(numeric_odds = parse_number(Odds)) %>%
  slice_min(order_by = numeric_odds, n = 10)

p <- df_team_odds %>%
  ggplot(aes(x = logo, y = Spent)) +
  geom_col(aes(color = alternate_color, fill = color)) +
  geom_label(data = df_top_10_odds, aes(label = Odds), size = 1.5, vjust = -.25, color = "#A47DAB", fontface = "bold") +
  scale_y_continuous(
    labels = unit_format(unit = "M", scale = 1e-6)
  ) +
  # scale_x_continuous(
  #   expand = expansion(mult = c(0.05, 0.15))
  # ) +
  scale_color_identity() +
  scale_fill_identity() +
  theme_analytics() +
  theme(
    plot.subtitle = element_markdown(size = 6),
    # turn the logo text urls into images
    axis.text.x = element_path(size = .5),
    axis.text.y = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    # remove vertical gridlines
    panel.grid.major.x = element_blank(),
    # remove legend
    legend.position = 'none',
    plot.title = element_markdown(
        # color = 'gray65',
        hjust = 0,
        vjust = 1,
        # margin = margin(10, 10, 15, 10),
        size = 10
      )
    # plot.margin = unit(c(1, 2, 1, 1), "lines")
  ) +
  coord_cartesian(clip = "off") +
  labs(title = "Does the Money Satisfy the <span
      style='color:#A47DAB'>**Odds**?
    </span>",
      subtitle = paste0("Shows the team salary with the top 10 teams predicted odds of winning *The Finals* as of ", formatted_date), 
      x = "",
      y = "Cap Allocated",
      caption = "Source: ESPN and spotrac.com"
      )

ggsave("sal_teams_odds_2025.png", p, w = 6.5, h = 3, dpi = 600)