library(nbastatR)
library(dplyr)
library(ggplot2)
library(rvest)
library(stringr)
library(tidyverse)
library(hoopR)
library(gtUtils)
library(extrafont)

font_import()
loadfonts(device = "win")

url <- "https://www.spotrac.com/nba/atlanta-hawks/yearly/_/sort/cap_total/view/roster"

salary_page <- read_html_live(url)

get_data <- function(row) {
  cells <- row %>%
    # Access table data
    html_nodes("td") %>%
    html_text(trim = TRUE)
  return(cells)
}
players_forcast_table <- salary_page %>%
  # Get Active Players table
  html_node(xpath = '//*[@id="dataTable-active"]')

data <- players_forcast_table %>%
  # Get all rows from Active Players table
  html_nodes("tbody tr") %>%
  # apply to each row
  lapply(get_data)

# convert to dataframe
df <- data %>%
  do.call(rbind, .) %>%
  as_tibble(stringsAsFactors = FALSE)

colnames(df) <- players_forcast_table %>%
  html_nodes("thead th") %>%
  html_text(trim = TRUE)

extract_data <- function(column, patterns) {
  # str_detect checks for any specified patterns
  result <- sapply(patterns, function(pattern) str_detect(column, pattern))
  # # will convert the result to a boolean vector if any pattern (i.e greater than zero) was found
  # rowSums() > 0
  result <- as_tibble(result)
  colnames(result) <- patterns
  return(result)
}

clean_data <- function(column, patterns) {
  # combine strings in patterns list to a single string and sepearte with | for regular expressions translation
  data <- column %>%
    str_replace_all(str_c(patterns, collapse = "|"), "")
  data <- gsub("\\$", "", data)
  print(data)
  if (grepl("M", data, ignore.case = TRUE)) {
    # Remove anything after an M
    data <- str_replace(data, "M.*$", "M")
    result <- as.numeric(gsub("M", "", data)) * 1000000
  } else {
    # Remove anything past 3 digits after the last comma
    data <- str_replace(data, "(.*),.*?(\\d{3}).*", "\\1,\\2")
    result <- as.numeric(gsub(",", "", data))
  }
  return(result)
}

extract_cap_hit <- function(column) {
  pattern <- paste0(
  "(?x)",             # Enable free-spacing mode and comments
  "(?<=",             # Positive Look behind
  "M",                # Match 'M'
  "|",                # OR
  ",\\d{3}",          # Match ',' followed by exactly 3 digits
  ")",                # Non-capturing group end
  "(",                # Start of capturing group 1
  "\\d+",             # Match one or more digits
  "(?:",              # Non-capturing group start
  "\\.",              # Match a decimal point
  "\\d+",             # Match one or more digits
  ")?",               # Non-capturing group end, zero or one time
  ")",                # End of capturing group 1
  "\\%",              # Match a percent sign
  "(?-x)"             # Disable free-spacing mode and comments
)
  hit <- str_extract(column, pattern)
  hit <- as.numeric(gsub("%", "", hit)) / 100
  return(hit)
}
extract <- c("Ext. Elig.", "UFA", "RFA")
patterns <- c("Ext. Elig.", "UFA", "RFA", "/")
# Clean Table
df <- df %>%
  rename("Player" = "Player (16)") %>%
  # Match first word (duplicate last name from table) and replace with empty string
  mutate(Player = str_trim(str_replace(Player, "^[\\w-']+\\s", ""))) %>%
  pivot_longer(
    cols = contains("-"),
    names_to = "Year",
    values_to = "Salary")
df <- df %>%
  bind_cols(extract_data(df$Salary, extract)) %>%
  mutate("Cap Hit" = extract_cap_hit(Salary)) %>%
  mutate(Salary = map_dbl(Salary,  ~ clean_data(.x, patterns)))

write_csv(df, "hawks_salaries.csv")

# df <- read_csv("hawks_salaries.csv")

# https://stackoverflow.com/questions/65642125/trying-to-make-a-table-in-r-where-i-group-columns-by-variables-of-a-vector
p <- df %>%
  mutate(Salary = if_else(!is.na(Salary), paste0(round(Salary / 1e6, 2), "M"), "-")) %>%
  rename("Hit" = `Cap Hit`) %>%
  # mutate(across(`Ext. Elig.`:RFA), ~if_else(.x == FALSE, "", as.character(.x))) %>%
  pivot_wider(names_from = Year, values_from = c(Salary, `Ext. Elig.`, UFA, RFA, Hit), names_glue = "{Year}_{.value}") %>%
  gt(rowname_col = "Player") %>%
  # data_color(
  #   columns = Pos,
  #   target_columns = everything(),
  #   palette = c("#DEB64B", "#FFCCCB")
  # )%>%
  fmt_percent(ends_with("Hit"), decimals = 1) %>%
  tab_header(title = md("**Atlanta Hawks Projected Salary Cap Table**"), 
             subtitle = "5-year overview of the salary cap finances for the Atlanta Hawks and their new additions"
  )

# Add nested spanners for each metric
years <- 2025:2030
metrics <- c("Salary", "Ext. Elig.", "UFA", "RFA", "Cap Hit")

for (year in years) {
  sub_year <- year %% 100
  p <- p %>%
    tab_spanner(
      label = year,
      columns = starts_with(glue::glue("{year}-{sub_year + 1}")),
      id = glue::glue("{year}")
    )
}  
rename_cols <- function(col) {
  col <- sub(".*_", "", col)
}

sal_cols <- grep("_Salary", names(p$data))

p <- p %>%
  cols_label_with(
    columns = everything(),
    fn = rename_cols
  ) %>%
  fmt_tf(
    columns = contains(c("Ext. Elig.", "UFA", "RFA")),
    tf_style = 'yes-no',
    colors = c("green", "black")
  )
 p %>%
 tab_style(
    style = cell_borders(
      sides = c("left", "right"),
      color = "black",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(columns = everything())
  ) %>%
  tab_options(data_row.padding = '0px', 
                row_group.padding = px(1.5),
                table.border.top.style = 'none',
                table.background.color = "#F0FFF0"
    ) %>%
  opt_table_font(
    font = "Consolas"
  ) %>%
  tab_source_note(
    source_note = "Data courtesy of Spotrac.com"
  ) %>%
  opt_row_striping() %>%
  gtsave("hawks_sal.png", vwidth = 1800)
# use hoopR package to get headshots for Hawks active players and then use gtextras for table
# Use waffle plot to show how the make up of the Hawks is setup for success (look at teams with similar profile to Hawks like Pacers of last year and categorize players):
# https://r-graph-gallery.com/waffle.html https://thef5.substack.com/p/how-to-win-totals-tables-and-more (NBA Team Roster Composition)
# Can get the top 8 players per min for Indiana and Hawks and then profile players from there. 

p <- df %>%
  filter(Year == "2025-26") %>%
  # mutate(Salary = if_else(!is.na(Salary), round(Salary / 1e6, 1), NA)) %>%
  # mutate(Salary = if_else(!is.na(Salary), paste0(round(Salary / 1e6, 2), "M"), "-")) %>%
  rename("Hit" = `Cap Hit`) %>%
  # mutate(across(`Ext. Elig.`:RFA), ~if_else(.x == FALSE, "", as.character(.x))) %>%
  pivot_wider(names_from = Year, values_from = c(Salary, `Ext. Elig.`, UFA, RFA, Hit), names_glue = "{Year}_{.value}") %>%
  gt(rowname_col = "Player") %>%
  fmt_percent(ends_with("Hit"), decimals = 1) %>%
  tab_header(title = md("**Atlanta Hawks Projected Salary Cap Table**"), 
             subtitle = "2025-26 season overview of the salary cap finances for the Atlanta Hawks and their new additions"
  ) %>%
  cols_label_with(
    columns = everything(),
    fn = rename_cols
  ) %>%
  fmt_number(
    columns = ends_with("_Salary"), 
    # rows = !is.na(ends_with("_Salary")),
    suffixing = TRUE
  ) %>%
  # fmt_currency(
  #   columns = ends_with("_Salary"),
  #   currency = "dollar"
  # ) %>%
  fmt_tf(
    columns = contains(c("Ext. Elig.", "UFA", "RFA")),
    tf_style = 'yes-no',
    colors = c("green", "black")
  ) %>%
  cols_align(
    align = "center",
    columns = contains(c("Ext. Elig.", "UFA", "RFA"))
  ) %>%
  sub_missing(
    columns = ends_with(c("_Salary", "Hit")),
    missing_text = "-"
  ) %>%
  tab_options(data_row.padding = '0px', 
                row_group.padding = px(1.5),
                table.border.top.style = 'none',
                table.background.color = "#F0FFF0"
    ) %>%
  opt_table_font(
    font = "Consolas"
  ) %>%
  tab_source_note(
    source_note = "Data courtesy of Spotrac.com"
  ) %>%
  opt_row_striping() %>%
  gt_save_crop("hawks_sal_2025.png", bg = "#F0FFF0")