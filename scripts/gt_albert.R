library(tidyverse)

# chapter 1 ----
penguins <- palmerpenguins::penguins %>% 
  filter(!is.na(sex))
penguins
penguin_counts <- penguins %>% 
  mutate(year = as.character(year)) %>% 
  group_by(species, island, sex, year) %>% 
  summarise(n = n(), .groups = 'drop')
penguin_counts

penguin_counts_wider <- penguin_counts %>% 
  pivot_wider(
    names_from = c(species, sex), 
    values_from = n
  ) %>% 
  mutate(across(.cols = -(1:2), .fns = ~replace_na(., replace = 0))) %>% 
  arrange(island, year)
penguin_counts_wider

library(gt)
penguin_counts_wider %>% 
  gt() %>% 
  cols_label(
    island = 'Island',
    year = 'Year',
    Adelie_female = 'Adelie (female)',
    Adelie_male = 'Adelie (male)',
    Chinstrap_female = 'Chinstrap (female)',
    Chinstrap_male = 'Chinstrap (male)',
    Gentoo_female = 'Gentoo (female)',
    Gentoo_male = 'Gentoo (male)',
  ) %>% 
  tab_spanner(
    label = md('**Adelie**'),
    columns = 3:4
  ) %>% 
  tab_spanner(
    label = md('**Chinstrap**'),
    columns = c('Chinstrap_female', 'Chinstrap_male')
  ) %>% 
  tab_spanner(
    label =  md('**Gentoo**'),
    columns = contains('Gentoo')
  )

actual_columns <- colnames(penguin_counts_wider)
actual_columns
desired_columns <- actual_columns %>% 
  str_remove('(Adelie|Chinstrap|Gentoo)_') %>% 
  str_to_title()
desired_columns  
names(desired_columns) <- actual_columns
desired_columns

penguin_counts_wider %>% 
  gt() %>% 
  cols_label(.list = desired_columns) %>% 
  tab_spanner(
    label = md('**Adelie**'),
    columns = 3:4
  ) %>% 
  tab_spanner(
    label = md('**Chinstrap**'),
    columns = c('Chinstrap_female', 'Chinstrap_male')
  ) %>%  
  tab_spanner(
    label =  md('**Gentoo**'),
    columns = contains('Gentoo')
  ) %>% 
  tab_header(
    title = 'Penguins in the Palmer Archipelago', 
    subtitle = 'Data is courtesy of the {palmerpenguins} R package'
  ) %>% 
  tab_caption('caption') %>% 
  tab_footnote('footnote') %>% 
  tab_source_note('source note')

spanner_and_header <- function(gt_tbl){
  gt_tbl %>% 
    tab_spanner(
      label = md('**Adelie**'),
      columns = 3:4
    ) %>%  
    tab_spanner(
      label = md('**Chinstrap**'),
      columns = c('Chinstrap_female', 'Chinstrap_male')
    ) %>%  
    tab_spanner(
      label =  md('**Gentoo**'),
      columns = contains('Gentoo')
    ) %>%  
    tab_header(
      title = 'Penguins in the Palmer Archipelago',
      subtitle = 'Data is courtesy of the {palmerpenguins} R package'
    ) 
}

penguin_counts_wider %>% 
  mutate(island = as.character(island), year = as.numeric(year)) %>% 
  gt() %>% 
  cols_label(.list = desired_columns) %>% 
  spanner_and_header()
penguin_counts_wider |> 
  gt() |> 
  cols_label(.list = desired_columns) |> 
  spanner_and_header()  |> 
  cols_align(align = 'right', columns = 'year') |> 
  cols_align(
    align = 'left', 
    columns = where(is.factor)
  )

penguin_counts_wider %>% 
  mutate(island = as.character(island), year = as.numeric(year)) %>% 
  gt(groupname_col = 'island') %>% 
  cols_label(.list = desired_columns) %>% 
  spanner_and_header()

penguin_counts_wider %>%  
  mutate(
    island = as.character(island), 
    year = as.numeric(year),
    island = paste0('Island: ', island)
  ) %>% 
  gt(groupname_col = 'island', rowname_col = 'year') %>% 
  cols_label(.list = desired_columns) %>% 
  spanner_and_header() 

penguin_counts_wider %>%  
  mutate(
    island = as.character(island), 
    year = as.numeric(year),
    island = paste0('Island: ', island)
  ) %>% 
  gt(groupname_col = 'island', rowname_col = 'year') %>% 
  cols_label(.list = desired_columns) %>% 
  spanner_and_header() %>% 
  sub_zero(zero_text = '-')

penguin_counts_wider |> 
  mutate(
    island = as.character(island), 
    year = as.numeric(year),
    island = paste0('Island: ', island)
  ) |> 
  gt(groupname_col = 'island', rowname_col = 'year') |> 
  cols_label(.list = desired_columns) |> 
  spanner_and_header()  |> 
  sub_zero(zero_text = '-') |>
  summary_rows(
    groups = TRUE,
    fns = list(
      'Maximum' = ~max(.),
      'Total' = ~sum(.)
    ),
    formatter = fmt_number,
    decimals = 0
  ) 

penguin_counts_wider |> 
  mutate(
    island = as.character(island), 
    year = as.numeric(year),
    island = paste0('Island: ', island)
  ) |> 
  gt(groupname_col = 'island', rowname_col = 'year') |> 
  cols_label(.list = desired_columns) |> 
  spanner_and_header()  |> 
  sub_zero(zero_text = '-') |>
  summary_rows(
    groups = TRUE,
    fns = list(
      'Maximum' = ~max(.),
      'Total' = ~sum(.)
    ),
    formatter = fmt_number,
    decimals = 0
  )  %>% 
  tab_options(
    data_row.padding = px(2),
    summary_row.padding = px(3), # A bit more padding for summaries
    row_group.padding = px(4)    # And even more for our groups
  ) |> 
  opt_stylize(style = 1, color = 'green') %>% #1, 3, 6 are ok
  gtsave('output/gt_chap1_table.html')

#chapter 2----
library(tidyverse)
library(gt)
gapminder_data <- gapminder::gapminder |> 
  janitor::clean_names() |> 
  select(continent, country, year, life_exp) |> 
  mutate(
    year = as.character(year),
    # Year is really categorical with numeric labels
    country = as.character(country) 
  ) 
gapminder_data

library(gtExtras)
gt_plt_summary(gapminder_data)

selected_countries <- gapminder_data %>% 
  # Filter to use only six years (those that end in 7)
  filter(str_ends(year, '7')) %>% 
  # sample two countries per continent
  group_by(continent, country) %>% 
  nest() %>% 
  group_by(continent) %>% 
  slice_sample(n=2) %>% 
  unnest(data) %>% 
  pivot_wider(names_from = year, names_prefix = 'year', values_from = life_exp)
selected_countries
  

