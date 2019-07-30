#inputs 
library(jsonlite)
num_teams = 12
score_type = tolower('PPR')
year = data.frame(yrs = 2010:2018)

base_url = "https://fantasyfootballcalculator.com/api/v1/adp/"
full_url = paste0(base_url,score_type,"?teams=",num_teams,"&year=")

year_df = year %>% mutate(
  final_url = paste0(full_url, yrs),
  final_lst = purrr::map(final_url, fromJSON),
  final_df = lapply(final_lst,'[[',"players")
)  %>% select(-final_url, -final_lst) %>% tidyr::unnest() %>% filter(yrs!= 2012)