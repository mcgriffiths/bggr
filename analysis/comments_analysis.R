library(dplyr)
library(readr)
library(stringr)

params <- list(
  'sort' = 'numvoters',
  'nosubtypes[0]' = 'boardgameexpansion')

top2000comments <- purrr::map_df(11:20, ~bggr::get_comments_search_page(params, .))

readr::write_csv(top5000comments, 'top5000_comments.csv')

top5000_comments <- read_csv('top5000_comments.csv') %>%
  mutate(id = as.character(id), rating = as.numeric(rating))

top5000_comments <- bind_rows(top2000_comments, top5000_comments)

top1000_comments <- dplyr::bind_rows(top100_comments, top200comments, top300comments, top1000comments)

top5000_comments %>%
  readr::write_csv('top5000_comments.csv')

top5000_comments <- read_csv('top5000_comments.csv') %>%
  mutate(id = as.character(id), rating = as.numeric(rating))

top5000_comments %>%
  filter(str_count(comment, " ") > 10) %>%
  dplyr::mutate(has_term = stringr::str_detect(comment, regex("\\btrick.taking\\b", ignore_case = T))) %>%
  dplyr::group_by(id, name) %>%
  dplyr::summarise(n_term = sum(has_term, na.rm = T), pc_term = 100*n_term/n()) %>%
  dplyr::arrange(desc(pc_term))

top5000_comments %>%
  dplyr::count(username, sort = T)

top5000_comments %>%
  dplyr::group_by(username) %>%
  dplyr::mutate(n_comments = n()) %>%
  dplyr::filter(n_comments > 500) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(mean(rating, na.rm = T))

top5000_comments %>%
  ggplot2::ggplot(aes(x = rating)) +
  ggplot2::geom_histogram(binwidth = 1)

top5000_comments %>%
  group_by(username) %>%
  mutate(n_comments = n()) %>%
  filter(n_comments > 500) %>%
  ggplot(aes(x = rating)) +
  geom_histogram(binwidth = 1)

top5000_comments %>%
  dplyr::group_by(username) %>%
  dplyr::mutate(n_comments = n()) %>%
  dplyr::filter(n_comments > 500) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(id, name) %>%
  dplyr::filter(n() > 50) %>%
  dplyr::summarise(av_rating = mean(rating, na.rm = T)) %>%
  dplyr::arrange(desc(av_rating))

# data subset
comments_subset <- top5000_comments %>%
  filter(str_count(comment, " +") > 10) %>%
  group_by(id) %>%
  filter(n() >= 100) %>%
  ungroup()
  
# functionalise search
search_term <- function(data, term){
  data %>%
    dplyr::mutate(has_term = stringr::str_detect(comment, regex(paste0("\\b",term), ignore_case = T))) %>%
    dplyr::group_by(id, name) %>%
    dplyr::summarise(n_term = sum(has_term, na.rm = T), pc_term = 100*n_term/n()) %>%
    dplyr::arrange(desc(pc_term))
}

comments_subset %>%
  search_term("abstract")

# iterate to create big term df
terms <- list("abstract" = "abstract",
              "wargame" = "war.?game",
              "ameritrash" = "ameritrash",
              "euro" = "euro\\b",
              "party" = "party",
              "traditional" = "traditional",
              "bluffing" = "bluffing",
              "deduction" = "deduction",
              "dexterity" = "dexterity",
              "negotiation" = "negotiation",
              "socialdeduction" = "social.?deduction",
              "speed" = "speed.?game",
              "trivia" = "trivia\\b",
              "word" = "word.?game",
              "areamajority" = "area.?majority",
              "deckbuilding" = "deck.?building",
              "drafting" = "drafting",
              "enginebuilding" = "engine",
              "investment" = "investment",
              "network" = "network",
              "pickup" = "pick.?up",
              "workerplacement" = "worker.?placement",
              "rummy" = "rummy",
              "memory" = "memory",
              "patternrecognition" = "pattern.?recognition",
              "pushyourluck" = "push.?your.?luck",
              "setcollection" = "set.?collection",
              "racing" = "racing",
              "tilelaying" = "tile.?laying",
              "trading" = "trading",
              "tricktaking" = "trick.?taking",
              "rpg" = "rpg",
              "dudesonamap" = "dudes.?on.?a.?map|doam",
              "hexandcounter" = "hex.+counter",
              "simulation" = "simulation",
              "specialpower" = "special.?power",
              "takethat" = "take.?that",
              "auction" = "auction.?game",
              "civ" = "civ.?game",
              "coop" = "co.?op\\b",
              "partnership" = "partnership", 
              "solo" = "solo\\b",
              "traitor" = "traitor"
              )

term_df <- purrr::map_df(terms, ~search_term(comments_subset, .), .id = 'term')

# scale
term_df <- term_df %>%
  group_by(term) %>%
  mutate(pc_term = pc_term/max(pc_term))

term_df_bin <- term_df %>%
  mutate(pc_term = ifelse(n_term > 5, pc_term, 0))

# summarise by cosine similarity

find_sim <- function(data, game_id){
  probe <- data %>%
    filter(id == game_id) %>% 
    pull(pc_term)
  
  data %>%
    group_by(id, name) %>%
    summarise(sim = lsa::cosine(pc_term, probe)) %>%
    arrange(desc(sim))
}

find_sim(term_df_bin, "42") %>% View()

term_df %>% 
  group_by(id, name) %>% 
  filter(pc_term > 0.1) %>% 
  filter(n() > 4) %>% 
  arrange(name) %>% View()

