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

top5000_comments %>%
  dplyr::mutate(has_term = stringr::str_detect(comment, regex("\\btigris\\b", ignore_case = T))) %>%
  dplyr::group_by(id, name) %>%
  dplyr::summarise(n_term = sum(has_term), pc_term = 100*n_term/n()) %>%
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
