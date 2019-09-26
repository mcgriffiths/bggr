params <- list(
  'sort' = 'numvoters',
  'nosubtypes[0]' = 'boardgameexpansion')

top1000_comments <- map_df(2:10, ~get_comments_search_page(params, .))

top200comments <- get_comments_search_page(params, 2)

top1000comments <- map_df(4:10, ~get_comments_search_page(params, .))

write_csv(top1000_comments, 'top1000_comments.csv')



top1000_comments <- bind_rows(top100_comments, top200comments, top300comments, top1000comments)

top1000_comments %>%
  write_csv('top1000_comments.csv')

top1000_comments %>%
  mutate(has_term = str_detect(comment, regex("\\bcutthroat\\b", ignore_case = T))) %>%
  group_by(id, name) %>%
  summarise(n_term = sum(has_term), pc_term = 100*n_term/n()) %>%
  arrange(desc(pc_term))


top1000_comments %>%
  count(username, sort = T) %>% View()

top1000_comments %>%
  count(username, sort = T)

top1000_comments %>%
  group_by(username) %>%
  mutate(n_comments = n()) %>%
  filter(n_comments < 10) %>%
  ungroup() %>%
  summarise(mean(rating, na.rm = T))

top1000_comments %>%
  ggplot(aes(x = rating)) +
  geom_histogram(binwidth = 1)

top1000_comments %>%
  group_by(username) %>%
  mutate(n_comments = n()) %>%
  filter(n_comments > 300) %>%
  ggplot(aes(x = rating)) +
  geom_histogram(binwidth = 1)

top1000_comments %>%
  group_by(username) %>%
  mutate(n_comments = n()) %>%
  filter(n_comments > 300) %>%
  ungroup() %>%
  group_by(id, name) %>%
  summarise(av_rating = mean(rating, na.rm = T)) %>%
  arrange(desc(av_rating))
