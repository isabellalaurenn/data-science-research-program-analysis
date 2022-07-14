## practice plotting

##practice subsetting data
#use a combination of...
#filter, select, mutate, arrange, summarize, group_by, 
#rename, sample, and/or slice

#create a visualization using your new subset of data

cerealData %>% 
  select(calories_content) %>% 
  ggplot(aes(x = calories_content)) + geom_bar()

cerealData %>%
  filter(calories_content <= 100) %>%
  ggplot(aes(x = calories_content, y = grain_name)) +
  geom_point()



