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


cerealData %>% 
  filter(Producer == "K") %>%
  ggplot(aes(x = calories_content, y = Producer)) +
  geom_violin(color = "pink", fill = "pink") + 
  labs(title = "Quantity of Calories in K Producer")

cerealData%>%
  sample_n(10) %>%
  arrange(grain_name) %>%
  ggplot(aes(x = calories_content, y = fat_content)) +
  geom_point(color = "purple") 
  #to change the scale of a graph,
  #scale_x_continuous(breaks = seq(min, max, step))

cerealData_small <- filter(cerealData, Producer == "K")
cerealData_small%>%
  summarize(mean_cals = mean(calories_content)) 

#cerealData%>%
  #mutate()


#practice using T tests
pop_mean_cereal <- mean(cerealData$calories_content)
honeynutcheerios <- filter(cerealData, grain_name == "Honey Nut Cheerios")
t.test(honeynutcheerios$calories_content, mu = pop_mean)

