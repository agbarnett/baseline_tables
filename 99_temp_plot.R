# quickly check a few results
selected = filter(for_model, pmcid %in% c('7115121','6945612','23432189','6329241')) %>%
  mutate(num = as.factor(pmcid))
plot_select = ggplot(data=selected, aes(x=num, y=t, col=factor(statistic)))+
  geom_point()+
  theme_bw()
plot_select
