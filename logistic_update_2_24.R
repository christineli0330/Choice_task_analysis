## logistic plot for individual in delta value
choice.beta = data.frame(ID=double(),
                         beta=double(),
                         p=double(),
                         stringsAsFactors=FALSE)[1:length(id.filtered),]

for (i in 1:length(id.filtered)) {
  d =  choice %>% filter(ID == id.filtered[i], is.na(choseright)==FALSE)
  logistic = glm(choseright ~ 1 + delta.value, data = d, family = "binomial")
  beta = summary(logistic)[['coefficients']][2,1]
  p = summary(logistic)[['coefficients']][2,4]
  choice.beta$ID[i] = id.filtered[i]
  choice.beta$beta[i] = beta
  choice.beta$p[i] = p
}

filtered.b = choice.beta %>% filter(beta>0, p<0.05)
#filtered.b = choice.beta %>% filter(p<0.05, beta>0)

choice = filter(choice, (ID %in% filtered.b$ID | ID %in% c(10, 56)))

choice %>% group_by(ID) %>% ggplot(aes(x = delta.value, y = choseright, group = ID)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  facet_wrap(~ID)


## Plot trends of their rating task

rid =  rating %>% filter(ID == choice.rtid$ID[1])
plot(rid$trial_index, rid$response, '-s')

rating = rating %>% mutate(response = as.numeric(response))
rating %>% group_by(ID) %>% ggplot(aes(x = trial_index, y = response, group = ID)) + 
  geom_point(color = "steelblue", size = 0.7) + geom_line(color = "steelblue3", alpha = 0.5) + 
  facet_wrap(~ID)

##test plot for old subjects
rat.old = read.csv("food-choice-batch-2-12.csv") %>% filter(ttype == 'rating_task')
rat.old$ID = cumsum(!duplicated(rat.old['run_id']))
rat.old %>% group_by(ID) %>% ggplot(aes(x = trial_index, y = response, group = ID)) + 
  geom_point(color = "steelblue", size = 0.7) + geom_line(color = "steelblue3", alpha = 0.5) + 
  facet_wrap(~ID)


rating %>%  group_by(ID) %>% dplyr::summarise(rt = mean(as.numeric(rt)))


# try z-scored ratings
rating = rating %>% group_by(ID) %>% mutate(z = scale(response))
choice = choice %>% dplyr::group_by(ID) %>% dplyr::mutate(z.delta.value = scale(delta.value))
                    

choice %>% group_by(ID) %>% ggplot(aes(x = z.delta.value, y = choseright, group = ID)) + 
  geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)+
  facet_wrap(~ID)





## test the distribution of positions for more valuable items
position = choice %>% dplyr::group_by(ID) %>% dplyr::mutate(posi = case_when(delta.value>0 ~ 1, delta.value<0 ~ 0, delta.value ==0 ~ 2))
x = position %>% dplyr::group_by(ID) %>% filter(posi != 2) %>% dplyr::summarize(p = mean(posi))

position %>% dplyr::group_by(ID) %>% dplyr::filter(posi == 2) %>% dplyr::summarize(p = mean(choseright))


### multilevel model mixed-effects
m1 = glmer(choseright~1+delta.mem+delta.value+(1+delta.mem+delta.value|ID), 
           data = choice, family = "binomial")
summary(m1)

m2 = glmer(choseright~1+delta.mem+(1+delta.mem|ID), 
           data = choice, family = "binomial")
