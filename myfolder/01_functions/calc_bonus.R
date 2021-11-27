bonus_2=cards2%>%group_by(subj)%>%summarise(sum(reward))
bonus_3=cards3%>%group_by(subj)%>%summarise(sum(reward))
(bonus_2$`sum(reward)`)*0.003
(bonus_3$`sum(reward)`)*0.003
mean(bonus_2$`sum(reward)`)*0.003
mean(bonus_3$`sum(reward)`)*0.003
range(bonus_2$`sum(reward)`)*0.003
range(bonus_3$`sum(reward)`)*0.003

range(bonus_3$`sum(reward)`+bonus_3$`sum(reward)`)*0.003
