#add 'probs for reward'
for (row in 1:nrow(cards)){
      trial=cards[row,'trial']
      card_left=cards[row,'card_left']
      card_right=cards[row,'card_right']
      cards[row,"card_left_prob"]=reward_matrix[trial,card_left+1]
      cards[row,"card_right_prob"]=reward_matrix[trial,card_right+1]
}
#add 'better decision'
cards=cards%>%mutate(better_card= ifelse(card_left_prob>card_right_prob,card_left,card_right))

#add is_better_choice
cards=cards%>%mutate(is_better=better_card==ch_card)

cards%>%group_by(block,trial)%>%summarise(accuracy=mean(is_better))%>%ggplot(aes(x=trial,y=accuracy,color=block))+geom_point()+
geom_smooth(aes(color=block))  
x=cards%>%group_by(block,trial)%>%summarise(accuracy=mean(rt))
x$trial=x$trial+(x$block+1)*50
plot(x$trial,x$accuracy)
