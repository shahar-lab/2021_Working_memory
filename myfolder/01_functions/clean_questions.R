library(tidyverse)
library(tidylog)

#questionnaires:sub_num,response
clean_question=questions%>%select(subj,response)

save(clean_question,file='myfolder/03_data/02_aggregated_data/question_agg.Rdata')