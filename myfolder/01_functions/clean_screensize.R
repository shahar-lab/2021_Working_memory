library(tidyverse)
library(tidylog)

#screen_size:sub_num,screen_width,screen_height
clean_screensize=screensize%>%select(subj,screen_width,screen_height)

save(clean_screensize,file='myfolder/03_data/02_aggregated_data/screensize_agg.Rdata')