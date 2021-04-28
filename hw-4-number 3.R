data_geek=geeklaesamp
library(ggpubr)
ggsummarystats(
  data_geek, x = "StatNbhd", y = "Squarefootage", 
  ggfunc = ggboxplot, add = "jitter",
  color = "StatNbhd", palette = "jpg"
)

library(survey)
data_geek$fpc1=7
clustdes= svydesign(id=~StatNbhd, data=data_geek,fpc=~fpc1)
summary(clustdes)
avgsqft=svymean(~Squarefootage,design=clustdes, deff=TRUE)
avgsqft
avghomeval=svymean(~HouseValue,design=clustdes,deff=TRUE)
avghomeval
theratio=svyratio(~HouseValue,~Squarefootage,clustdes)
theratio
CI_ratio=confint(theratio,level=0.95,df=degf(clustdes))
CI_ratio

