data_geek=geeklaesamp
library(ggpubr)
ggsummarystats(
  data_geek, x = "StatNbhd", y = "Squarefootage", 
  ggfunc = ggboxplot, add = "jitter",
  color = "StatNbhd", palette = "jpg"
)
#No, the plot does not seem to support ICC value computed in part a. The ICC computed in part a is 0.929014885. Based on our
#  plot, we can see that the data points are scattered within a cluster.The clusters also have outliers. In all of the individual neighborhoods,
# we can see that the variation exists. By the ICC computed in part a, the data points in each neighborhood should have been very close, but this
# is not the case as revealed by the boxplot
library(survey)
data_geek$fpc1=7
clustdes= svydesign(id=~StatNbhd, data=data_geek,fpc=~fpc1)
summary(clustdes)
avgsqft=svymean(~Squarefootage,design=clustdes)
avgsqft
avghomeval=svymean(~HouseValue,design=clustdes)
avghomeval
ICC_household_price=0.937833983
ICC_household_avgsqft=0.929014885
#Calculating design effect for homevalue and sqft
Design_eff_avghomeval= 1+ICC_household_price *4
Design_eff_avghomeval
Design_eff_avgsqft=1+ICC_household_avgsqft*4
Design_eff_avgsqft
# ESTIMATing PRICE PER SQFT
theratio=svyratio(~HouseValue,~Squarefootage,clustdes)
theratio
CI_ratio=confint(theratio,level=0.95,df=degf(clustdes))
CI_ratio

