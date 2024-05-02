library(tidyverse)
library(reshape2)

mx_age <- readRDS('data/mx_age.rds')

#------------------------------------------------
#a lot of dirty deeds done dirt cheap so relax
#------------------------------------------------

df <- dcast(mx_age,Age+Population~ICD_chapter,value.var = 'Deaths')
colnames(df) <- c('age','population','alldeaths','intrinsic','extrinsic')

#Intrinsic + Extrinsic should add to all death causes
df%>%group_by(alldeaths==intrinsic+extrinsic)%>%summarise(n())
df$intrinsic=with(df,alldeaths-extrinsic)

#fill NA population to 99 years, so data looks nice (log)
#prob of death
df$p <- with(df,alldeaths/population)
#fit p of death for NA population only with the ages that has a log progression
m <- lm(log(p)~age+I(age*age),data=df%>%filter(!is.na(population),age>25))

#check if it looks nice
temp=exp(predict(object = m,newdata=df))
#looks nice indeed
ggplot(df)+geom_line(aes(age,temp,color='fit'))+geom_line(aes(age,p))+scale_y_log10()

#fill missing probs
df$p <- with(df,ifelse(is.na(p),temp,p))
#tail(df,n=30)
#now, at age 110 no survivors, sorry
df.temp <- data.frame(
  age=101:110,
  population=NA,
  alldeaths=rep(df[101,'alldeaths'],10),
  intrinsic=rep(df[101,'intrinsic'],10),
  extrinsic=rep(df[101,'extrinsic'],10),
  p=seq(from=df[101,'p'],to=1,length.out =11)[-1]
)
df <- rbind(df,df.temp)
#fill NA population with an estimate using prob of deaths
df$population <- with(df,ifelse(is.na(population),alldeaths/p,population))

#looks nice to me
ggplot(df)+
  geom_line(aes(x=age, y=alldeaths/population, color='all')) +
  geom_line(aes(x=age, y=intrinsic/population, color='intrinsic')) +
  geom_line(aes(x=age, y=extrinsic/population, color='extrinsic')) +
  scale_y_log10()

#I never know wich version will be better: long or wide so I always do both.
df.wide <- df
df.long <- melt(df[,-which(names(df) == "p")],id.vars = c('age' ,'population'))
head(df.long)
colnames(df.long) <- c('age','population','type','deaths')
df.long$p=with(df.long,deaths/population)  

ggplot(df.long)+
  geom_line(aes(x=age, y=p, color=type)) +
  scale_y_log10()

saveRDS(df.long,file='data/df.long')
saveRDS(df.wide,file='data/df.wide')
