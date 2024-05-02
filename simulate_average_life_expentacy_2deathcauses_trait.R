library(gridExtra)
#----


df.long <- readRDS('data/df.long')
df.wide <- readRDS('data/df.wide')

group.colors <- c("alldeaths" = "black", 
                  "intrinsic" = "#4C6A9C", 
                  "extrinsic" ="#B16214")
plot <- ggplot(df.long)+
  geom_line(aes(x=age, y=p, color=type,linetype=type),linewidth=1) +
  theme_classic() +
  labs(title = "Death rate per age") +
  ylab("") + xlab("Age")+
  theme(strip.background = element_blank(),
        legend.key.width = unit(2, "line"),
        legend.position=c(.2,.8)) +
  scale_x_continuous(breaks = c(seq(0, 100, by=20),110)) +
  scale_linetype_manual(values = c("alldeaths" = "solid", 
                                   "intrinsic" = "dashed", 
                                   "extrinsic" = "dashed")
                        ,labels=c('All','Intrinsic','Extrinsic'),name='Type') +
  scale_color_manual(values=group.colors,labels=c('All','Intrinsic','Extrinsic'),name='Type') +
  scale_y_continuous(labels = comma, trans='log2', breaks = c(0.0001,0.001,0.01,1)) 

plot

#intrinsic and extrinsic death ratios per age
p.i <- (df.long%>%filter(type=='intrinsic'))$p
p.e <- (df.long%>%filter(type=='extrinsic'))$p


set.seed(1)
Borns <- function(n=1,size=100000,sd=100) return(size) #rnorm(1,100000,sd=100)
Borns()

#Statesystem: alive person per cohort and current age
A=matrix(c(0,Borns(),-1,0),nrow=1,ncol=4) 
colnames(A) <- c('cohort','alive','age','time_after_trait')

#Data: persons that dies per cicle and its age when they died
D <- matrix(c(0,0,0,0,0,0,0),nrow=1,ncol=7)
colnames(D) <- c('cycle','Deaths','extrinsic','intrinsic','age','alive','trait_active')

A;D

#more step by step data
basic_info <- data.frame()

n_cycles=250  #total cycles to simulate
t_change=130  #when the change quik in

trait_active=0

p.i.coeff=(1+0.0015)  #increase in intrinsic probability during treatment
p.e.impact=0.5        #decrease in extrinsic probability during treatment
p.e.coeff=1           #default coef, will be p.e.impact when treatment active

#need to add 0 to p to avoid problesm
p.i <- c(p.i,rep(0,n_cycles-length(p.i)+1))
p.e <- c(p.e,rep(0,n_cycles-length(p.e)+1))

cycle=1
for(cycle in 1:n_cycles){
  #new cicle
  #all existing cohorts are affected
  cohort=1:cycle
  
  #survivors grow one year
  A[,'age']=A[,'age']+1
  
  
  #info 1: average age and alive persons
  tmp_avg_age=sum(A[,'age']*A[,'alive'])/sum(A[,'alive'])
  tmp_alive=sum(A[,'alive'])
  
  #is change time?
  if(cycle>t_change){
    p.e.coeff = p.e.impact
    A[,'time_after_trait']=A[,'time_after_trait']+1
    trait_active=1
  }
  
  #deaths = alive + prob of deat[age+1]
  #only cohorts with live people
  #avoid probs>1
  p.i.temp=pmin(rep(1,length(p.i[A[cohort,'age']+1])),p.i[A[cohort,'age']+1]*(p.i.coeff^A[,'time_after_trait']))
  Deaths.i=A[cohort,'alive']*p.i.temp
  
  p.e.temp=p.e[A[cohort,'age']+1]*p.e.coeff
  Deaths.e=A[cohort,'alive']*p.e.temp
  

  #alternative:
  #
  # p.i.temp=p.i[A[,'age']+1]
  # Deaths.i=rbinom(length(p.i.temp),ceiling(A[,'alive']),p.i.temp*(p.i.coeff^A[,'time_after_trait']))
  # Deaths.i[is.na(Deaths.i)] <- 0 #to fix p=0
  # 
  # p.e.temp=p.e[A[,'age']+1]
  # Deaths.e=rbinom(length(p.e.temp),ceiling(A[,'alive']),p.e.temp*p.e.coeff)
  # Deaths.e[is.na(Deaths.e)] <- 0
  # 

  
  Deaths=Deaths.i+Deaths.e
  #info 2: observed life_exp and total deaths
  tmp_life_exp=sum(Deaths*(A[,'age']))/sum(Deaths)
  tmp_deaths=sum(Deaths)
  tmp_deaths.e=sum(Deaths.e)
  tmp_deaths.i=sum(Deaths.i)

  #expected live at birth
  #reverse the vector so they are h(t) agin, compute F=1-S (this happens because p.i.temp=p.i['age'] and 'age' is added "reversed")
  mp <- c(1,cumprod(1-(rev(p.e.temp)+rev(p.i.temp)))) 
  tmp_life_exp_at_born= sum(-diff(mp)*0:110)   #and  diff=integral for discrete values to compute average value. The 0:110 is because i know  max age is 110
  
  
  #live people before death
  L <- A[,'alive']
  #update alive 
  A[,'alive']=A[,'alive']-Deaths
  
  
  #add deaths info of this cycle
  #this is unecesary but I keep it just in case
  D=rbind(D,
          cbind(
            cycle,               #current cyle
            Deaths,              #vector of deaths
            Deaths.e,            #extrinsic
            Deaths.i,            #intrinsic
            A[,'age'],      #vector of the age per death
            L,                   #alive per age before death update
            trait_active   #is trait active
          ))
  
  #New people are born
  A=rbind(A,matrix(c(cycle,Borns(),-1,0),nrow=1,ncol=4))
  
  #basic info
  basic_info <- rbind(basic_info,data.frame(
    cycle=cycle,
    avg_age=tmp_avg_age,
    alive_persons=tmp_alive,
    obs_live_exp=tmp_life_exp,
    total_deaths=tmp_deaths,
    ext_deaths=tmp_deaths.e,
    int_deaths=tmp_deaths.i,
    avg_life_exp_at_born=tmp_life_exp_at_born
  ))
}

tail(A)
tail(D)

basic_info

#


#plot
color1='#E69F00'
color2='#3F61A6'
color3='#E600BA'
color4="#B16214"


p1 <- ggplot(basic_info%>%filter(cycle>=120,cycle<=137))+
  geom_hline(aes(yintercept=77.24,color='default'),linewidth=1)+
  geom_line(aes(cycle,obs_live_exp,color='treatment'),linewidth=1)+
  geom_vline(aes(xintercept=130),linetype='dashed')+
  geom_label(aes(132.5,78.3,label='Treatement start'))+
  theme_classic() +
  labs(title= "AB test on life expectancy",
       subtitle= "Default vs Treatmen= Recesion",
      x="Cycle",y="Life Expentancy",color='a') +
  scale_color_manual(name='AB test', values=c('black',color1))+
  theme(strip.background = element_blank())+
  theme(legend.position=c(.2,.75)) +
  coord_cartesian(ylim=c(77,78.4))+
  scale_x_continuous(labels = comma, breaks = seq(110,150,by=5))+
  scale_y_continuous(breaks = seq(76,80,by=1)) 


p1

p2 <- ggplot(basic_info%>%filter(cycle>=120))+
  geom_hline(aes(yintercept=77.24,color='default'),linewidth=1)+
  geom_line(aes(cycle,obs_live_exp,color='treatment'),linewidth=1)+
  geom_vline(aes(xintercept=130),linetype='dashed')+
  geom_vline(aes(xintercept=140),linetype='dashed')+
  geom_label(aes(132.5,78.3,label='Treatement start'),hjust=-0.1)+
  theme_classic() +
  labs(title= "AB test on life expectancy",
       subtitle= "Default vs Treatmen= Recesion",
       x="Cycle",y="Life Expentancy",color='a') +
  scale_color_manual(name='AB test', values=c('black',color1))+
  theme(strip.background = element_blank())+
  theme(legend.position=c(.8,.75)) +
  coord_cartesian(ylim=c(77,78.4))+
  scale_x_continuous(labels = comma, breaks = seq(120,300,by=20))+
  scale_y_continuous(breaks = seq(76,80,by=1)) 

p2
  
p3 <- ggplot(basic_info%>%filter(cycle>=120))+
  geom_line(aes(cycle,obs_live_exp,color='Observed Life expectancy'),linewidth=1)+
  geom_line(aes(cycle,avg_life_exp_at_born,color='Life expectancy at born'),linewidth=1,linetype='dashed')+
  theme_classic() +
  labs(title= "Life expectancy",
       subtitle= "Theorical (at born) vs Observed",
       x="Cycle",y="Life Expentancy",color='a') +
  scale_color_manual(name='', values=c('black',color1))+
  theme(strip.background = element_blank())+
  theme(legend.position=c(.8,.75)) +
  coord_cartesian(ylim=c(77,78.4))+
  scale_x_continuous(labels = comma, breaks = seq(120,300,by=20))+
  scale_y_continuous(breaks = seq(76,80,by=1)) 

p3
