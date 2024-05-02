#----


df.long <- readRDS('data/df.long')
df.wide <- readRDS('data/df.wide')

p.i <- (df.long%>%filter(type=='intrinsic'))$p
p.e <- (df.long%>%filter(type=='extrinsic'))$p

Borns <- 10000
#Alive person per cohort and current age
A=matrix(c(0,Borns,-1),nrow=1,ncol=3) 
colnames(A) <- c('cohort','alive','age')


#persons that dies per cicle and its age when they died
D <- matrix(c(0,0,0,0,0,0),nrow=1,ncol=6)
colnames(D) <- c('cycle','Deaths','extrinsic','intrinsic','age','alive')

A;D




n_cycles=150
#need to add 0 to p to avoid problesm
p.i <- c(p.i,rep(0,n_cycles-length(p.i)))
p.e <- c(p.e,rep(0,n_cycles-length(p.e)))

cycle=1
basic_info <- data.frame()
for(cycle in 1:n_cycles){
  #new cicle
  #all existing cohorts are affected
  cohort=1:cycle
  
  #survivors grow one year
  A[cohort,'age']=A[cohort,'age']+1
  
  
  #info 1: average age and alive persons
  tmp_avg_age=sum(A[cohort,'age']*A[cohort,'alive'])/sum(A[cohort,'alive'])
  tmp_alive=sum(A[cohort,'alive'])
  
  #deaths = alive + prob of deat[age+1]
  #only cohorts with live people
  Deaths.i=A[cohort,'alive']*p.i[A[cohort,'age']+1]
  Deaths.e=A[cohort,'alive']*p.e[A[cohort,'age']+1]
  
  Deaths=Deaths.i+Deaths.e
  #info 2: life_exp and total deaths
  tmp_life_exp=sum(Deaths*(A[cohort,'age']))/sum(Deaths) 
  tmp_deaths=sum(Deaths)
  tmp_deaths.e=sum(Deaths.e)  #total extrinsic deaths
  tmp_deaths.i=sum(Deaths.i)  #total intrinsic deaths
  
  
  #live people before death
  L <- A[cohort,'alive']
  
  #update alive 
  A[cohort,'alive']=A[cohort,'alive']-Deaths
  
  
  #add deaths info of this cycle
  #this is unecesary but I keep it just in case
  D=rbind(D,
          cbind(
            cycle,               #current cyle
            Deaths,              #vector of deaths
            Deaths.e,            #extrinsic
            Deaths.i,            #intrinsic
            A[cohort,'age'],      #vector of the age per death
            L                   #alive per age before death update
          ))
  
  #New people are born
  A=rbind(A,matrix(c(cycle,Borns,-1),nrow=1,ncol=3))
  
  #basic info
  basic_info <- rbind(basic_info,data.frame(
    cycle=cycle,
    avg_age=tmp_avg_age,
    alive_persons=tmp_alive,
    avg_live_exp=tmp_life_exp,
    total_deaths=tmp_deaths,
    ext_deaths=tmp_deaths.e,
    int_deaths=tmp_deaths.i
  ))
}


A
D

basic_info

ggplot(basic_info)+geom_line(aes(cycle,avg_live_exp))
ggplot(basic_info)+
  geom_line(aes(cycle,total_deaths))+
  geom_line(aes(cycle,ext_deaths,color='ext'))+
  geom_line(aes(cycle,int_deaths,color='int'))
ggplot(basic_info)+
  geom_line(aes(cycle,total_deaths))+
  geom_line(aes(cycle,ext_deaths,color='ext'))+
  geom_line(aes(cycle,int_deaths,color='int'))+
  scale_y_log10()
head(D)  
