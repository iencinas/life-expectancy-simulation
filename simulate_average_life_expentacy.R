#----


df.long <- readRDS('data/df.long')
df.wide <- readRDS('data/df.wide')

p <- df.wide$p
Borns <- 10000
#Statesystem: aliveperson per cohort and current age
A=matrix(c(0,Borns,-1),nrow=1,ncol=3) 
colnames(A) <- c('cohort','alive','age')

#persons that dies per cicle and its age when they died
D <- matrix(c(0,0,0,0),nrow=1,ncol=4)
colnames(D) <- c('cycle','Deaths','age','alive')

A;D



n_cycles=150
#need to add 0's to p to avoid problems, i know i'll never fix this
p <- c(p,rep(0,n_cycles-length(p)))

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
  Deaths=A[cohort,'alive']*p[A[cohort,'age']+1]
  
  
  #info 2: life_exp and total deaths
  tmp_life_exp=sum(Deaths*(A[cohort,'age']))/sum(Deaths) 
  tmp_deaths=sum(Deaths)
  
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
            A[cohort,'age'],    #vector of the age per death
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
    total_deaths=tmp_deaths
    ))
}


  basic_info
  

#plot
  color1='#E69F00'
  color2='#3F61A6'
  color3='#E600BA'
  color4="#B16214"
  
 
plot1 <- ggplot(basic_info)+
  geom_hline(aes(yintercept=77.24),color=color4,linetype='dashed')+
  geom_line(aes(cycle,avg_live_exp),linewidth=1)+
  geom_text(aes(20,72,label="Live Expentancy: 77.24"),color=color4)+
  theme_classic() +
  labs(x="Cycle",y="Life Expentancy") +
  theme(strip.background = element_blank()) 

plot1
help(grid.arrange)
