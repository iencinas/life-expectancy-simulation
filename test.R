#https://ourworldindata.org/how-do-the-risks-of-death-change-as-people-age


df.long <- readRDS('data/df.long')
df.wide <- readRDS('data/df.wide')

#playground
p <- df.wide$p
mp <- c(1,cumprod(1-p))
1-p

Borns <- 100000
alive <- round(mp*Borns,0)
d <- alive[1:111]*p
deaths <- -diff(alive)
plot(0:110,deaths,'l')
sum(deaths*0:110)/sum(deaths) #from 0 as age of first death population is 0 ages. I'm not sure if this is correct in terms of demografics, but I stick to it

mp <- c(1,cumprod(1-p))
sum(-diff(mp)*0:110)

#------------
#step 0
#------------

cycle=1

#----

#Statesystem: alive person per cohort and current age
A <- matrix(c(1,Borns,0),nrow=1,ncol=3)
colnames(A) <- c('cohort','alive','age')

#persons that dies per cicle and its age when they died
D <- matrix(c(0,0,0,0),nrow=1,ncol=4)
colnames(D) <- c('cycle','Deaths','age','alive')

A;D

cohort=1:cycle

#deaths = alive + prob of deat[age+1]
Deaths=A[cohort,'alive']*p[A[cohort,'age']+1]

#live people before death
L <- A[cohort,'alive']
#update alive 
A[cohort,'alive']=M[cohort,'alive']-Deaths


#add deaths info of this cycle
D=rbind(D,
        cbind(
          cycle,               #current cyle
          Deaths,              #vector of deaths
          A[cohort,'age'],      #vector of the age per death
          L
        ))

#-------------
#step=step+1
#-------------

#new cicle
cycle=cycle+1
A
#survivors grow one year
A[cohort,'age']=A[cohort,'age']+1
A
#New people are born
A=rbind(A,matrix(c(cycle,Borns,0),nrow=1,ncol=3))
A
#all existing cohorts are affected
cohort=1:cycle

#deaths = alive + prob of deat[age+1]
Deaths=A[cohort,'alive']*p[A[cohort,'age']+1]

#live people before death
L <- A[cohort,'alive']
A_back <- A
#update alive 
A[cohort,'alive']=A[cohort,'alive']-Deaths
A_back
A
L

#add deaths info of this cycle
D=rbind(D,
        cbind(
          cycle,               #current cyle
          Deaths,              #vector of deaths
          A[cohort,'age'],    #vector of the age per death
          L
        ))



A
D
