library(mvtnorm)
library(mirt)    
library(logitnorm)

#Simulation to capture person fit statistics
gen<-function(N,items,reps,slps){
  factors<-4
  items=items
  
  #matrix to capture the simulation output
  res<-matrix(NA,ncol=9,nrow=reps)
  
  for(i in 1:reps){
    #Print Reps
    cat('\n')
    cat("rep ", i,'\n')
    
    seed<-round(runif(1,1000,9990),0)
    set.seed(seed)
    
    #Create the Variance Covariance Matrix for 6 items
    sigma<-diag(factors)
    
    theta=as.matrix(rmvnorm(n=N,mean=c(rep(0,factors)),sigma=sigma))
    
    #General slope parameter always taken from this
    a.gent<-rlnorm(items*3,0,.5)# runif(items*3, 1.09, 2.19)
    a.s<-rlnorm(items,0,.5)
    
    #Create Matrix for slopes
    a.gen<-matrix(a.gent)
    a.s1<-matrix(c(a.s,rep(NA,items),rep(NA,items)))
    a.s2<-matrix(c(rep(NA,items),a.s,rep(NA,items)))
    a.s3<-matrix(c(rep(NA,items),rep(NA,items), a.s))
    a<-cbind(a.gen,a.s1,a.s2,a.s3)
    
    
    d<-matrix(rnorm(items*3))
    data<-simdata(a,d,N,itemtype='dich',Theta=theta,sigma=sigma)
    #TRUE MODEL
    mod1 <- bfactor(data, c(rep(1,items),rep(2,items),rep(3,items)), itemtype='2PL',verbose=F)
    print(mod1@OptimInfo$converged)
    if (mod1@OptimInfo$converged == TRUE){
      #3Factor EFA
      mod2 <- mirt(data,3,itemtype='2PL',verbose=F)
      if (mod2@OptimInfo$converged == TRUE){
        print(mod2@OptimInfo$converged)
        #2Factor EFA
        mod3 <- mirt(data,2,itemtype='2PL',verbose=F)
        if (mod3@OptimInfo$converged == TRUE){
          print(mod3@OptimInfo$converged)
          #UNIDIM Model
          mod4 <- mirt(data,1,itemtype='2PL',verbose=F)
          if (mod4@OptimInfo$converged == TRUE){
            print(mod4@OptimInfo$converged)
            
            #m1_scores<-fscores(mod1,full.scores=TRUE,full.scores.SE=TRUE,QMC=TRUE)
            m1_scores<-fscores(mod1,full.scores=TRUE,full.scores.SE=TRUE,QMC=TRUE)
            m2_scores<-fscores(mod2,full.scores=TRUE,full.scores.SE=TRUE,QMC=TRUE)
            m3_scores<-fscores(mod3,full.scores=TRUE,full.scores.SE=TRUE,QMC=TRUE)
            m4_scores<-fscores(mod4,full.scores=TRUE,full.scores.SE=TRUE,QMC=TRUE)
            
            pfit1<-personfit(mod1,QMC=TRUE) 
            flagged1<-sum(as.matrix(pfit1)<=-1.96)
            
            pfit2<-personfit(mod2,QMC=TRUE) #Normal
            flagged2<-sum(as.matrix(pfit2)<=-1.96)
            
            pfit3<-personfit(mod3,QMC=TRUE) #Normal
            flagged3<-sum(as.matrix(pfit3)<=-1.96)
            
            pfit4<-personfit(mod4,QMC=TRUE) #Normal
            flagged4<-sum(as.matrix(pfit4)<=-1.96)
            
            res[i,1]<-flagged1
            res[i,2]<-flagged2
            res[i,3]<-flagged3
            res[i,4]<-flagged4
            
            #Capture convergence Rates. 
            res[i,6]<-mod1@OptimInfo$converged
            res[i,7]<-mod2@OptimInfo$converged
            res[i,8]<-mod3@OptimInfo$converged
            res[i,9]<-mod4@OptimInfo$converged
          }
          else {print("Next Cell on 4")}
        }
        else {print("Next Cell on 3")}
      }
      else {print("Next Cell on 2")}
    }
    else {print("Next Cell on 1")}
    
  }
  return(res)
  
}

debug(gen)
testcell<-gen(N=300,items=10,reps=3)

#Ruben
#cell1 <-gen(N= 200,items= 5,reps=3)
#cell2 <-gen(N= 200,items=10,reps=3)
cell3.1 <-gen(N= 200,items=15,reps=100);save.image()
cell3.2 <-gen(N= 200,items=15,reps=100)
cell3.3 <-gen(N= 200,items=15,reps=100)
cell3.4 <-gen(N= 200,items=15,reps=100)
cell3.5 <-gen(N= 200,items=15,reps=100);save.image()
cell3.6 <-gen(N= 200,items=15,reps=100)
cell3.7 <-gen(N= 200,items=15,reps=100)
cell3.8 <-gen(N= 200,items=15,reps=100)
cell3.9 <-gen(N= 200,items=15,reps=100)
cell3.10 <-gen(N= 200,items=15,reps=100);save.image()

#cell4 <-gen(N= 200,items=20,reps=3)
#cell5 <-gen(N= 500,items= 5,reps=3)
cell6.1 <-gen(N= 500,items=10,reps=100)
cell6.2 <-gen(N= 500,items=10,reps=100)
cell6.3 <-gen(N= 500,items=10,reps=100)
cell6.4 <-gen(N= 500,items=10,reps=100);save.image()
cell6.5 <-gen(N= 500,items=10,reps=100)
cell6.6 <-gen(N= 500,items=10,reps=100)
cell6.7 <-gen(N= 500,items=10,reps=100)
cell6.8 <-gen(N= 500,items=10,reps=100);save.image()
cell6.9 <-gen(N= 500,items=10,reps=100)
cell6.10 <-gen(N= 500,items=10,reps=100);save.image()

#cell7 <-gen(N= 500,items=15,reps=3)
#cell8 <-gen(N= 500,items=20,reps=3)
cell9.1 <-gen(N=1000,items= 5,reps=100)
cell9.2 <-gen(N=1000,items= 5,reps=100)
cell9.3 <-gen(N=1000,items= 5,reps=100)
cell9.4 <-gen(N=1000,items= 5,reps=100);save.image()
cell9.5 <-gen(N=1000,items= 5,reps=100)
cell9.6 <-gen(N=1000,items= 5,reps=100)
cell9.7 <-gen(N=1000,items= 5,reps=100)
cell9.8 <-gen(N=1000,items= 5,reps=100);save.image()
cell9.9 <-gen(N=1000,items= 5,reps=100)
cell9.10 <-gen(N=1000,items= 5,reps=100)

#cell10<-gen(N=1000,items=10,reps=3)
#cell11<-gen(N=1000,items=15,reps=3)
cell12.1<-gen(N=1000,items=20,reps=100)
cell12.2<-gen(N=1000,items=20,reps=100);save.image()
cell12.3<-gen(N=1000,items=20,reps=100)
cell12.4<-gen(N=1000,items=20,reps=100)
cell12.5<-gen(N=1000,items=20,reps=100);save.image()
cell12.6<-gen(N=1000,items=20,reps=100)
cell12.7<-gen(N=1000,items=20,reps=100)
cell12.8<-gen(N=1000,items=20,reps=100);save.image()
cell12.9<-gen(N=1000,items=20,reps=100)
cell12.10<-gen(N=1000,items=20,reps=100);save.image()

cell12.11<-gen(N=1000,items=20,reps=100)
cell12.12<-gen(N=1000,items=20,reps=100);save.image()
cell12.13<-gen(N=1000,items=20,reps=100)
cell12.14<-gen(N=1000,items=20,reps=100)
cell12.15<-gen(N=1000,items=20,reps=100);save.image()
cell12.16<-gen(N=1000,items=20,reps=100)
cell12.17<-gen(N=1000,items=20,reps=100)
cell12.18<-gen(N=1000,items=20,reps=100);save.image()
cell12.19<-gen(N=1000,items=20,reps=100)
cell12.20<-gen(N=1000,items=20,reps=100);save.image()


