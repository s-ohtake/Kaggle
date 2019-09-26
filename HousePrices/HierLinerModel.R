HierLinearModel<-R6Class(
  classname="HierLinearModel",
  portable = FALSE,
  cloneable = FALSE,
  
  public  = list (
    R          = NA,  # iteration parameters 
    burn.in    = NA,  # burn-in  is Percentage ex(sample * burn.in)
    ind        = NA,  # index name
    reg        = NA,  # index vector
    nreg       = NA,  # index count
    out        = NA,  # bayesm result
    tr         = NA,  # training data
    y.n        = NA,  # dependent variable  names
    x.n        = NA,  # features names
    beta.d     = NA,  # beta data
    beta.n     = NA,  # beta  names
    regdata    = list(),
    
    # constractor		
    Run = function(R=2000,K=1,burn.in=0.4,ind="reg_channel",tr,y.n,x.n,set.Z=F,
                   z.mean=0,z.sd=1) {
      R <<- R
      burn.in <<- burn.in	
      ind <<- ind
      if (!missing(tr)) tr <<- tr
      if (!missing(y.n)) y.n <<- y.n
      if (!missing(x.n)) x.n <<- x.n
      
      # Priors:
      #	 tau_i ~ nu.e*ssq_i/ﾏ・2_{nu.e}. tau_i is the variance of e_i.
      #  beta_i ~ N(ZDelta[i,],V_{beta}). 
      # Note: ZDelta is the matrix Z * Delta; [i,] refers to ith row of this product.
      #  vec(Delta) given V_{beta} ~ N(vec(Deltabar),V_{beta} (x) A^{-1}).
      #  V_{beta} ~ IW(nu,V). 
      #  Delta, Deltabar are nz x nvar. A is nz x nz. V_{beta} is nvar x nvar. 
      # Note: if you don't have any z vars, set Z=iota (nreg x 1).
      set.seed(1)
      
      reg <<-unique(tr[,ind])
      nreg<<-length(reg)	
      
      print('set nreg')
      
      # set variables
      for (j in 1:nreg) {
        #tau=0.1*runif(1,min=0.5,max=1)
        #tau<-runif(1,min=0,max=100000000)
        eval(parse(text=paste0("y=tr[tr$",ind,"==reg[j],y.n]")))	
        
        iota=c(rep(1,length(y)))
        eval(parse(text=paste0("X=as.matrix(cbind(iota,tr[tr$",ind,"==reg[j],x.n]))")))
        #regdata[[j]] <<-list(y=y,X=X,tau=tau)
        regdata[[j]] <<-list(y=y,X=X)
        
      }
      if (set.Z){
        Z=matrix(rnorm(nreg, z.mean, z.sd),ncol=1)
        Data <- append(Data, list(Z=Z))
      }else{
        Data=list(regdata=regdata)
      }
      #Mcmc=list(R=R,keep=K)
      Mcmc=list(R=R)
      
      out <<- rhierLinearModel(Data=Data,Mcmc=Mcmc)
      private$BetaCalc()
    },
    Predict = function(test=NA){
      calc.d<-merge(tr,beta.d)	
      pred.d<-private$PredictDataSet(calc.d)
      R2<-private$CoefDetermCalc(pred.d)
      #  When the test data is set 
      test.pred<-data.frame()
      if(is.data.frame(test)){
        print("No 1 set test data!")
        test.d<-test%>%inner_join(self$beta.d)
        test.pred<-private$PredictDataSet(test.d)
        
        print(paste('test row=>',nrow(test),'merge row =>',nrow(test.d),'pred row=>',nrow(test.pred)))
      }
      return(list(pred=pred.d,R2=R2,test=test.pred))
    }
  ),
  private=list(
    BetaCalc =function(){
      # calclation beta means
      beta <- data.frame()
      tmp<-c()
      for (i in 1:nreg){
        tmp <- rowMeans(out$betadraw[i,,seq(R*burn.in,R)])
        #tmp <- rowMeans(out$betadraw[i,,seq(2000,10000)])
        
        beta <- rbind(beta,tmp)
      }
      colnames(beta) <- c("I",paste(self$x.n,".b",sep=""))
      beta.n<<-colnames(beta)
      eval(parse(text=paste("beta.d<<-cbind(",ind,"=reg,beta)",sep='')))		
    },
    PredictDataSet=function(calc.d){
      print("No 2 set test data!")
      
      pred.blm<-private$PredictCalc(calc.d)			
      pred.d<-cbind(calc.d,predict=pred.blm)				
      if(length(grep("log",y.n))>=1){
        eval(parse(text = paste("pred.d$predict_",self$y.n,"<-pred.d$predict", sep = "")))
        pred.d$predict_l<-exp(pred.d$predict)
      }else{
        eval(parse(text = paste("pred.d$predict_",self$y.n,"<-pred.d$predict", sep = "")))
        pred.d$predict_l<-pred.d$predict
        
      }
      return(pred.d)
    },
    PredictCalc=function(test.d=NA){
      # y_i = X_ibeta_i + e_i. e_i ~ N(0,tau_i). 
      # nvar X vars in each equation.
      #print(beta.n)
      print("No 3 calc predict!")
      
      result<-apply(test.d,1,function(x){ sum(as.numeric(x[beta.n]) * c(1,as.numeric(x[x.n])))})	
      print(head(result))
      return(result)
    },
    CoefDetermCalc=function(pred.d){
      # calclation coefficient of determination
      R2 <- 1 - sum((pred.d[,y.n] - pred.d[,"predict"])^2) / sum((pred.d[,y.n] - mean(pred.d[,y.n]))^2)
      return(R2)
    }
  )
)