
library(nnet)

# Initialize the variables 
transAgent.df <- NULL
chocieCoeff.df <- NULL
tmpDP <- NULL
norDP <- NULL
tmpPP <- NULL
norPP <- NULL
tmpMatrix.df <- NULL
carSet1coeff.mt <- NULL
carSet1.df<-NULL
carSet2coeff.mt <- NULL
carSet2.df<-NULL




############## Functions ####################

# normalization
normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

# Bass model for estimating the diffusion of innovation
bassModel=function(p, q, m, T)
{
  S = double(T)
  Y = double(T+1)
  Y[1] = 0
  for(t in 1:T)
  {
    S[t] = p*m+(q-p)*Y[t] - (q/m)*(Y[t]^2)
    Y[t+1] = Y[t] + S[t]
  }
  
  return(list(nPat = S, cumPat = cumsum(S)))
}

# Calculate the car-sharing infra at t time
s.Infra = function(x)
{
  totalVal = s.baseVal*(1+CAGR)^x
  return(totalVal)
}



# Set several variables for simulation

gTime <- 30   # Time Horizon
tTime <- 0    # 
curTime <- 2020
selfTime <- 2025 # Starting point at emerging Autonomous Vehicles

# parameters for calibrate the model
bAt <- 1
bSt <- 1

# Set for simulation iterations
simCnt <- 0
simTime <- 100 # the number of simulation iterations

# case 1 for slowly diffusion
#a.p <- 0.0001
#a.q <- 0.341865
#a.m <- 26000000

# case 2 for rapldly diffusion
a.p <- 0.017099
a.q <- 0.204529
a.m <- 75000000

# build the parameter for weight of car-sharing 
s.baseVal = 19
CAGR = 0.08
s.maxValue <- s.Infra(500)

# Index of simulation reuslts
owner = c("ownCompact", "ownMidsize", "ownFullsize")
sharing = c("shareCompact", "shareMidsize", "shareFullsize")
base <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)


############# Data Frames #####################

# select over 18 ages
transAgent.df <- HHagent.df[which(HHagent.df$age > 18),]
transAgent.df <- subset(transAgent.df, select = -nCars)

# Data frame for Final output
resDataSum.df <- NULL
resDataSum.df <- data.frame(Var1 = base)
resDataSum.df <- cbind(resDataSum.df, data.frame(matrix(0, nrow = 16, ncol = gTime )))

# load the coefficient table of carSet1 
carSet1coeff.df <- read.csv("data/carSet1.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
row.names(carSet1coeff.df) <- carSet1coeff.df[,1]
carSet1coeff.df[,1] <- NULL

# change the data format for matrix x matrix
carSet1coeff.mt <- as.matrix(carSet1coeff.df)

# load the coefficient table of carSet2
carSet2coeff.df <- read.csv("data/carSet2.csv", header = TRUE, sep=",", stringsAsFactors = FALSE)
row.names(carSet2coeff.df) <- carSet2coeff.df[,1]
carSet2coeff.df[,1] <- NULL

# change the data format
carSet2coeff.mt <- as.matrix(carSet2coeff.df)


# build the parameter for weight of self-driving
Ppred <- bassModel(a.p, a.q, a.m, 100)$cumPat



#simulation iterations
while(simCnt < simTime)
{

  tTime <- 0
  
  # Data frame for storing result that selected
  choiceResult.df <- NULL
  choiceResult.df <- data.frame(matrix(nrow = nrow(choiceCoeff.df) , ncol = 0))


  # Data frame for a result  
  resData.df <- NULL
  resData.df <- data.frame( Var1 = base)  


  # At t time
  while(tTime < gTime)
  {                     ## <============= while {}
  
    tTime <- tTime + 1  
    
    # make the driving pattern and public transportation pattern of each agent
    choiceCoeff.df <- NULL
    tmpDP <- NULL
    norPP <- NULL
    choiceCoeff.df <- transAgent.df[ ,1, drop=F]
    tmpDP <- rlnorm(nrow(choiceCoeff.df), meanlog = -1.642256, sdlog = 0.8313353)
    norDP <- normalize(tmpDP)  
    choiceCoeff.df$pDP <- norDP
    tmpPP <- rlnorm(nrow(choiceCoeff.df), meanlog = 1.095104, sdlog = 0.5426319)
    norPP <- normalize(tmpPP)  
    choiceCoeff.df$pPP <- norPP
    
    
    
    #make the carSet1 table
    rm(tmpMatrix.df)
    tmpMatrix.df <- exp(as.data.frame(t(carSet1coeff.mt %*% t(as.matrix(transAgent.df[,-1])))))
    carSet1.df <- cbind(id = transAgent.df[,1], tmpMatrix.df)
    
    #make the carSet2 table
    rm(tmpMatrix.df)
    tmpMatrix.df <- exp(as.data.frame(t(carSet2coeff.mt %*% t(as.matrix(transAgent.df[,-1])))))
    carSet2.df <- cbind(id = transAgent.df[,1], tmpMatrix.df)
    
    
    
  
    # Calculate the parameter at t time
    St <- ( s.Infra(tTime+10) - s.baseVal) / (s.maxValue - s.baseVal)
  
    # Choice Probability of each agent in CarSet2 
    choiceCoeff.df$curProb <- runif(nrow(choiceCoeff.df))
  
    # Total sum of weight in CarSet2
    sumWeightWoSelf <- 7 + (1-choiceCoeff.df$pDP*exp(-(bSt*St)))  
  
    # make the CarSharing Prob table at t time 
    tmpProbCar.df <- NULL
    tmpProbCar.df <- cbind((1/sumWeightWoSelf)*carSet1.df[,names(carSet1.df) %in% owner],
                         ((1-choiceCoeff.df$pDP*exp(-(bSt*St)))/sumWeightWoSelf)*carSet1.df[,names(carSet1.df) %in% sharing],
                         pTrans = (1/sumWeightWoSelf)*carSet1.df$pTrans, 
                         pTaxi = (1/sumWeightWoSelf)*carSet1.df$pTaxi,
                         walk = (1/sumWeightWoSelf)*carSet1.df$walk)
  
    # cumulative Prob table
    choiceProbCarSharing.df <- t(as.data.frame(apply(tmpProbCar.df/rowSums(tmpProbCar.df), 1, cumsum)))
  
    # Start to emerge using AVs  
    if (tTime + curTime >= selfTime)  
    {
    
      # Calculate the parameter at t time 
      At <- (Ppred[tTime-(selfTime - curTime)+1]) / a.m
      # Total sum of weights in carSet1
      sumWeightSelf <-  (6 * (1-choiceCoeff.df$pDP*exp(-(bAt*At)))) + (1-choiceCoeff.df$pPP*exp(-(bAt*At))) + 1
      sumWeightSharing <- 7 + (1-choiceCoeff.df$pDP*exp(-(bSt*St)))  
      
      # make the AVs Prob table at t time 
      tmpProbCar.df <- NULL
      tmpProbCar.df <- cbind(((1-choiceCoeff.df$pDP*exp(-(bAt*At)))/(sumWeightSelf*sumWeightSharing))*carSet2.df[,names(carSet2.df) %in% owner],
                           (( (1-choiceCoeff.df$pDP*exp(-(bAt*At)))*(1-choiceCoeff.df$pDP*exp(-(bSt*St))))/(sumWeightSelf*sumWeightSharing) )*carSet2.df[,names(carSet2.df) %in% sharing],
                           pTrans = ((1-choiceCoeff.df$pPP*exp(-(bAt*At)))/(sumWeightSelf*sumWeightSharing))*carSet2.df$pTrans, 
                           nothing =(1/(sumWeightSelf*sumWeightSharing))*carSet2.df$nothing)
    
      # row cumulative probability 
      choiceProbSelfDriving.df <- t(as.data.frame(apply(tmpProbCar.df/rowSums(tmpProbCar.df), 1, cumsum)))
    
      # Choice a vehicle in CarSet2
      tmpChoiceRes.df <- as.data.frame(apply(choiceProbSelfDriving.df - choiceCoeff.df$curProb, 
                                           1, function(x) min(which(x>=0))+ncol(choiceProbCarSharing.df))) 
      
      colnames(tmpChoiceRes.df) <- c("tmpResult")
    
      #check the nothing in CarSet2
      tmpChoiceSharing.df <- cbind(choiceProbCarSharing.df, tmpChoiceRes.df[, 1, drop=FALSE])
    
      # make random variable (normal distribution) for CarSet1
      tmpProbChoice <- rnorm(nrow(tmpChoiceSharing.df))
      tmpnorProbChoice <- normalize(tmpProbChoice)
      tmpChoiceSharing.df$pSelfChoice <-tmpnorProbChoice
    
      # check the probability about selecting a self-driving car
      tmpChoiceSelfNothing.df <- tmpChoiceSharing.df[tmpChoiceSharing.df$tmpResult == 17 | tmpChoiceSharing.df$pSelfChoice >= At, , drop = FALSE]
      tmpChoiceSelfDriving.df <- tmpChoiceSharing.df[tmpChoiceSharing.df$tmpResult != 17 & tmpChoiceSharing.df$pSelfChoice < At, , drop = FALSE]
    
      # 
      tmpChoiceRes.df <- subset(tmpChoiceSelfDriving.df, select = tmpResult)
      tmpChoiceWoSelf.df <- subset(tmpChoiceSelfNothing.df, select =-c(tmpResult, pSelfChoice))
      tmpChoiceSelfNothing.df$tmpProb <- runif(nrow(tmpChoiceSelfNothing.df))
    
      # Calculate without Self-Driving
      tmpChoiceResWo.df <- as.data.frame(apply(tmpChoiceWoSelf.df - tmpChoiceSelfNothing.df$tmpProb, 
                                             1, function(x) min(which(x>=0)))) 
    
      colnames(tmpChoiceResWo.df) <- c("tmpResult")
    
      # temporary store
      tmpChoiceRes.df <- rbind(tmpChoiceRes.df, tmpChoiceResWo.df)
      colnames(tmpChoiceRes.df) <- tTime
    
      choiceResult.df <- cbind(choiceResult.df, tmpChoiceRes.df)
    
      tmpChoiceRes.df <- NULL
      tmpChoiceResWo.df <- NULL
      tmpChoiceSharing.df <- NULL
      tmpChoiceWoSelf.df <- NULL
      tmpChoiceSelfNothing.df <- NULL 
      choiceProbSelfDriving.df <- NULL
      choiceProbCarSharing.df <- NULL
    
    }
    else
    {
      # Calculate without Self-Driving
      tmpChoiceRes.df <- as.data.frame(apply(choiceProbCarSharing.df - choiceCoeff.df$curProb, 
                                           1, function(x) min(which(x>=0)))) 
      colnames(tmpChoiceRes.df) <- tTime
    
      # Mission complete
      choiceResult.df <- cbind(choiceResult.df, tmpChoiceRes.df)
    
      choiceProbCarSharing.df <- NULL
      tmpChoiceRes.df <- NULL
    
    } #### else
  
  
  }  ####   <====== while {}


  timData.df <- NULL
  timeData.df <- choiceResult.df
  cntMax <- ncol(timeData.df)

  for(j in 1:cntMax)
  {
    tmp.df <- data.frame(round(prop.table(table(timeData.df[,j]))*100,1))
    resData.df <- merge(resData.df, tmp.df, by="Var1", all = TRUE) 
  }


  resTmpSum.df <- NULL
  resTmpSum.df <- resDataSum.df[,-1, drop = F] + resData.df[,-1, drop = F]
  resDataSum.df <- cbind(resDataSum.df[,1, drop = F], resTmpSum.df)

  simCnt <- simCnt + 1

} ##### while ( number of simulation)


#resTmpSum.df <- NULL
#resDataSum.df <- round(resDataSum.df[,-1, drop = F] / (simTime) , 1)

  
  
  
rm(list=ls(pat="test"))
#test.visual <-t(resData.df)
test.visual <- t(resDataSum.df)
test.visual <- test.visual[-1,]

test.ownerCompact <- ts(test.visual[,1], start = 2020)
test.ownerMidsize <- ts(test.visual[,2], start = 2020)
test.ownerFullsize <- ts(test.visual[,3], start = 2020)
test.shareCompact <- ts(test.visual[,4], start = 2020)
test.shareMidsize <- ts(test.visual[,5], start = 2020)
test.shareFullsize <- ts(test.visual[,6], start = 2020)
test.pTrans <- ts(test.visual[,7], start = 2020)
test.pTaxi <- ts(test.visual[,8], start = 2020)
test.walk <- ts(test.visual[,9], start = 2020)
test.selfownerCompact <- ts(test.visual[,10], start = 2020)
test.selfownerMidsize <- ts(test.visual[,11], start = 2020)
test.selfownerFullsize <- ts(test.visual[,12], start = 2020)
test.selfshareCompact <- ts(test.visual[,13], start = 2020)
test.selfshareMidsize <- ts(test.visual[,14], start = 2020)
test.selfshareFullsize <- ts(test.visual[,15], start = 2020)
test.selfpTrans <- ts(test.visual[,16], start = 2020)

test.ownerNotSelf <- (test.ownerCompact + test.ownerMidsize + test.ownerFullsize)/simTime
test.sharingNotSelf <- (test.shareCompact + test.shareMidsize + test.shareFullsize)/simTime
test.ownerSelf <- (test.selfownerCompact + test.selfownerMidsize + test.selfownerFullsize)/simTime
test.shareSelf <- (test.selfshareCompact + test.selfshareMidsize + test.selfshareFullsize)/simTime


ts.plot(test.ownerNotSelf, test.sharingNotSelf, test.ownerSelf, test.shareSelf, test.pTrans/simTime,
        col=c("blue", "red", "green", "grey", "orange")) 


