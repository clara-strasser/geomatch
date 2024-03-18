######################### BOOSTED TREES ########################################
# Summary:
# This function takes training and out-of-sample data to model the outcome 
# and provide a matrix of predicted probabilities for each out-of-sample
# individual at each location under consideration, along with other 
# information and outputs.

# Load packages ----------------------------------------------------------------

library(CORElearn)
library(gbm)
library(dplyr)

# Arguments --------------------------------------------------------------------

# outcome: name of (binary, 0 or 1) outcome variable being predicted
# Lframe: data used for model training
# Rframe: data for the units for which we want predicted probability matrix
# aid: name of variable specifying actual location assignment
# cid: name of variable specifying case identifier codes
# rid: name of variable specifying individual identifier codes
# csize: name of variable specifying size of case (e.g. # refugees in family)
# incl.locs: vector of the resettlement location names to be evaluated
# predictors: vector of predictor variable names to be used in modeling
# depth.vec: vector of tree depths to be considered in CV
# n.trees: number of boosting iterations to be considered in CV
# shrink: shrinkage rate


# Output -----------------------------------------------------------------------

#Function output is a list of 4 objects:
# 1. Combined O (individual info) and M (pred prob) matrix for Rframe units
# 2. Vector of tree depth values chosen by CV for each location model
# 3. Vector of boosting iteration values chosen by CV for each location model
# 4. In-sample percent correctly predicted stats (for reference)


# Function ---------------------------------------------------------------------

func_LR_to_OM_boostedtreesCV_binary <- function(outcome,Lframe,Rframe,
           aid,cid,rid,csize,
           incl.locs,predictors,
           depth.vec,n.trees,shrink){
  
  # Create empty objects to return ---------------------------------------------

  Mframe <- matrix(NA, nrow = nrow(Rframe), ncol = length(incl.locs),
                     dimnames = list(row.names(Rframe), incl.locs))

  PCPstats <- data.frame(loc = incl.locs, 
                         model.PCP = rep(NA,length(incl.locs)), 
                         null.PCP = rep(NA,length(incl.locs)))
  
  depth.ntrees.CVs <- list()
  best.depths <- c()
  best.ntrees <- c()
  
  # Populate and format the matrix and other objects ---------------------------
  
  Lframe$outcome <- Lframe[,outcome]
  Lframe <- Lframe[,c("outcome",predictors,aid)]
  Lframe$outcome <- as.numeric(Lframe$outcome==2)
  
  Oframe.later <- Rframe
  
  Rframe$outcome <- Rframe[,outcome]
  Rframe <- Rframe[,c("outcome",predictors,aid)]
  Rframe$outcome <- as.numeric(Rframe$outcome==2)
  
  for (k in 1:ncol(Lframe)){
    if (is.character(Lframe[,k])){
      Lframe[,k] <- as.factor(Lframe[,k])
    }
  }
  
  for (k in 1:ncol(Rframe)){
    if (is.character(Rframe[,k])){
      Rframe[,k] <- as.factor(Rframe[,k])
    }
  }
  
  
  # Cross-validation step ------------------------------------------------------

  for (j in 1:length(incl.locs)){
    
    depth.ntrees.CVs[[j]] <- matrix(NA,nrow = n.trees,
                                    ncol = length(depth.vec))
    
    for (k in 1:length(depth.vec)){
      depth <- depth.vec[k]
      
      rm(boost.ref)
      rm(Lframe.loc)
      
      Lframe.loc <- subset(Lframe, levels(Lframe$aid)[Lframe$aid] == incl.locs[j])[, -ncol(Lframe)] 
      #Lframe.loc <- subset(Lframe,
                            #Lframe[,aid] == incl.locs[j])[,-ncol(Lframe)]
      boost.ref <- gbm(outcome ~ ., data = Lframe.loc, shrinkage = shrink,
                       distribution="bernoulli", n.trees = n.trees, 
                       interaction.depth = depth, cv.folds = 3)
      depth.ntrees.CVs[[j]][,k] <- boost.ref$cv.error
    }
    
    best.depths[j] <- depth.vec[which(depth.ntrees.CVs[[j]] == 
                                        min(depth.ntrees.CVs[[j]]), 
                                      arr.ind = TRUE)[2]]
    best.ntrees[j] <- which(depth.ntrees.CVs[[j]] == 
                              min(depth.ntrees.CVs[[j]]), 
                            arr.ind = TRUE)[1]
    
    print(j)
    
  }
  
  # Modeling step --------------------------------------------------------------
  
  for (j in 1:length(incl.locs)){
    
    rm(boost.ref)
    rm(Lframe.loc)
    
    Lframe.loc <- subset(Lframe, levels(Lframe$aid)[Lframe$aid] == incl.locs[j])[, -ncol(Lframe)] 
    
    #Lframe.loc <- 
      #subset(Lframe,Lframe[,aid] == incl.locs[j])[,-ncol(Lframe)]
    boost.ref <- gbm(outcome ~ ., data = Lframe.loc, shrinkage = shrink,
                     distribution="bernoulli", n.trees = best.ntrees[j], 
                     interaction.depth = best.depths[j])
    y <- Lframe.loc$outcome
    
    oospreds <- predict(boost.ref, newdata = Rframe,n.trees = best.ntrees[j],
                        type = "response")
    Mframe[,j] <- oospreds
    
    trainpreds <- predict(boost.ref,newdata = Lframe.loc,
                          n.trees = best.ntrees[j],type="response")
    preds <- as.numeric(trainpreds > 0.5)
    PCPstats$model.PCP[j] <- mean(preds == Lframe.loc$outcome)
    PCPstats$null.PCP[j] <- mean(Lframe.loc$outcome==
                                   round(mean(Lframe.loc$outcome==1)))
    
    print(j)
    
  }
  
  # Format matrices to be usable by algorithm code and return ------------------
  
  Mframe <- as.data.frame(Mframe)
  
  Rframe$temp.aid <- Rframe[,aid]
  
  freq.out <- (Rframe %>% group_by(temp.aid) %>% mutate(freq = n()))$freq
  aid.out <- Rframe[,aid]
  outcome.out <- Rframe$outcome
  cid.out <- Oframe.later[,cid]
  rid.out <- Oframe.later[,rid]
  csize.out <- Oframe.later[,csize]
  
  OMmatrices <- cbind(rid = rid.out, cid = cid.out, csize = csize.out,
                      freq = freq.out, aid = aid.out, outcome = outcome.out,
                      Mframe)
  
  out <- list(OMmatrices,best.depths,best.ntrees,PCPstats)
  return(out)
  
}

