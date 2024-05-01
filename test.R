outcome = "employment_one_year_arrival"
Lframe = Lframe
Rframe = Rframe
aid = "aid"
rid = "rid"
cid = "cid"
csize = "csize"
incl.locs = incl.locs
predictors = predictors
depth.vec = c(4,5,6)
n.trees=1000
shrink = 0.01


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
  j <- 1
  for (j in 1:length(incl.locs)){
    
    depth.ntrees.CVs[[j]] <- matrix(NA,nrow = n.trees,
                                    ncol = length(depth.vec))
    
    for (k in 1:length(depth.vec)){
      depth <- depth.vec[k]
      
      Lframe.loc <- Lframe %>%
        subset(aid==incl.locs[j]) %>%
        select(-ncol(.))
      
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
    
    
    
  }
  
  # Modeling step --------------------------------------------------------------
  
  for (j in 1:length(incl.locs)){
    
    rm(boost.ref)
    rm(Lframe.loc)
    
    Lframe.loc <- Lframe %>%
      subset(aid==incl.locs[j]) %>%
      select(-ncol(.))
    #Lframe.loc <- subset(Lframe, levels(Lframe$aid)[Lframe$aid] == incl.locs[j])[, -ncol(Lframe)] 
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
    
    Lframe.loc$predictions <- preds
    Lframe.loc_test <- Lframe.loc %>% select(c(outcome,predictions))
    
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
  
  out <- list(OMmatrices,depth.ntrees.CVs,best.depths,best.ntrees,PCPstats)
  return(out)
  
}

