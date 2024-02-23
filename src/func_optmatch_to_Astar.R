########################### CASE ASSIGNMENT ###################################
# Summary:
# This function takes the optmatch() output and re-formats it
# into a usable data frame indicating case assignments.


# Arguments --------------------------------------------------------------------

# optout: output from pairmatch in optmatch
# aids: vectors of all unique affiliate names
# print: indicator for printing progress or not (0 no, > 0 yes)

# Output -----------------------------------------------------------------------

# Function output is a data frame with case ids and location assignments


# Function ---------------------------------------------------------------------

func_optmatch_to_Astar <- function(optout,aids,print=0){
  
  optout <- optout[!is.na(optout)]
  l <- length(optout)
  cases <- optout[1:(l/2)]
  cids <- names(cases)
  allocs <- optout[-(1:(l/2))]
  
  Astar.out <- data.frame(cid = cids)
  Astar.out$raid <- NA
  
  for (i in 1:nrow(Astar.out)){
    
    tcid <- Astar.out$cid[i]
    cval <- cases[which(as.numeric(names(cases)) == tcid)]
    Astar.out$raid[i] <- names(allocs)[which(allocs == cval)]
    Astar.out$raid[i] <- str_extract(Astar.out$raid[i], "\\d+(?=_\\d*$)")
    
    #Progress message
    if(i %% 100==0 & print>0) {
      cat(paste0("iteration: ", i, "\n"))
    }
    
  }
  
  for (i in 1:length(aids)){
    
    taid <- aids[i]
    #Astar.out$raid <- str_extract(Astar.out$raid, "\\d+(?=_)")
    #Astar.out$raid <- gsub(paste0("\\b", taid, "\\b"), taid, Astar.out$raid)
    Astar.out$raid[Astar.out$raid == taid] <- taid
    
  }
  
  return(Astar.out)
  
}
