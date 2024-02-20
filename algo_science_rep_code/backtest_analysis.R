##
# This file contains the replication code for the study
# entitled "Improving Refugee Integration Through Data-Driven Algorithmic Assignment" by 
# Kirk Bansak, Jeremy Ferwerda, Jens Hainmueller, Andrea Dillon,
# Dominik Hangartner, Duncan Lawrence, and Jeremy Weinstein. To be published in Science.
# The algorithm and code for the study was developed by 
# Kirk Bansak and Jens Hainmueller (both Stanford University).
# The algorithm and code are distributed under the terms of the 
# GNU Affero General Public License, Version 3, 19 November 2007.
# The terms of the license are in a file called LICENSE.txt
# which is deposited in this dataverse (doi:10.7910/DVN/MS8XES).
# The algorithm and code comes with ABSOLUTELY NO WARRANTY.
# Please cite this dataverse and the accompanying study when using this material. 


library(dplyr)
library(ggplot2)


# Summary gains -----------------------------------------------------------

# load A matrix produced in main script

#observed rate:
mean(A$outcome)
#predicted rate under algorithmic assignment:
mean(A$predprob)
#gains:
(mean(A$predprob)-mean(A$outcome))/mean(A$outcome)


# Gains by location -------------------------------------------------------

# load A matrix produced in main script

holdreal <- (A %>% group_by(aid) %>% summarise(out = mean(outcome),
                                               n = n()))
holdalgo <- (A %>% group_by(raid) %>% summarise(out = mean(predprob),
                                                n = n()))
gains.location <- data.frame(name = holdreal$aid, 
                             algoemp = holdalgo$out, 
                             actualemp = holdreal$out, 
                             nalgo = holdalgo$n,
                             nactual = holdreal$n)
gains.location


# Comparing ECDFs ---------------------------------------------------------

# load A and OM matrices produced in main script

comp <- merge(OM,A[,c("rid","raid")],by = "rid")
comp$predprobactual <- NA
comp$predprobalgo <- NA

for(i in 1:nrow(comp)){
  comp$predprobactual[i] <- comp[i,which(colnames(comp) == comp[i,"aid"])]
  comp$predprobalgo[i] <- comp[i,which(colnames(comp) == comp[i,"raid"])]
}

ggplot(comp) + 
  stat_ecdf(aes(predprobactual), geom = "step", pad = T, color = "red") +
  stat_ecdf(aes(predprobalgo), geom = "step", pad = T, color = "blue") +
  xlab("Probability of Employment") + 
  ylab("Fraction with Equal or Lower Predicted Probability of Employment")


# Model fit ---------------------------------------------------------------

# load OM matrices produced in main script

OMmatrices <- OM
OMmatrices$predprobactual <- NA
for (i in 1:nrow(OMmatrices)){
  OMmatrices$predprobactual[i] <- 
    OMmatrices[i,as.character(OMmatrices$aid[i])]
}
true.employment <- OMmatrices$outcome

#confirm pred probs for actual assignments is close to true emp rate
mean(true.employment)
mean(OMmatrices$predprobactual)

#percent correctly predicted:
null.PCC <- mean(round(mean(true.employment)) == true.employment)
null.PCC
model.PCC <- mean(as.numeric(OMmatrices$predprobactual > 0.5) == 
                    true.employment)
model.PCC

# Percent reduction in error:
null.error <- mean(round(mean(true.employment)) != true.employment)
model.error <- mean(as.numeric(OMmatrices$predprobactual > 0.5) != 
                      true.employment)
pre <- (null.error - model.error)/null.error
pre
