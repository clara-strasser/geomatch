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


library(lmtest)
library(sandwich)
library(foreign)
library(dplyr)


# Historical employment rates ---------------------------------------------

# Load historical working age refugee data
#Using "wdf" as object name
#Using "location" as resettlement location column name
#Using "employed" as employment outcome column name

empout <- as.data.frame((wdf %>% group_by(location) %>% 
             summarise(emprate = mean(employed), n = n())))
empout


# Historical predictor effects --------------------------------------------

# Load historical working age refugee data
#Using "wdf" as object name
#Using "location" as resettlement location column name
#Using "employed" as employment outcome column name

# Choose predictors to include in regression model
#e.g. in U.S. application:
variable.vec <- c("gender","age3039","age4049","age5064",
                  "english_speaking","educ_high_cat",
                  "nat_burma","nat_iraq",
                  "nat_bhutan","nat_somalia",
                  "nat_afghanistan")

# Choose relevant subset of data (pooled or location-specific)
#for pooled model:
mod <- lm(as.formula(paste("employed ~ ",
                           paste(variable.vec,collapse = "+"))),
          data = wdf)
coeftest(mod, vcov = vcovHC(mod, "HC3"))
