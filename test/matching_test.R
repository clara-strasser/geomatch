############################ Matching Test ####################################

# Prepare ----------------------------------------------------------------------

# Create a data set
df <- data.frame(
  rid = c(8, 9, 10, 11, 12),
  cid = c(1, 2, 2, 3, 4),
  aid = c(5, 5, 5, 6, 6),
  outcome = c(1, 0, 0, 0, 0),
  `5` = c(0.8,0.1,0.08,0.8,0.9),
  `6` = c(0.2, 0.9, 0.75, 0.2, 0.3),
  check.names = FALSE
)

# Modify format
df <- df %>%
  mutate(rid = as.integer(rid),
         cid = as.integer(cid),
         aid = as.character(aid))

# Rename 
OM <- df

# Run Algorithm ----------------------------------------------------------------

# 1)

# Remove variables
lastvar <- 4

# O: N x P dataframe with refugee identifier/information variables
O <- OM[,1:lastvar]

# M: N x L matrix of refugee-location predicted probabilities of employment
M <- as.matrix(OM[,-(1:lastvar)])
rownames(M) <- O$rid

# 2)

Mstar <- func_M_to_Mstar(mmat = M, cid = O$cid, tfunc = compute_femp_prob)


++# 3) 

# 3.1) 
source("src/func_Mstar_to_D.R")
source("src/func_optmatch_to_Astar.R")
source("src/func_Astar_to_A.R")

# 3.2) 

# Set cushion for location capacity constraints
# Note: 1 if each location is to take exact number of cases specified
Cushion <- 1

# Specify location capacity slots with named vector
# The following can be used in backtest context
holder <- (O %>% group_by(aid) %>% summarise(out = length(unique(cid))))
slots <- holder$out
names(slots) <- holder$aid

# Create distance matrix for input into optimal matching procedure
D <- func_Mstar_to_D(Mstar = Mstar, slots = slots, cushion = Cushion)

# Implement optimal matching via linear sum optimality criterion
options(optmatch_max_problem_size = Inf)
optout <- pairmatch(x = D, controls = 1)


# Reformat output (see function code for argument and output details)
aids <- unique(colnames(Mstar))
Astar <- func_optmatch_to_Astar(optout = optout, aids = aids, print=0)

# Comparing case assignment numbers (if backtest)
slots
table(Astar$raid)

# Convert case-level assignments to individual-level assignments
A <- func_Astar_to_A(Astar = Astar, O = O, M = M, print=0)

# raid column is optimal assignment
# predprob is predicted probability at optimal assignment
# aid column is original assignment (if backtest)


# 3.3) 

# Mean outcome
mean(A$outcome)

# Mean predicted probability
mean(A$predprob)

# Relative difference between predicted outcome and true outcome
(mean(A$predprob)-mean(A$outcome))/mean(A$outcome)

























