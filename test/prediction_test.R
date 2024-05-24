Rframe <- Rframe %>%
  subset(refugee_sample == 1)

Lframe <- Lframe %>%
  filter(!is.na(employment_two_year_arrival))

Rframe <- Rframe %>%
  filter(!is.na(employment_two_year_arrival))



outcome <- "employment_two_year_arrival"
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



set.seed(1234)
LRtoOMout <- func_LR_to_OM_boostedtreesCV_binary(outcome = outcome,
                                                 Lframe = Lframe, Rframe = Rframe,
                                                 aid = "aid", rid = "rid",
                                                 cid = "cid", csize = "csize",
                                                 incl.locs = incl.locs, 
                                                 predictors = predictors,
                                                 depth.vec = c(4,5,6),n.trees=1000,
                                                 shrink = 0.01)  

















