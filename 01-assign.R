library(tidyverse)
source("common.R")

## Input filename (ifn)
ifn <- "data/CSBC_PS-ON Image Analysis Hackathon 2022 (Responses) - Form Responses 1.csv"

## Exclude champions and folks who decided not to participate
excl <- scan("data/exclude.txt", what=character())

## Load and clean up all registrant information
## Resolve duplicate email entries by taking the more recent one
X <- read_csv(ifn, col_types=cols()) %>%
    rename(!!!g_sn) %>% filter( !(Email %in% excl) ) %>%
    group_by(Email) %>% slice(n()) %>% ungroup() %>%
    mutate(across(all_of(g_ct), replace_na,  "Not interested"))
stopifnot(anyNA(X)==FALSE)

## The endosomes challenge has been cancelled
X <- X %>% select(-Timestamp, -endosomes)
ct <- setdiff(g_ct, "endosomes")

## Identify and display any duplicate names for manual verification
cat("Entries with identical names:\n")
X %>% group_by(Name) %>% filter(n()>1) %>% select(Name, Email)

## Remove the duplicates (assuming that they pass manual inspection)
X <- X %>% group_by(Name) %>% slice(n()) %>% ungroup()

## Randomly shuffle the registrants
set.seed(42)
X <- slice(X, sample(1:n()))

nc <- length(ct)           # Number of challenges
np <- nrow(X)              # Number of people

## Maximize the interest of participants
w <- X %>% select(all_of(ct)) %>%
    mutate(across(everything(), recode, !!!g_il)) %>%
    as.matrix() %>% c()

## Such that...
MAT <- list()
DIR <- list()
RHS <- list()

## 1. Assignment is binary (yes=1, no=0)
MAT[[1]] <- diag(np*nc)
DIR[[1]] <- rep("<=", np*nc)
RHS[[1]] <- rep(1, np*nc)

## 2. Each person is assigned to at most one challenge
MAT[[2]] <- kronecker( t(rep(1,nc)), diag(np) )
DIR[[2]] <- rep("<=", np)
RHS[[2]] <- rep(1, np)

## 3. The difference in the number of participants between any
##    two challenges is not greater than 1
MAT[[3]] <- map(1:nc, ~`[<-`(-diag(nc)[-.x,],,.x,1)) %>%
    do.call(rbind, .) %>% kronecker( t(rep(1,np)) )
DIR[[3]] <- rep("<=", nc*(nc-1))
RHS[[3]] <- rep(1, nc*(nc-1))

## 4. Each challenge includes enough GPUs
MAT[[4]] <- kronecker( diag(nc), t(as.integer(X$GPU == "Yes")) )
DIR[[4]] <- rep(">=", nc)
RHS[[4]] <- rep(3, nc)

## 5. Each challenge includes adequate image analysis expertise
MAT[[5]] <- kronecker( diag(nc), t(as.integer(X$Experience >= 5)) )
DIR[[5]] <- rep(">=", nc)
RHS[[5]] <- rep(1, nc)

## Solve via integer programming
Z <- lpSolve::lp(direction    = 'max',
                 objective.in = w,
                 const.mat    = do.call(rbind, MAT),
                 const.dir    = do.call(c, DIR),
                 const.rhs    = do.call(c, RHS),
                 int.vec      = 1:(nc*np))
z <- Z$solution

## Quick checks
w[as.logical(z)]
matrix(z, np, nc) %>% rowSums()
matrix(z, np, nc) %>% colSums()

## Wrangle the solution and join against the participant list
Y <- matrix(z, np, nc) %>% apply(1, which.max) %>% ct[.] %>%
    mutate( X, Assignment = . )

## Additional checks
Y %>% group_by(Assignment) %>%
    summarize(nGPU    = sum(GPU == "Yes"),
              nExpert = sum(Experience >= 5))

Y %>% select(Assignment, Name, Email, everything()) %>%
    write_csv("data/assignment.csv")

## Isolate a slice to be shared with registrants
Y %>% select(Assignment, Name, Email, Institution, Experience, Languages, GPU) %>%
    arrange(Assignment, Name) %>% write_csv("teams.csv")

