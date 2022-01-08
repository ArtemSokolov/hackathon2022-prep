library(tidyverse)

## Interest level (il)
il <- c("Very interested"     = 3,
        "Somewhat interested" = 2,
        "Not interested"      = 1)

## Load all data and apply minor modifications
X <- read_csv("data/reg-clean.csv", col_types=cols()) %>%
    select(-Timestamp, -Languages) %>%
    mutate(across(`01-artifacts`:`12-cosmetic`, recode, !!!il))

nc <- 12       # Number of challenges
np <- nrow(X)  # Number of people

## Maximize the interest of participants
w <- X %>% select(`01-artifacts`:`12-cosmetic`) %>% as.matrix() %>% c()

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

## 3. Each challenge has at least the average number of participants
MAT[[3]] <- kronecker( diag(nc), t(rep(1,np)) )
DIR[[3]] <- rep(">=", nc)
RHS[[3]] <- rep(floor(np/nc), nc)

## 4. Each challenge includes at least one person with a GPU
MAT[[4]] <- kronecker( diag(nc), t(as.integer(X$GPU == "Yes")) )
DIR[[4]] <- rep(">=", nc)
RHS[[4]] <- rep(1, nc)

## 5. Each challenge includes adequate image analysis expertise
MAT[[5]] <- kronecker( diag(nc), t(as.integer(X$Experience >= 4)) )
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
w[z]
matrix(z, np, nc) %>% rowSums()
matrix(z, np, nc) %>% colSums()

## Wrangle the solution and join against the participant list
ct <- X %>% select(`01-artifacts`:`12-cosmetic`) %>% colnames
Y <- matrix(z, np, nc) %>% apply(1, which.max) %>%
    ct[.] %>% mutate( X, Assignment = . )

## Additional checks
Y %>% group_by(Assignment) %>%
    summarize(nGPU    = sum(GPU == "Yes"),
              nExpert = sum(Experience >= 4))

write_csv(Y, "data/assignment.csv")
