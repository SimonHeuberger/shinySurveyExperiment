### So far ###

# The function is designed to first be run with no pre-existing data for the first incoming participant. If the user specifies my.data in the first run, it will result in an error, unless my.data is set up exactly like my function does it for the first run (which is most unlikely). My function is all about sequential blocking from the beginning. If you just want to block already collected data, use Ryan's package.
# For the first run, the user needs to specify n.tr (the number of treatment groups), var.levels (a vector of the levels to be used), and inc.par (the level of the incoming participant)
# That creates this data frame:
#    block.vars        group   id
# 1           1   treatment1    1
# The number in block.vars is inc.par
# The treatment group is randomly assigned (since it's the first guy we have)
# block.vars and group are factor variables, id is numeric
# The resulting data frame can then be input into the function again for the next incoming participant, then for the one after that etc.
# For any runs after the first one, the user needs to specify inc.par and my.data -- var.levels and n.tr are extracted from the loaded my.data that was created in the first run and do not need to be specified again
# Four runs (twice inc.par = 1, then twice inc.par = 3) create this data frame
#    block.vars        group   id
# 1           1   treatment1    1
# 2           1   treatment2    2
# 3           3   treatment1    3
# 4           3   treatment1    4
# The general idea is working. You run once with no data then use the resulting data from the first run for the next incoming participant etc.

### PROBLEM: I did some calculations by hand, and my devised formula does is wrong. According to my formula, the score for treatment1 after 3 runs is lower than the score for treatment2, which means inc.par #4 is assigned to treatment1. Logically, though, it should go to treatment2, to even things out. I need to work on and test my formula ###


rm(list=ls())

# Function
assignt <- function(my.data = NULL, n.tr, inc.par, var.levels){
  if(is.null(my.data)){
    treat <- "treatment"
    groups <- c()
    for(j in 1:n.tr){
      groups[j] <- paste(treat, j, sep="")
    }
    groups <- as.factor(groups)
    my.data <- data.frame(matrix(NA, 1, 3))
    colnames(my.data) <- c("block.vars", "group", "id")
    my.data$block.vars <- as.factor(inc.par)
    levels(my.data$block.vars) <- var.levels
    my.data$group <- sample(groups, 1)
    my.data$id <- 1
  }else{
    groups <- as.factor(levels(my.data$group))
    n.tr <- length(levels(my.data$group))
    levs <- as.factor(levels(my.data[,"block.vars"]))
    I <- length(levs)
    listoflists <- rep(list(list()), n.tr)
    listofscores <- list()
    for(j in 1:n.tr){
      for(i in 1:I){
        listoflists[[j]][[i]] <- ((I-(as.numeric(levs[i])-1)) / (n.tr+1)) * 
          (length(my.data[,"block.vars"][my.data$group == groups[j] & my.data[,"block.vars"] == levs[i]]) / nrow(my.data[my.data$group == groups[j], ]))
        listoflists[[j]][[i]][is.nan(listoflists[[j]][[i]])] <- 0
      }
      listofscores[[j]] <- do.call(sum, listoflists[[j]])
    }
    scores <- unlist(listofscores)
    names(scores) <- groups
    my.data[(nrow(my.data)+1), c("block.vars", "group", "id")] <- c(inc.par, names(scores)[which.min(scores)], (nrow(my.data)+1))
    my.data$id <- as.numeric(my.data$id)
    }
  return(my.data)
}

# First run
inc.par <- 1
n.tr <- 2
var.levels <- c(1:3)
my.data <- assignt(n.tr = n.tr, inc.par = inc.par, var.levels = var.levels)

# All subsequent runs
inc.par <- 3
my.data <- assignt(n.tr = n.tr, inc.par = inc.par, my.data = my.data)

my.data
