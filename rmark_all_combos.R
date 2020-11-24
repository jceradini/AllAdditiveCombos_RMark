

require(gtools) # combinations function
require(plyr) # rbind.fill

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make all possible ADDITIVE combos of predictors and combine into formulas
# - Need to use exact column names of predictors and exact parameter name
# - Prints the formulas in console then need to paste into function to run models

# http://www.phidot.org/forum/viewtopic.php?f=21&t=2478&p=11056&hilit=additive+combinations#p11056
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

all.combos <- function(preds, param, preds.in.all=NULL){

  # preds = c("cover", "ndvi", "slope + slopeSq")
  # param = "p"
  
  vars <- data.frame(preds = preds)
  vars$preds <- as.character(vars$preds)
  pred.num <- nrow(vars)
  combos <- 1:pred.num

  # Make all possible combos of predictors as columns in DF
  pred.combos <- data.frame()
  for(i in 1:pred.num){
    temp <- data.frame(combinations(pred.num, combos[i], vars$preds))
    pred.combos <- rbind.fill(temp, pred.combos)
  }

  
  # Each ROW is a model
  # Rename last column
  last.col <- paste("X", ncol(pred.combos), sep = "") # rename last col
  names(pred.combos)[ncol(pred.combos)] <- last.col # rename last col
  last.var.col <- ncol(pred.combos)
  
  # Change all cols to character
  i <- sapply(pred.combos, is.factor)
  pred.combos[i] <- lapply(pred.combos[i], as.character)

  # Add extra row for constant model (intercept only)
  constant <- data.frame(x = 1)
  names(constant) <- last.col
  pred.combos <- rbind.fill(pred.combos, constant)
  
  # Add column for preds.in.all, which will be in every model
  pred.combos$in.all <- preds.in.all
  # Remove from constant model row
  pred.combos$in.all[pred.combos[last.var.col] == "1"] <- NA
  
  # Add parameter column
  pred.combos$param <- param
  
  
  # Paste all pred cols together to make model names
  # 1st reorder cols so paste is correct 
  pred.combos <- pred.combos[ ,c(ncol(pred.combos), 1:(ncol(pred.combos)-1))] # reorder
  pred.combos2 <- pred.combos
  pred.combos$mod <- apply(pred.combos, 1, paste, collapse = ".") # paste
  pred.combos$mod <- gsub(".NA", "", pred.combos$mod, fixed = TRUE) # remove NA's
  pred.combos$mod <- gsub(" + ", ".", pred.combos$mod, fixed = TRUE)
  pred.combos$mod <- gsub("*", "X", pred.combos$mod, fixed = TRUE)
  pred.combos$mod <- gsub(" * ", "X", pred.combos$mod, fixed = TRUE)
  
  pred.combos2 <- pred.combos2[ ,-1]
  pred.combos2$mod2 <- apply(pred.combos2, 1, paste, collapse = " + ")
  pred.combos2$mod2 <- gsub(" + NA", "", pred.combos2$mod2, fixed = TRUE) # remove NA's
  pred.combos2$mod2 <- gsub("NA + ", "", pred.combos2$mod2, fixed = TRUE) # remove NA's
  
  # Paste all pred cols together to make formulas
  pred.combos$formula <- apply(pred.combos[ ,-c(1, ncol(pred.combos))], 1, paste, collapse = " + ")
  # Strip NA's from formulas
  pred.combos$formula <- gsub(" + NA", "", pred.combos$formula, fixed = TRUE)
  pred.combos$formula <- gsub("NA + ", "", pred.combos$formula, fixed = TRUE)

  
  # Paste list-formula stuff so RMark knows it is a formula
  pred.combos$formula <- paste("<- list(formula = ~", pred.combos$formula, ")", sep = "")
  
  
  # Combine mods and formula
  pred.combos$formula <- paste(pred.combos$mod, pred.combos$formula, sep = " ")
  
  # Add generic formula col
  pred.combos$formula.generic <- pred.combos2$mod2
  
  # Print equations, without rownames, in console so can copy-paste directly into function
  print(pred.combos[ ,c("formula", "formula.generic"), drop=FALSE], row.names = FALSE)
  #return(pred.combos)
} # END FUNCTION
  

#all.combos(preds = c("cover", "ndvi", "slope + slopeSq"), param = "p")

#~~~~~~~~~~~~~~~~~~~~~
# EXAMPLES / OUTPUT
#~~~~~~~~~~~~~~~~~~~~~
# all.combos(preds = c("cover", "ndvi", "year"), param = "Psi")
# all.combos(preds = c("cover", "ndvi"), param = "p")
# 
# all.combos(preds = c("ndvi", "precip"), param = "Psi")
# all.combos(preds = c("ndvi", "precip"), param = "p")
# 
# 
# all.combos(preds = c("ndvi", "precip"), param = "p", preds.in.all = "strata")
# all.combos(preds = c("ndvi", "precip"), param = "p", preds.in.all = "year")
# 
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Example
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# # Detection formulas
# all.combos(preds = c("precip", "temp"), param = "p", preds.in.all = "year")
# 
# # Occupancy formulas
# all.combos(preds = c("cover", "slope", "year"), param = "Psi")
# 
# 
# combos.formulas <- function()
# {
#   # Detection
#   p.precip.temp.year <- list(formula = ~precip + temp + year)
#   p.precip.year <- list(formula = ~precip + year)
#   p.temp.year <- list(formula = ~temp + year)
#   p.1 <- list(formula = ~1)
#   
#   # Occupancy
#   Psi.cover.slope.year <- list(formula = ~cover + slope + year)
#   Psi.cover.slope <- list(formula = ~cover + slope)
#   Psi.cover.year <- list(formula = ~cover + year)
#   Psi.slope.year <- list(formula = ~slope + year)
#   Psi.cover <- list(formula = ~cover)
#   Psi.slope <- list(formula = ~slope)
#   Psi.year <- list(formula = ~year)
#   Psi.1 <- list(formula = ~1)
#   
#   cml <- create.model.list("Occupancy") 
#   
#   return(mark.wrapper(cml, data = data.proc, ddl = data.ddl, adjust = FALSE, output = FALSE))
#   
# }
# 
# # Run models
# mod.set <- combos.formulas()














