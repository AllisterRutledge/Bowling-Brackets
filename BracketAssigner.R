library(dplyr)
recap <- readxl::read_excel("Recap/Example1.xlsx", sheet = "Sheet2")

GetNumberOfBrackets <- function(recap = recap) {
  NumberOfBrackets <- 0
  top8 <- slice_max(recap, Entries, n = 8, with_ties = FALSE)
  
  while (!0 %in% top8$Entries) {
    recap$Entries[recap$Player %in% top8$Player] <-
      recap$Entries[recap$Player %in% top8$Player] - 1
    NumberOfBrackets <- NumberOfBrackets + 1
    top8 <- slice_max(recap, Entries, n = 8, with_ties = FALSE)
  }

  entriesNeeded <- sum(top8$Entries == 0)
  refunds <- recap[recap$Entries > 0, c("Player", "Entries")]
  bracketsMessage <- paste0("Number of Brackets: ", NumberOfBrackets)
  entriesMessage <- paste0("Entries needed for additional bracket: ", entriesNeeded)
  print(bracketsMessage)
  print(entriesMessage)
  print("POTENTIAL REFUNDS")
  print(refunds)
  
  return(NumberOfBrackets)
}

AssignToBrackets <- function(recap = recap, NumberOfBrackets = NumberOfBrackets){
  #list of data frames so we can use lapply
  replicate(NumberOfBrackets, data.frame())
  #assign top entries to brackets first and work down
  }

NumberOfBrackets <- GetNumberOfBrackets(recap)
# clean up bracketsentered so the function can work properly
recap <- recap |> 
  mutate(BracketsEntered = if_else(Entries > NumberOfBrackets,
                                   NumberOfBrackets,
                                   Entries),
         Refunds = Entries - BracketsEntered)
refunds <- sum(recap$BracketsEntered) %% 8
refunds

recap[recap$BracketsEntered %in% max(recap$BracketsEntered),]
# TODO: How to give refunds to sheet 2 example?
