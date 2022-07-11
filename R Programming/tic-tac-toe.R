if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}
# Player total wins after each game
playerOpts <- 0
playerXpts <- 0


# Welcome Message
cat("\nXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOX
    \nWelcome to Tic-Tac-Toe!
    \nThe Goal is to get three X's or O's in a row in a 3x3 board before your opponent.
    \nPlayer 1 will start with an X.\nPlayer 2 with follow with an O.
    \nRepeat until someone wins or until the board is filled!
    \nXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOXOX
    \nDo you want to play [y/n]? ")

# Confirm if want to play
play <- toupper(readLines(con = con, n = 1))

# Allow replay
while (play == "Y") {
  # Count number of rounds, reset after each game
  rct <- 1
  # Record who wins, reset after each game
  win <- NA
  
  # Set up matrix (game = player viewable, calc = behind-door status), reset after each game
  gamematrix <- matrix(nrow=3,ncol=3)
  calcmatrix <- matrix(0, nrow=3,ncol=3)
  
  # Status for if user input is valid
  checkA <- FALSE
  checkB <- FALSE
  
  while (!(win %in% c("X", "O")) & rct < 10) {
    print(gamematrix)
    while (checkA == FALSE) {
      cat("X or O? ")
      symbol <- toupper(readLines(con = con, n = 1))
      if (symbol != "X" & symbol != "O") {
        cat(symbol, "is not a valid input. Please try again: \n")
      } else if (rct%%2 == 1 & symbol != "X" || rct%%2 == 0 & symbol != "O") {
        cat("It is not ", symbol, "'s turn. Please try again: \n")
      } else {
        checkA <- TRUE
      }
    }
    
    while (checkB == FALSE) {
      cat("Which Row? ")
      row <- as.integer(readLines(con = con, n = 1))
      cat("Which Column? ")
      column <- as.integer(readLines(con = con, n = 1))
      if (row %in% c(1:3) & column %in% c(1:3) && is.na(gamematrix[row, column]) == TRUE) {
        gamematrix[row, column] <- symbol
        
        # Confirm play; ERROR
        cat(c("\nConfirm: ", symbol, "at row ", row, "and column ", col, ". [y/n]?"), sep = "")
        confirm <- toupper(readLines(con = con, n = 1))
        if (confirm == "Y"){
          checkB <- TRUE
        }
        if (symbol == "O" & checkB == TRUE){
          calcmatrix[row, column] <- -1
        } else if (symbol == "X" & checkB == TRUE) {
          calcmatrix[row, column] <- 1
        }
      } else {
        cat("Try again. Please indicate a value from 1 to 3\n")
      }
    }
    
    # Calculate rows/cols/diags to deterime game status (3 = win for X and -3 = win for O)
    sumlist <- c(rowSums(calcmatrix),colSums(calcmatrix), sum(diag(calcmatrix)), sum(calcmatrix[1,3], calcmatrix[2,2], calcmatrix[3,1]))
    if (3 %in% sumlist){
      win <- "X"
      playerXpts <- playerXpts + 1
      cat("Congratulations to Player X!
          \nCurrent score: Player 1", playerXpts, "and Player 2", playerOpts)
    } else if (-3 %in% sumlist) {
      win <- "O"
      playerOpts <- playerOpts + 1
      cat("Congratulations to Player O!
        \nCurrent score: Player 1", playerXpts, "and Player 2", playerOpts)
    } else if (sum(is.na(gamematrix) == TRUE) == 0){
      cat("Tied!
          \nCurrent score: Player 1", playerXpts, "and Player 2", playerOpts)
    }
    rct <- rct + 1
    checkA <- FALSE
    checkB <- FALSE
  }
  
  # Confirm if want to play again
  cat("Do you want to play [y/n]? ")
  play <- toupper(readLines(con = con, n = 1))
} 

# If don't want to play, send goodbye message
if (play == "N"){
    cat("\nAlright. Come again next time! Bye Bye!\n")
}

