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
    \nPlayer X will start with an X first.\nPlayer O with follow with an O next.
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
  
  # One Game
  while (!(win %in% c("X", "O")) & rct < 10) {
    # Track whose turn it is
    rtrk <- rct%%2
    if (rtrk == 1){
      cat("\nPlayer X's turn!")
    } else if (rtrk == 0) {
      cat("\nPlayer O's turn!")
    }
    
    # Ask user to input X or O
    while (checkA == FALSE) {
      cat("X or O? ")
      symbol <- toupper(readLines(con = con, n = 1))
      
      # Check if input valid and provide feedback if needed
      if (symbol != "X" & symbol != "O") {
        cat(symbol, "is not a valid input. Please try again: \n")
      } else if (rtrk == 1 & symbol != "X" || rtrk == 0 & symbol != "O") {
        cat("It is not Player", symbol, "'s turn. Please try again: \n")
      } else {
        checkA <- TRUE
      }
    }
    
    # Ask user to input Row and Column
    while (checkB == FALSE) {
      cat("Which Row? ")
      row <- as.integer(readLines(con = con, n = 1))
      cat("Which Column? ")
      column <- as.integer(readLines(con = con, n = 1))
      
      # Check if input valid and provide feedback if needed
      if (row %in% c(1:3) & column %in% c(1:3) && is.na(gamematrix[row, column]) == TRUE) {
        gamematrix[row, column] <- symbol
        # User input valid -- Confirm play
        cat("Confirm:", symbol, "at row", row, "and column", column, "\n[y/n]?")
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
    
    print(gamematrix)
    
    # Calculate rows/cols/diags to deterime game status (3 = win for X and -3 = win for O)
    sumlist <- c(rowSums(calcmatrix),colSums(calcmatrix), sum(diag(calcmatrix)), sum(calcmatrix[1,3], calcmatrix[2,2], calcmatrix[3,1]))
    if (3 %in% sumlist){
      win <- "X"
      playerXpts <- playerXpts + 1
      cat("Congratulations to Player X!
          \nCurrent score: Player O has", playerXpts, "points and Player X has", playerOpts, "points")
    } else if (-3 %in% sumlist) {
      win <- "O"
      playerOpts <- playerOpts + 1
      cat("Congratulations to Player O!
        \nCurrent score: Player O has", playerXpts, "points and Player X has", playerOpts, "points")
    } else if (sum(is.na(gamematrix) == TRUE) == 0){
      cat("Tied!
          \nCurrent score: Player O has", playerXpts, "points and Player X has", playerOpts, "points")
    }
    rct <- rct + 1
    checkA <- FALSE
    checkB <- FALSE
  }
  
  # Confirm if want to play again
  cat("\nDo you want to play [y/n]? ")
  play <- toupper(readLines(con = con, n = 1))
} 

# If don't want to play, send goodbye message
if (play == "N"){
    cat("\nAlright. Come again next time! Bye Bye!\n")
}

