if (interactive()) {
  con <- stdin()
} else {
  con <- "stdin"
}

checkA <- FALSE
checkB <- FALSE
rct <- 1
gamematrix <- matrix(nrow=3,ncol=3)
calcmatrix <- matrix(0, nrow=3,ncol=3)
win <- NA

cat("Welcome to Tic-Tac-Toe!\nThe Goal is to get three X's or O's in a row.
    \nPlayer 1 will start with an X.\nPlayer 2 with follow with an O.
    \nRepeat until someone wins or the board is filled\n")
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
      checkB <- TRUE
      if (symbol == "O"){
        calcmatrix[row, column] <- -1
      } else if (symbol == "X") {
        calcmatrix[row, column] <- 1
      }
    } else {
      cat("Value Out of Range. Please indicate a value from 1 to 3\n")
    }
  }
  
  sumlist <- c(rowSums(calcmatrix),colSums(calcmatrix), sum(diag(calcmatrix)), sum(calcmatrix[1,3], calcmatrix[2,2], calcmatrix[3,1]))
  if (3 %in% sumlist){
    win <- "X"
    cat("Congratulations to Player X!\n")
  } else if (-3 %in% sumlist) {
    win <- "O"
    cat("Congratulations to Player O!\n")
  } else if (sum(is.na(gamematrix) == TRUE) == 0){
    cat("Tied!")
  }
  rct <- rct + 1
  checkA <- FALSE
  checkB <- FALSE
}

# 
# cat("Confirm: ", symbol, "at row ", row, "and column ", col, ". [y/n]?")
# confirm <- readLines(con = con, n = 1)

