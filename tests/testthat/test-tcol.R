context('tcol')

test_that('new tcol is identical to old', {
  
  ## old tcol
  tcol_old <- function(color, trans = 255) {
    if (length(color) != length(trans) & 
          !any(c(length(color), length(trans)) == 1)) 
      stop('Vector lengths not correct')
    if (length(color) == 1 & length(trans) > 1) 
      color <- rep(color, length(trans))
    if (length(trans) == 1 & length(color) > 1) 
      trans <- rep(trans, length(color))
    
    num2hex <- function(x) {
      hex <- unlist(strsplit('0123456789ABCDEF', split = ''))
      return(paste0(hex[(x - x %% 16) / 16 + 1], hex[x %% 16 + 1]))
    }
    
    rgb <- rbind(col2rgb(color), trans)
    res <- paste0('#', apply(apply(rgb, 2, num2hex), 2, paste, collapse = ''))
    
    tolower(res)
  }
  
  ## tests
  cols <- c('red', 'green', 'pink')
  colh <- c('#FF0000', '#00FF00', '#FFC0CB')
  trns <- c(50, 100, 150, 200)
  alps <- trns / 255
  
  invisible(
    lapply(list(cols, colh), function(col) {
      expect_identical(tcol_old(col), tcol(col))
      
      expect_identical(
        tcol_old(col[1], trns),
        tcol(col[1], alps)
      )
      
      expect_identical(
        tcol_old(col[1:2], trns[4:3]),
        tcol(col[1:2], alps[4:3])
      )
      
      expect_identical(
        tcol_old(col, trns[4]),
        tcol(col, alps[4])
      )
    })
  )
  
  ## these lengths are not compatible
  ## e.g., 2 colors and 3 transparencies
  expect_error(tcol(cols[1:2], alps[1:3]))
  expect_error(tcol(cols, alps[1:2]))
  expect_error(tcol(coln[1:2], alps[1:3]))
  expect_error(tcol(colh, alps[1:2]))
})
