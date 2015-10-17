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
    return(res)
  }
  
  ## tests
  cols <- c('red','green','pink')
  colh <- c('#FF0000','#00FF00','#FFC0CB')
  
  invisible(
    lapply(list(cols, colh), function(col) {
      expect_identical(tcol_old(col), toupper(tcol(col)))
      
      expect_identical(tcol_old(col[1], c(50, 100, 150, 200)), 
                       toupper(tcol(col[1], c(50, 100, 150, 200))))
      
      expect_identical(tcol_old(col[1:2], c(200, 100)), 
                       toupper(tcol(col[1:2], c(200, 100))))
      
      expect_identical(tcol_old(col, c(200)),
                       toupper(tcol(col, c(200))))
    }))
  
  ## these lengths are not compatible
  ## e.g., 2 colors and 3 transparencies
  expect_error(tcol(cols[1:2], c(50, 100, 150)))
  expect_error(tcol(cols, c(50, 150)))
  expect_error(tcol(coln[1:2], c(50, 100, 150)))
  expect_error(tcol(colh, c(50, 150)))
})
