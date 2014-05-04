### miscellaneous stuff
# sparkbar, R, witchcraft game
###

#' SparkBar generator
#' 
#' spark(...)
#' 
#' @param ... numeric vector
#' @seealso \url{https://gist.github.com/ramnathv/7793167}
#' 
#' @examples
#' spark(30, 31, 32, 33)
#' spark(runif(20))
#' @export

spark <- function(...) {
  
  numbers <- c(...)
  min_value <- min(numbers)
  max_value <- max(numbers)
  value_scale <- max_value - min_value
  nums <- NULL
  
  for (number in numbers) {
    if ((number - min_value) != 0 && (value_scale != 0)) {
      scaled_value <- (number - min_value) / value_scale
    } else {
      scaled_value <- 0
    }
    
    # hack
    # 9604 and 9608 aren't vertically aligned the same as other block elements
    num <- floor(min(6, scaled_value * 7))
    if (num == 3){
      if ((scaled_value * 7) < 3.5) {
        num <- 2
      } else {
        num <- 4
      }
    } else if (num == 7){
      num <- 6
    }
    nums <- c(nums, num)
  }
  
  noquote(intToUtf8(9601 + nums))
}

#' \code{R}
#' @usage R()
#' @export

R <- function() {
  eval(quote({h=character;r=rep;a=b=h(0);p=options()$width%/%2-5;n="
  ";j=r(toupper(substring(mode(a),4,4)),sum(r(5:9,2)+1)-3)
  k=r(5:9,2);k[4:5]=7;k=cumsum(k+1);j[k]=n;m=paste(h(1),h(1
  ));s=c(0,k[-10])+1;j[c(16:17,24:26,32:33,46:47,53:55,61:64
  ,70:74)]=m;for(i in 1:10)a=c(a,r(m,p),j[s[i]:k[i]])
  cat(c(n,a),sep=b)}))
}

#' Simple memory game
#' 
#' Pick boxes in the correct numerical order to win a prize.
#' 
#' @param level difficulty; choose levels 0 (easy) to 3 (difficult)
#' @param seed seed; see \code{\link{set.seed}}
#' 
#' @return Interactive game
#' @details Set the seed to play the same game.
#' @return A prize.
#' 
#' @examples
#' \dontrun{
#' witchcraft()
#' witchcraft(level = 3)
#' }
#' @export

witchcraft <- function(level = 1, seed = NULL) {
  
  stopifnot(is.numeric(level))
  cat('\014')
  level <- as.numeric(cut(level, c(Inf, 3, 2, 1, -Inf))) - 1
  level <- abs(3 - level)
  
  set.seed(seed)
  x <- runif(9)
  y <- runif(9)
  
  plot.new()
  par(mfrow = c(1,1))
  plot(x, y, pch = as.character(1:9), 
       xlab = '', ylab = '',  
       xaxt = 'none', yaxt = 'none', bty = 'none',
       main = 'Get ready to fight, you pansy!',
       xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
  Sys.sleep(level)
  plot.new()
  
  for(i in 1:9) {
    plot(x[i:9], y[i:9], xlab = 'Pick one', ylab = '', main = 'Go on...',
         xaxt = 'none', yaxt = 'none', bty = 'none',
         pch = 15, cex = sample(1:9) / 2, col = sample(1:9),
         xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
    
    ans <- identify(x, y, n = 1, plot = FALSE)
    
    if(ans == 9 && i == 9) {
      cat('\014')
      cat('WITCHCRAFT... DIE, DEMON, DIE!\n')
      # smiley face, go again
      plot(c(.47,.4,.4,.55), c(.5,.35,.65,.5), xlab = 'go again!', 
           ylab = '', main = 'cool!', xaxt = 'none', yaxt = 'none', 
           bty = 'none', pch = c(19,19,19,41), cex = c(25,2,2,4), 
           col = c('yellow', rep('black',3)),
           xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
    } else if(ans == i) {
      cat('\014')
      cat('Grrr... lucky guess. \n\n\nGo again :)\n')
    } else if(ans != i) {
      cat('\014')
      cat(matrix('LOL you lose!', 100, ncol = 4))
      # X$ face
      plot(c(.4,.4,.55), c(.35,.65,.5), xlab = 'you have died of dysentery', 
           ylab = '', main = '', xaxt = 'none', yaxt = 'none', bty = 'none',
           pch = c('x','x','{'), cex = 4, col = 'black',
           xlim = c(-0.05, 1.05), ylim = c(-0.05, 1.05))
      break
    }
  }
}