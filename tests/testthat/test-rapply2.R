context('rapply2')

test_that('versions of rapply2 agree', {
  ## original version
  rapply2_v1 <- function(l, FUN, classes = 'any', ...) {
    islist <- function(x) inherits(x, 'list')
    stopifnot(islist(l))
    FUN <- match.fun(FUN)
    for (ii in seq_along(l))
      l[[ii]] <- if (islist(l[[ii]]) & ('list' %ni% classes))
        Recall(l[[ii]], FUN, classes, ...) else
          if (any(classes == 'any') || inherits(l[[ii]], classes))
            FUN(l[[ii]], ...) else l[[ii]]
    l
  }
  
  ## examples from ?rapply2
  ## rapply2(..., check.nested = FALSE) gives the old behavior
  
  ll <- list(list(list(head(cars), list(head(cars)))), letters[1:4],
             factor(1:4), 1:3, head(cars))
  
  expect_identical(
    rapply2(ll, class),
    rapply2_v1(ll, class)
  )
  
  expect_identical(
    rapply2(ll, log, classes = 'data.frame', base = 10),
    rapply2_v1(ll, log, classes = 'data.frame', base = 10)
  )
  
  expect_identical(
    rapply2(ll, unlist, classes = 'list', check.nested = FALSE),
    rapply2_v1(ll, unlist, classes = 'list')
  )
  
  expect_identical(
    rapply2(ll, unlist, classes = c('list', 'data.frame'), check.nested = FALSE),
    rapply2_v1(ll, unlist, classes = c('list', 'data.frame'))
  )
  
  expect_identical(
    rapply2(ll, unlist),
    rapply2_v1(ll, unlist)
  )
  
  ll <- list(NULL, 1, list(2, NULL, list(3, NULL)))
  expect_identical(
    rapply2(ll, function(x) if (is.null(x)) NA else x, skip.null = FALSE),
    rapply2_v1(ll, function(x) if (is.null(x)) NA else x)
  )
  
  
  ## this one fails for the original version (ll$id is not removed)
  f <- function(x) if (!is.null(names(x))) x[names(x) %ni% 'id'] else x
  ll <- list(a = list(id = 1, name = 'a-1'),
             b = list(id = 1, list(id = 2, name = 'b-2'),
                      list(id = 3, name = 'b-3', id = 3)),
             c = list(id = 4),
             id = 'n/a')

  rapply2(ll, f, 'list', check.nested = F)
  rapply2_v1(ll, f, 'list')
})
