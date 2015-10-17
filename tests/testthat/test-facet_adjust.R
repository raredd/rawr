context('facet adjust')

test_that('facet adjust gives proper classes', {
  
  require(ggplot2)
  p <- ggplot(diamonds[1:100, ], aes(carat, price, colour = clarity)) +
    geom_point() + facet_wrap( ~ cut)
  
  p_up <- facet_adjust(p, pos = 'up')
  p_down <- facet_adjust(p, pos = 'down')
  
  expect_equivalent(class(p_up), c('facet_adjust','gtable','ggplot','gg'))
  expect_equivalent(class(p_down), c('facet_adjust','gtable','ggplot','gg'))
})
