context('facet adjust')

test_that('facet adjust gives proper classes', {
  
  require(ggplot2)
  p <- ggplot(diamonds[1:100, ], aes(carat, price, colour = clarity)) +
    geom_point() + facet_wrap( ~ cut)
  
  p_up <- facet.adjust(p, pos = 'up')
  p_down <- facet.adjust(p, pos = 'down')
  
  expect_equivalent(class(p_up), c('facet.adjust','gtable','ggplot','gg'))
  expect_equivalent(class(p_down), c('facet.adjust','gtable','ggplot','gg'))
})