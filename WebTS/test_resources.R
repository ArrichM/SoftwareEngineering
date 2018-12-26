
#Test function to retrieve level values from groth series

test_that("Test multiply_recursive() Constant Growth",{
  constant_11_periods <- 111*(1.11)^11 #Level after 11 of constant 11 percent growth
  test_level_ts <- ts(111) #initial level
  test_growth_ts <- ts(rep(0.11,12)) #discrete growth of 11 percent each period for 11 periods ahead - thus we need t+1 growth periods
  expect_equal(as.numeric(tail(multiply_recursive(test_level_ts,test_growth_ts),1)),constant_11_periods)
  
})
