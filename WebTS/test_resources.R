
#Test function to retrieve level values from groth series

# Test reproductin of constant growth rate
test_that("Multiply_recursive() Constant Growth",{
  constant_11_periods <- 111*(1.11)^11 #Level after 11 of constant 11 percent growth
  test_level_ts <- ts(111) #initial level
  test_growth_ts <- ts(rep(0.11,12)) #discrete growth of 11 percent each period for 11 periods ahead - thus we need t+1 growth periods
  expect_equal(as.numeric(tail(multiply_recursive(test_level_ts,test_growth_ts),1)),constant_11_periods)
  
})

# Test reproduction of varying growth and correct handling of historic base level values
test_that("Multiply_recursive()  Varying Growth",{
  test_level_ts <- ts(c(10,10*1.08,10*1.08*1.03)) #empiric levels in first 3 periods
  test_growth_ts <- ts(c(NA,0.08,0.03,0.07,0.02,0.04))  #empiric growth in first 3 and then 2 and 4 percent
  expected <- c(10.8,11.124,11.90268,12.1407336,12.62636294) #manually calculated values. We loose first observation due to differentiation
  expect_equal(as.numeric(multiply_recursive(test_level_ts,test_growth_ts)),expected)
  
})