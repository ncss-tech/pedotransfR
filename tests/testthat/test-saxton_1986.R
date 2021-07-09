# Hydraulic Conductivity after Saxton et al. (1986)
# 
test_that("saxton_1986_K works", {
  # estimate saturated water filled porosity
  theta_s <- saxton_1986_theta_s(80, 10)
  expect_equal(theta_s, 0.401592)
  
  # estimate hydraulic conductivity at 50% porosity filled with water
  expect_equal(saxton_1986_K(80, 10, theta = theta_s / 2), 5.8693943e-08)

  # estimate hydraulic conductivity at saturation
  expect_equal(saxton_1986_Ksat(80, 10), 7.997718e-06)
})

test_that("saxton_1986_psi works", {
  expect_equal(saxton_1986_psi(80, 10, c(0.09,0.25,0.4)), 
               c(1203.78269370527, 8.57617977382728, 2.95395771676562))
})
