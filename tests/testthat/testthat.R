#######
# ABE #
#######
test_that("ABE (90% CI, PE) of rds01", {
  x <- ABE(details=TRUE, print=FALSE, data=rds01)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(107.1057, 124.8948, 115.6587))
})
test_that("ABE (90% CI, PE) of rds02", {
  x <- ABE(details=TRUE, print=FALSE, data=rds02)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(97.31555, 107.4649, 102.2644))
})
test_that("ABE (90% CI, PE) of rds03", {
  x <- ABE(details=TRUE, print=FALSE, data=rds03)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(113.0492, 136.4254, 124.1885))
})
test_that("ABE (90% CI, PE) of rds04", {
  x <- ABE(details=TRUE, print=FALSE, data=rds04)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(117.9016, 159.6893, 137.2138))
})
test_that("ABE (90% CI, PE) of rds05", {
  x <- ABE(details=TRUE, print=FALSE, data=rds05)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(103.8242, 112.0357, 107.8518))
})
test_that("ABE (90% CI, PE) of rds06", {
  x <- ABE(details=TRUE, print=FALSE, data=rds06)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(80.06738, 93.36574, 86.46127))
})
test_that("ABE (90% CI, PE) of rds07", {
  skip_on_cran()
  x <- ABE(details=TRUE, print=FALSE, data=rds07)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(86.45598, 92.81029, 89.57681))
})
test_that("ABE (90% CI, PE) of rds08", {
  x <- ABE(details=TRUE, print=FALSE, data=rds08)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(75.69153, 87.59971, 81.42823))
})
test_that("ABE (90% CI, PE) of rds09", {
  x <- ABE(details=TRUE, print=FALSE, data=rds09)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(75.69153, 87.59971, 81.42823))
})
test_that("ABE (90% CI, PE) of rds10", {
  x <- ABE(details=TRUE, print=FALSE, data=rds10)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(96.26997, 107.5861, 101.7709))
})
test_that("ABE (90% CI, PE) of rds11", {
  x <- ABE(details=TRUE, print=FALSE, data=rds11)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(80.63656, 100.3801, 89.96836))
})
test_that("ABE (90% CI, PE) of rds12", {
  x <- ABE(details=TRUE, print=FALSE, data=rds12)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(90.82107, 158.9575, 120.1528))
})
test_that("ABE (90% CI, PE) of rds13", {
  x <- ABE(details=TRUE, print=FALSE, data=rds13)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(72.71128, 85.35728, 78.78094))
})
test_that("ABE (90% CI, PE) of rds14", {
  x <- ABE(details=TRUE, print=FALSE, data=rds14)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(69.98855, 123.1679, 92.84581))
})
test_that("ABE (90% CI, PE) of rds15", {
  x <- ABE(details=TRUE, print=FALSE, data=rds15)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(72.71128, 85.35728, 78.78094))
})
test_that("ABE (90% CI, PE) of rds16", {
  x <- ABE(details=TRUE, print=FALSE, data=rds16)
  expect_equivalent(x[15:17],
                    expected=c(69.53983, 89.36796, 78.83294),
                    tolerance=5e-7)
})
test_that("ABE (90% CI, PE) of rds17", {
  x <- ABE(details=TRUE, print=FALSE, data=rds17)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(116.0171, 155.1944, 134.1835))
})
test_that("ABE (90% CI, PE) of rds18", {
  x <- ABE(details=TRUE, print=FALSE, data=rds18)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(54.15838, 99.45727, 73.3924))
})
test_that("ABE (90% CI, PE) of rds19", {
  x <- ABE(details=TRUE, print=FALSE, data=rds19)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(54.17604, 100.0003, 73.60448))
})
test_that("ABE (90% CI, PE) of rds20", {
  x <- ABE(details=TRUE, print=FALSE, data=rds20)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(51.17198, 96.74928, 70.36229))
})
test_that("ABE (90% CI, PE) of rds21", {
  x <- ABE(details=TRUE, print=FALSE, data=rds21)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(111.7245, 127.7421, 119.4652))
})
test_that("ABE (90% CI, PE) of rds22", {
  x <- ABE(details=TRUE, print=FALSE, data=rds22)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(77.98481, 106.0858, 90.95646))
})
test_that("ABE (90% CI, PE) of rds23", {
  x <- ABE(details=TRUE, print=FALSE, data=rds23)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(97.12989, 128.4137, 111.6817))
})
test_that("ABE (90% CI, PE) of rds24", {
  x <- ABE(details=TRUE, print=FALSE, data=rds24)
  expect_equivalent(x[15:17],tolerance=5e-7,
                    expected=c(87.23787, 109.8533, 97.89466))
})
test_that("ABE (90% CI, PE) of rds25", {
  x <- ABE(details=TRUE, print=FALSE, data=rds25)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(77.92805, 98.10162, 87.43493))
})
test_that("ABE (90% CI, PE) of rds26", {
  x <- ABE(details=TRUE, print=FALSE, data=rds26)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(133.5157, 171.4202, 151.2854))
})
test_that("ABE (90% CI, PE) of rds27", {
  skip_on_cran()
  x <- ABE(details=TRUE, print=FALSE, data=rds27)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(78.64846, 89.05791, 83.69151))
})
test_that("ABE (90% CI, PE) of rds28", {
  x <- ABE(details=TRUE, print=FALSE, data=rds28)
  expect_equivalent(x[15:17], tolerance=5e-7,
                    expected=c(87.86358, 100.0704, 93.76858))
})
###################
# ABEL (Method A) #
###################
test_that("method.A (CVwR, EL, 90% CI, PE) of rds01", {
  x <- method.A(details=TRUE, print=FALSE, data=rds01)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(46.96431, 71.22698, 140.3962,
                               107.1057, 124.8948, 115.6587))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds02", {
  x <- method.A(details=TRUE, print=FALSE, data=rds02)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(11.17076, 80, 125,
                               97.31555, 107.4649, 102.2644))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds03", {
  x <- method.A(details=TRUE, print=FALSE, data=rds03)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(58.34494, 69.83678, 143.191,
                               113.0492, 136.4254, 124.1885))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds04", {
  x <- method.A(details=TRUE, print=FALSE, data=rds04)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(61.21664, 69.83678, 143.191,
                               117.9016, 159.6893, 137.2138))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds05", {
  x <- method.A(details=TRUE, print=FALSE, data=rds05)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(11.92193, 80, 125,
                               103.8242, 112.0357, 107.8518))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds06", {
  x <- method.A(details=TRUE, print=FALSE, data=rds06)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(35.15709, 77.14772, 129.6215,
                               80.06738, 93.36574, 86.46127))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds07", {
  skip_on_cran()
  x <- method.A(details=TRUE, print=FALSE, data=rds07)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(34.18815, 77.67137, 128.7476,
                               86.45598, 92.81029, 89.57681))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds08", {
  x <- method.A(details=TRUE, print=FALSE, data=rds08)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(77.61894, 69.83678, 143.191,
                               75.69153, 87.59971, 81.42823))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds09", {
  x <- method.A(details=TRUE, print=FALSE, data=rds09)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(77.61894, 69.83678, 143.191,
                               75.69153, 87.59971, 81.42823))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds10", {
  x <- method.A(details=TRUE, print=FALSE, data=rds10)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(9.506099, 80, 125,
                               96.26997, 107.5861, 101.7709))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds11", {
  x <- method.A(details=TRUE, print=FALSE, data=rds11)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(36.23019, 76.57463, 130.5916,
                               80.63656, 100.3801, 89.96836))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds12", {
  x <- method.A(details=TRUE, print=FALSE, data=rds12)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(221.5472, 69.83678, 143.191,
                               90.82107, 158.9575, 120.1528))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds13", {
  x <- method.A(details=TRUE, print=FALSE, data=rds13)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(79.58209, 69.83678, 143.191,
                               72.71128, 85.35728, 78.78094))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds14", {
  x <- method.A(details=TRUE, print=FALSE, data=rds14)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(125.9951, 69.83678, 143.191,
                               69.98855, 123.1679, 92.84581))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds15", {
  x <- method.A(details=TRUE, print=FALSE, data=rds15)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(79.58209, 69.83678, 143.191,
                               72.71128, 85.35728, 78.78094))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds16", {
  x <- method.A(details=TRUE, print=FALSE, data=rds16)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(49.71545, 69.96489, 142.9288,
                               69.53983, 89.36796, 78.83294))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds17", {
  x <- method.A(details=TRUE, print=FALSE, data=rds17)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(30.38521, 79.7839, 125.3386,
                               116.0171, 155.1944, 134.1835))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds18", {
  x <- method.A(details=TRUE, print=FALSE, data=rds18)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(125.9951, 69.83678, 143.191,
                               54.15838, 99.45727, 73.3924))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds19", {
  x <- method.A(details=TRUE, print=FALSE, data=rds19)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(115.231, 69.83678, 143.191,
                               54.17604, 100.0003, 73.60448))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds20", {
  x <- method.A(details=TRUE, print=FALSE, data=rds20)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(135.9316, 69.83678, 143.191,
                               51.17198, 96.74928, 70.36229))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds21", {
  x <- method.A(details=TRUE, print=FALSE, data=rds21)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(32.16196, 78.7855, 126.9269,
                               111.7245, 127.7421, 119.4652))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds22", {
  x <- method.A(details=TRUE, print=FALSE, data=rds22)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(45.28325, 72.01939, 138.8515,
                               77.98481, 106.0858, 90.95646))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds23", {
  x <- method.A(details=TRUE, print=FALSE, data=rds23)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(49.60714, 70.01378, 142.829,
                               97.12989, 128.4137, 111.6817))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds24", {
  x <- method.A(details=TRUE, print=FALSE, data=rds24)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(54.24018, 69.83678, 143.191,
                               87.23787, 109.8533, 97.89466))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds25", {
  x <- method.A(details=TRUE, print=FALSE, data=rds25)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(82.80518, 69.83678, 143.191,
                               77.92805, 98.10162, 87.43493))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds26", {
  x <- method.A(details=TRUE, print=FALSE, data=rds26)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(60.25584, 69.83678, 143.191,
                               133.5157, 171.4202, 151.2854))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds27", {
  skip_on_cran()
  x <- method.A(details=TRUE, print=FALSE, data=rds27)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(35.76263, 76.82345, 130.1686,
                               78.64846, 89.05791, 83.69151))
})
test_that("method.A (CVwR, EL, 90% CI, PE) of rds28", {
  x <- method.A(details=TRUE, print=FALSE, data=rds28)
  expect_equivalent(x[, c(12, 15:19)],
                    tolerance=5e-7,
                    expected=c(28.74524, 80, 125, 87.86358,
                               100.0704, 93.76858))
})
###################
# ABEL (Method B) #
###################
test_that("method.B (CVwR, EL, 90% CI, PE) of rds01", {
  x <- method.B(details=TRUE, print=FALSE, data=rds01)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(46.96431, 71.22698, 140.3962,
                               107.1707, 124.9725, 115.7298))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds02", {
  x <- method.B(details=TRUE, print=FALSE, data=rds02)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(11.17076, 80, 125,
                               97.31555, 107.4649, 102.2644))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds03", {
  x <- method.B(details=TRUE, print=FALSE, data=rds03)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(58.34494, 69.83678, 143.191,
                               113.3136, 136.7324, 124.4734))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds04", {
  x <- method.B(details=TRUE, print=FALSE, data=rds04)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(61.21664, 69.83678, 143.191,
                               117.9016, 159.6893, 137.2138))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds05", {
  x <- method.B(details=TRUE, print=FALSE, data=rds05)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(11.92193, 80, 125,
                               103.8242, 112.0357, 107.8518))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds06", {
  x <- method.B(details=TRUE, print=FALSE, data=rds06)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(35.15709, 77.14772, 129.6215,
                               80.01762, 93.30905, 86.40815))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds07", {
  x <- method.B(details=TRUE, print=FALSE, data=rds07)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(34.18815, 77.67137, 128.7476,
                               86.45598, 92.81029, 89.57681))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds08", {
  x <- method.B(details=TRUE, print=FALSE, data=rds08)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(77.61894, 69.83678, 143.191,
                               75.69153, 87.59971, 81.42823))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds09", {
  x <- method.B(details=TRUE, print=FALSE, data=rds09)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(77.61894, 69.83678, 143.191,
                               75.69153, 87.59971, 81.42823))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds10", {
  x <- method.B(details=TRUE, print=FALSE, data=rds10)
  expect_equivalent(x[, c(12, 15:19)],
                    tolerance=5e-7,
                    expected=c(9.506099, 80, 125,
                               96.26997, 107.5861, 101.7709))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds11", {
  x <- method.B(details=TRUE, print=FALSE, data=rds11)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(36.23019, 76.57463, 130.5916,
                               80.63656, 100.3801, 89.96836))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds12", {
  x <- method.B(details=TRUE, print=FALSE, data=rds12)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(221.5472, 69.83678, 143.191,
                               90.34419, 157.8835, 119.4314))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds13", {
  x <- method.B(details=TRUE, print=FALSE, data=rds13)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(79.58209, 69.83678, 143.191,
                               72.86789, 85.51224, 78.9373))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds14", {
  x <- method.B(details=TRUE, print=FALSE, data=rds14)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(125.9951, 69.83678, 143.191,
                               69.21029, 121.2766, 91.61655))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds15", {
  x <- method.B(details=TRUE, print=FALSE, data=rds15)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(79.58209, 69.83678, 143.191,
                               72.86789, 85.51224, 78.9373))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds16", {
  x <- method.B(details=TRUE, print=FALSE, data=rds16)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(49.71545, 69.96489, 142.9288,
                               69.53983, 89.36796, 78.83294))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds17", {
  x <- method.B(details=TRUE, print=FALSE, data=rds17)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(30.38521, 79.7839, 125.3386,
                               115.9678, 155.0942, 134.1116))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds18", {
  x <- method.B(details=TRUE, print=FALSE, data=rds18)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(125.9951, 69.83678, 143.191,
                               59.12418, 107.2187, 79.61922))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds19", {
  x <- method.B(details=TRUE, print=FALSE, data=rds19)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(115.231, 69.83678, 143.191,
                               53.84191, 98.77551, 72.92641))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds20", {
  x <- method.B(details=TRUE, print=FALSE, data=rds20)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(135.9316, 69.83678, 143.191,
                               50.918, 95.62665, 69.77906))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds21", {
  x <- method.B(details=TRUE, print=FALSE, data=rds21)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(32.16196, 78.7855, 126.9269,
                               111.7166, 127.7332, 119.4568))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds22", {
  x <- method.B(details=TRUE, print=FALSE, data=rds22)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(45.28325, 72.01939, 138.8515,
                               77.98481, 106.0858, 90.95646))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds23", {
  x <- method.B(details=TRUE, print=FALSE, data=rds23)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(49.60714, 70.01378, 142.829,
                               97.12989, 128.4137, 111.6817))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds24", {
  x <- method.B(details=TRUE, print=FALSE, data=rds24)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(54.24018, 69.83678, 143.191,
                               87.23787, 109.8533, 97.89466))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds25", {
  x <- method.B(details=TRUE, print=FALSE, data=rds25)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(82.80518, 69.83678, 143.191,
                               77.92805, 98.10162, 87.43493))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds26", {
  x <- method.B(details=TRUE, print=FALSE, data=rds26)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(60.25584, 69.83678, 143.191,
                               133.5121, 171.4248, 151.2854))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds27", {
  x <- method.B(details=TRUE, print=FALSE, data=rds27)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(35.76263, 76.82345, 130.1686,
                               78.85772, 89.3044, 83.91866))
})
test_that("method.B (CVwR, EL, 90% CI, PE) of rds28", {
  x <- method.B(details=TRUE, print=FALSE, data=rds28)
  expect_equivalent(x[, c(12, 15:19)], tolerance=5e-7,
                    expected=c(28.74524, 80, 125, 87.86358,
                               100.0704, 93.76858))
})
#####################################
# ABEL (Method B) demanding dataset #
#####################################
test_that("method.B (Satterthwaite DF, CVwR, EL, 90% CI, PE) of rds18", {
  x <- method.B(details=TRUE, print=FALSE, data=rds18, option=1)
  expect_equivalent(x[, c(10, 12, 15:19)], tolerance=5e-7,
                    expected=c(177.922, 125.9951, 69.83678, 143.19102,
                               59.132003, 107.204568, 79.619224))
})
test_that("method.B (Kenward-Roger DF, CVwR, EL, 90% CI, PE) of rds18", {
  x <- method.B(details=TRUE, print=FALSE, data=rds18, option=3)
  expect_equivalent(x[, c(10, 12, 15:19)], tolerance=5e-7,
                    expected=c(179.69018, 125.99509, 69.83678, 143.19102,
                               59.107151, 107.24964, 79.619224))
})
