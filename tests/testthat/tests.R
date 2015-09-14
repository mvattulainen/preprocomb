t1 <- transformdata(naomit1, data)

expect_is(transformdata(naomit1, data), "BaseClass")
expect_is(transformdata(knnimpute1, data), "BaseClass")
expect_is(transformdata(meanimpute1, data), "BaseClass")
expect_is(transformdata(rfimpute1, data), "BaseClass")

expect_is(transformdata(basescaling1, t1), "BaseClass")
expect_is(transformdata(centering1, t1), "BaseClass")
expect_is(transformdata(noscaling, t1), "BaseClass")
expect_is(transformdata(minmaxscaling, t1), "BaseClass")
expect_is(transformdata(softmaxscaling, t1), "BaseClass")

expect_is(transformdata(lof, t1), "BaseClass")
expect_is(transformdata(orh, t1), "BaseClass")
expect_is(transformdata(noout, t1), "BaseClass")

expect_is(transformdata(oversample1, t1), "BaseClass")
expect_is(transformdata(nosample1, t1), "BaseClass")
