library(preprocomb)

context("Basic")

modifiediris <- droplevels(iris[-c(1:60),])
modifiediris2 <- iris
modifiediris2$Char <- rep("A", 150)

## DataClass
testdataobject <- initializedataclassobject(modifiediris)

test_that("DataClass initialization", {
expect_is(testdataobject, "DataClass")
expect_true(all(testdataobject@variance==TRUE, testdataobject@finite==TRUE, testdataobject@completeobs==TRUE, testdataobject@classbalance==TRUE, testdataobject@ntopratiotwoplus==TRUE, testdataobject@mindimensions==TRUE))
expect_error(initializedataclassobject(modifiediris2))
})

## PhaseClass
testphase_success <- setphase("testphase_success", "naomit", preimpute=TRUE)

test_that("PhaseClass initialization", {
expect_error(testphase_error <- setphase("testphase_error", "NOTVALIDPHASE", preimpute=TRUE))
expect_is(testphase_success, "PhaseClass")
})

## GridClass
testgrid <- setgrid(phases=c("valuerange"), data=modifiediris)

test_that("GridClass initialization", {
expect_is(testgrid, "GridClass")
expect_error(setgrid(phases=c("NOTVALIDPHASE"),data=modifiediris))
})

## PreprocessorClass

testprepro <- prepro(iris, "basicscale", nholdout = 1)

test_that("PreprocessorClass initialization", {
expect_error(prepro(iris, "NOTVALIDPREPROCESSOR", nholdout = 1))
})

## ResultClass

#test_that("ResultClass initialization", {
#expect_is(preprocomb(gridclassobject=testgrid, nholdout=1 ), "ResultClass")
#})



