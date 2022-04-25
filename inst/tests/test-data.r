context("Test data")

test_that("midcap.ts is as expected", {
	expect_that(midcap.ts,is_a("xts"))
	expect_that(dim(midcap.ts),is_identical_to(c(60L,22L)))
	expect_that(start(midcap.ts),is_equivalent_to(as.Date("1997-01-31")))
	expect_that(end(midcap.ts),is_equivalent_to(as.Date("2001-12-31")))
	expect_that(sum(midcap.ts),equals(20.53956115))
  expect_that(names(midcap.ts),equals(
		c("MAT","EMN","LEG","AAPL","UTR","HB","BNK","APA","LNCR","BMET","DBD","FAST","AF",
		  "CPWR","EC","SNV","HSY","TXT","APCC","LXK","market","t90")))
})
