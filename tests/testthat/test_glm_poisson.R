Renv = new.env(parent = globalenv())
FLenv <- as.FL(Renv)

Renv$dataf<- data.frame(var1 = rnorm(200),
                        var2 = rnorm(200), 
                        var3 = sample( c(0, 1), 200, replace = TRUE),
                        offset=1)
#rownames(var4) <- 1:nrow(var4)
FLenv$dataf <- as.FLTable(Renv$dataf,
                        tableName="ARBaseTestTempTable",
                        drop=TRUE)

test_that("glm: execution for poisson ",{
  result = eval_expect_equal({
    glmobj <- glm(var3 ~ var1 + var2, data=dataf, family = "poisson")
    coeffs <- coef(glmobj)
  },Renv,FLenv,
  expectation = "coeffs",
  noexpectation = "glmobj",
  check.attributes=F,
  tolerance = .000001)
}) 




test_that("glm: equality of coefficients, residuals, fitted.values, df.residual for poisson",{
    result = eval_expect_equal({
        coeffs2 <- glmobj$coefficients
        res <- glmobj$residuals
        fitteds <- glmobj$fitted.values
        dfres <- glmobj$df.residual
    },Renv,FLenv,
    expectation=c("coeffs2","res",
                "fitteds","dfres"),
    noexpectation = "glmobj",
    tolerance = .000001,
    check.attribute = F
  )
})

#summary, plot??