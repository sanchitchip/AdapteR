## AdapteR deep table Example.
FLenv <- new.env(parent = globalenv())
FLenv$tbl <- FLTable("tblLSMeansDeep", "ObsIDCol","VarIDCol","ValueCol")
Renv <- new.env(parent = globalenv())
FLenv$fit <- lm(formula= a~., data = FLenv$tbl)
FLenv$mod <- lsmeans(object = FLenv$fit, data = FLenv$tbl, specs = 1)
Renv$tbl <- as.R(FLenv$tbl)
Renv$tbl<-Renv$tbl[,-2]
colnames(Renv$tbl) <- letters[1:4]
Renv$fit <- lm(formula= a~., data = Renv$tbl)
Renv$mod <- lsmeans(Renv$fit,specs = "b")

FLexpect_equal(FLenv$mod$val$lsmean,40.2)
FLexpect_equal(FLenv$mod$val[,1],as.numeric(Renv$mod@levels), tolerance = .01)
FLexpect_equal(FLenv$mod$val$SE,.4118391)




## R lsmeans package example:
Renv <- new.env(parent = globalenv())
Renv$tbl<-fiber
FLenv<-as.FL(Renv)
FLenv$fit <- lm(formula= strength~machine+diameter, data = FLenv$tbl)
Renv$fit <- lm(formula= strength~machine+diameter, data = Renv$tbl)
FLenv$mod <- lsmeans(object = FLenv$fit, data = FLenv$tbl, specs = "machine")
Renv$mod <- lsmeans(object = Renv$fit, data = Renv$tbl, specs = "machine")


FLexpect_equal(FLenv$mod$val$lsmean,c(40.38241,41.41922,38.79836), tolerance = .1)
FLexpect_equal(FLenv$mod$val$SE,c(0.7236252,0.7444169,0.7878785), tolerance = .001)
