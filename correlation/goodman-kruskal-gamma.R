require(vcdExtra)

# See http://www.inside-r.org/packages/cran/vcdExtra/docs/JobSat
# contingency table of job satisfaction (1996 General Social Survey)
data(JobSat)
#          satisfaction
# income   VeryD LittleD ModerateS VeryS
# < 15k      1       3        10     6
# 15-25k     2       3        10     7
# 25-40k     1       6        14    12
# > 40k      0       1         9    11

GKgamma(JobSat, level=0.95)
# gamma        : 0.221
# std. error   : 0.117
# CI           : -0.009 0.451
