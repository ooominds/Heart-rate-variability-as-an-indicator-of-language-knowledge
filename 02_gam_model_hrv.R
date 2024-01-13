
require(mgcv)
require(itsadug)
require(ggplot2)

options(show.signif.stars=FALSE)

load('gamdat.rda')

##################
### MAIN MODEL ###
##################

# NOTE: Warning message can be ignored!

summary(gam.model <- gam(HRV.NN50 ~
    SpeakerCountry +
    s(TrialOrder.z) +
    s(ErrorDensity) +
    s(HR.M) +
    s(TrialOrder.z, ID, bs='fs', m=1),
    method='ML',
    data = gamdat))
# Parametric coefficients:
#                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)     -0.02767    0.14973  -0.185   0.8534
# SpeakerCountryP  0.04218    0.01738   2.426   0.0155
# 
# Approximate significance of smooth terms:
#                       edf  Ref.df      F  p-value
# s(TrialOrder.z)     1.000   1.000  0.532    0.466
# s(ErrorDensity)     2.509   3.039 10.650 9.62e-07
# s(HR.M)             2.952   3.752 11.162  < 2e-16
# s(TrialOrder.z,ID) 91.454 368.000 34.121  < 2e-16
# 
# R-sq.(adj) =  0.943   Deviance explained =   95%
# -ML = 141.09  Scale est. = 0.056169  n = 804

################
### TRIMMING ###
################

summary(gam.model.t <- gam(HRV.NN50 ~
    SpeakerCountry +
    s(TrialOrder.z) +
    s(ErrorDensity) +
    s(HR.M) +
    s(TrialOrder.z, ID, bs='fs', m=1),
    method='ML',
    data = gamdat,
    subset=abs(scale(resid(gam.model)))<2.5))
# Parametric coefficients:
#                 Estimate Std. Error t value Pr(>|t|)
# (Intercept)     -0.04679    0.14626  -0.320  0.74911
# SpeakerCountryP  0.04696    0.01443   3.255  0.00119
# 
# Approximate significance of smooth terms:
#                       edf  Ref.df      F p-value
# s(TrialOrder.z)     1.000   1.000  0.926   0.336
# s(ErrorDensity)     2.863   3.465 12.739  <2e-16
# s(HR.M)             4.164   5.202 13.555  <2e-16
# s(TrialOrder.z,ID) 81.711 368.000 45.494  <2e-16
# 
# R-sq.(adj) =  0.958   Deviance explained = 96.3%
# -ML = -7.5007  Scale est. = 0.038389  n = 789

#############
### PLOTS ###
#############

### Speaker ###
speakerdat = get_predictions(gam.model.t,
    cond=list(SpeakerCountry=c('B', 'P')))
speakerdat$lo95 = speakerdat$fit - speakerdat$CI
speakerdat$hi95 = speakerdat$fit + speakerdat$CI

p1 <- ggplot(speakerdat, aes(x=SpeakerCountry, y=fit, ymin=lo95, ymax=hi95)) +
    geom_point(size=3, position=position_dodge(0.5)) +
    geom_errorbar(width=0.12, linewidth=0.5, position=position_dodge(0.5)) +
    scale_x_discrete(name='Speaker Accent',
        labels=c('British', 'Polish')) +
    scale_y_continuous('Heart Rate Variability NN50\n(Partial Effect)', limits=c(-0.5,0.5)) +
    theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16)) +
    theme_classic()
plot(p1)

### Error Denisty ###
par(mar=c(4,5,1,1))
plot.gam(gam.model.t, scheme=1, select=2, ylim=c(-0.15,0.15), rug=FALSE,
    xlab='Error Density', ylab='Heart Rate Variability NN50\n(Partial Effect)')
    abline(h=0)

### Mean Heart Rate (HR.M) ###
par(mar=c(4,5,1,1))
plot.gam(gam.model.t, scheme=1, select=3, ylim=c(-0.62,0.62), rug=FALSE,
    xlab='Mean Heart Rate', ylab='Heart Rate Variability NN50 (Partial Effect)')
    abline(h=0)

