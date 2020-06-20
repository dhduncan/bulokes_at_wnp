# This code was to make sure I knew how the fit curve (currently non-sensical) was being generated. The indexing of rows 31:60 is about selecting only the Partial Treatment group. The curve is aberrant for both Total and Partial in that the cumulative hazard function decreases toward the end of the experiment.

test <- with(pred_timeXtreat[31:60,], mod_timeXtreat$coefficients[1] + #intercept
               (mod_timeXtreat$coefficients[2] * elapsedDays) + 
               mod_timeXtreat$coefficients[3] + # Effect Partial treatment
               (elapsedDays * mod_timeXtreat$coefficients[5]) + # Time X treat
                 off) # offset
plot(pred_timeXtreat$elapsedDays[31:60], 1-exp(test), type = "l", lty = 2)
lines(pred_timeXtreat$elapsedDays[31:60], 1-pred_timeXtreat$fit[31:60], type = "l")
legend('bottomright', legend = c("pred object", 'hand calc'), lty = c(1,2), bty = 'n')

library(survival)

fit_treat <- survfit(Surv(elapsedDays, Dead) ~ Treat + Habitat, id = uniqID, conf.type = 'log-log', data = saplings_surv)
plot(cph_treat, conf.int = TRUE, xlim = c(0,404))

coxph(Surv(elapsedDays, Dead) ~ Habitat*Treat + Snd_gr20um, data = saplings_surv)
