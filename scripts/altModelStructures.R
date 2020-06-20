require(survival)


plot(treeStructure <- ctree(Surv(elapsedDays, Dead) ~ ., data = saplings_surv[-c(1,3,4,6:10,13:27:35,38, 39)]))

summary(survreg(Surv(exp(off),Dead)~Treat, dist="exp", data = st_cent_surv))






summary(
  gam(Dead ~ Context * Treat +
                    elapsedDays * Treat +
                    Lagomrph_site_mean +
                    s(Site, bs = 're') +
                    offset(off),
                  data = st_cent_surv,
                  method = 'REML',
                  family = binomial('cloglog'),
                  control = list(scale.est = 'deviance')
  )
)


summary(
  glm(Dead ~ 1 +
      #Habitat * Treat +
       #         elapsedDays * Treat +
        #        Lagomrph_site_mean +
        #        (1 | Site) +
                offset(off),
              family = binomial(link = 'cloglog'),
              data = st_cent_surv
  )
  )


summary(
  a <- glm(cbind(cumDead, nAlive) ~
        Context * Treat +
        days * Treat +
        lagomrph_site_mean +
         #(1 | Site) +
        offset(off),
      family = binomial(link = 'cloglog'),
      data = site_surv_cnSt
  )
)



summary(
  b <- glm(Dead ~ #1 +
        Context * Treat +
        elapsedDays * Treat +
        Lagomrph_site_mean, #+
        #(1 | Site), 
        offset = off,
      family = binomial(link = 'cloglog'),
      data = st_cent_surv
  )
)


summary(
  c <- glmer(Dead ~ Treat * Context + 
               Lagomrph_site_mean +
               (1 | Site) +
        offset(off),
      family = 'poisson',
      data = st_cent_surv
  )
)

summary(
  glm1 <- glm(Dead ~ #1 +
               Context * Treat +
               #elapsedDays * Treat +
               Lagomrph_site_mean +
               #Roo_site_mean +
               offset(off),
             family = 'poisson',
             data = st_cent_surv
  )
)
