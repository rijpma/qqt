rm(list=ls())
setwd('~/dropbox/viennapaper')

# todo: check if larger samples change results

library(data.table)
library(zoo)
library(bit64)
library(countrycode)
library(stringdist)
library(AER)
library(texreg)

source('scripts/qqtheader.r')

mar <- c(4, 3.5, 2.5, 0.5)

clio <- read.csv('~/dropbox/cliodata/allcliodata_raw.csv', stringsAsFactors=F)
# interpolate clio data
clio$lif_ipl <- panel_approx(clio$lif, clio$year, clio$ccode)
clio$gdp_ipl <- exp(panel_approx(log(clio$gdp), clio$year, clio$ccode))
clio$lab_ipl <- exp(panel_approx(log(clio$lab), clio$year, clio$ccode))
clio$edu_ipl <- exp(panel_approx(log(clio$edu), clio$year, clio$ccode))

ss <- read.csv('tabfigs/sumstats_all_harm.csv', stringsAsFactors=F)

# calc patriarchy index
ss$maledom <- rowSums(cbind(patr_transform(ss$femhhh, positive=FALSE), 
                           patr_transform(ss$yngbrd),
                           patr_transform(ss$olderwives, positive=FALSE),
                           patr_transform(ss$femnonkin, positive=FALSE)))
ss$gendom <- rowSums(cbind(patr_transform(ss$oldmen),
                           patr_transform(ss$neolocals, positive=FALSE),
                           patr_transform(ss$oldjoints),
                           patr_transform(ss$oldlats)))
ss$patriloc <- patr_transform(ss$mardghtr, positive=FALSE)
ss$sonpref <- rowSums(cbind(patr_transform(ss$lastboy, absolute_min=0.51), 
                           patr_transform(ss$sexratio04, absolute_min=1.05)))
ss$maledom[grepl('usa-18[5-7]', ss$sample)] <- NA
ss$patriloc[grepl('usa-18[5-7]', ss$sample)] <- NA
ss$patri <- ss$maledom / 4 + ss$gendom / 3 + # why not 4, 4 components?
            ss$patriloc + ss$sonpref / 2
ss$patri <- round(ss$patri)
write.csv(ss, 'tabfigs/sumstatus_withpatrind.csv', row.names=F)

datapath <- '~/downloads/data/qqt/cleandata/'
files <- list.files(datapath)
files <- grepr('-', files)

dats <- list()
for (file in files){
  datname <- regexprr('-\\w\\w', file)
  year <- regexprr('\\d{4}', file)
  datname <- paste0(gsub('-', '', datname), year)
  x <- fread(paste0(datapath, file), sep=',', header=TRUE) # , data.table=F)
  print(file)
  print(unique(x$iso3))
  print(datname)
  assign(datname, x)
  rm(x)
}

au1910 <- hhaggregate(au1910, extravrbs='literate')
ca1891 <- hhaggregate(ca1891, extravrbs=c('literate', 'occhisco'))
ca1901 <- hhaggregate(ca1901, extravrbs=c('literate', 'income', 'property', 'ethnic', 'occode', 'occscale'))
hu1869 <- hhaggregate(hu1869, extravrbs='literate')
ie1901 <- hhaggregate(ie1901, extravrbs='literate')
ie1911 <- hhaggregate(ie1911, extravrbs='literate')
ja1884 <- hhaggregate(ja1884, extravrbs=c('literate', 'occhisco'))
kr1918 <- hhaggregate(kr1918, extravrbs=c('literate', 'ethnic'))
us1850 <- hhaggregate(us1850, extravrbs=c('literate', 'occhisco', 'ocscorus', 'race', 'servants'))
us1860 <- hhaggregate(us1860, extravrbs=c('literate', 'occhisco', 'ocscorus', 'race', 'servants'))
us1870 <- hhaggregate(us1870, extravrbs=c('literate', 'occhisco', 'ocscorus', 'race', 'servants'))
us1880 <- hhaggregate(us1880, extravrbs=c('literate', 'occhisco', 'ocscorus', 'race', 'servants'))
us1900 <- hhaggregate(us1900, extravrbs=c('literate', 'occhisco', 'ocscorus', 'race', 'servants'))
us1910 <- hhaggregate(us1910, extravrbs=c('literate', 'occhisco', 'ocscorus', 'race', 'servants'))

ro1867 <- hhaggregate(ro1867)
au1632 <- hhaggregate(au1632)
en1881 <- hhaggregate(en1881, extravrbs='ordergb')
gr1851 <- hhaggregate(gr1851, extravrbs='ordergb')
ro1900 <- hhaggregate(ro1900, extravrbs='occhisco')
sa1827 <- hhaggregate(sa1827, extravrbs='berstel')
sc1881 <- hhaggregate(sc1881, extravrbs='ordergb')
zu1870 <- hhaggregate(zu1870, extravrbs='ethnic')

au10 <- au1910[age > 5 & age < 16 & !schoolage & !is.na(age), ]
au32 <- au1632[age > 5 & age < 16 & !schoolage & !is.na(age), ]
ca91 <- ca1891[age > 13 & age < 16 & !is.na(age), ] # Lotte's over Annelies' coding
ca01 <- ca1901[age > 13 & age < 16 & !is.na(age), ] # Lotte's over Annelies' coding
hu69 <- hu1869[age > 5 & age < 16 & !schoolage & !is.na(age), ]
ie01 <- ie1901[age > 13 & age < 16 & !schoolage & !is.na(age), ] # L/A found no mandatory sch.
ie11 <- ie1911[age > 13 & age < 16 & !schoolage & !is.na(age), ] # L/A found no mandatory sch.
ja84 <- ja1884[age > 5 & age < 16 & !schoolage & !is.na(age), ]
kr18 <- kr1918[age > 5 & age < 16 & !schoolage & !is.na(age), ]
us50 <- us1850[age > 5 & age < 16 & !schoolage & !is.na(age), ]
us60 <- us1860[age > 5 & age < 16 & !schoolage & !is.na(age), ]
us70 <- us1870[age > 5 & age < 16 & !schoolage & !is.na(age), ]
us80 <- us1880[age > 5 & age < 16 & !schoolage & !is.na(age), ]
us00 <- us1900[age > 5 & age < 16 & !schoolage & !is.na(age), ]
us10 <- us1910[age > 5 & age < 16 & !schoolage & !is.na(age), ]
ro67 <- ro1867[age > 5 & age < 16 & !schoolage & !is.na(age), ]
en81 <- en1881[age > 5 & age < 16 & !schoolage & !is.na(age), ]
gr51 <- gr1851[age > 5 & age < 16 & !schoolage & !is.na(age), ]
ro00 <- ro1900[age > 5 & age < 16 & !schoolage & !is.na(age), ]
sa27 <- sa1827[age > 5 & age < 16 & !schoolage & !is.na(age), ]
sc81 <- sc1881[age > 5 & age < 16 & !schoolage & !is.na(age), ]
zu70 <- zu1870[age > 5 & age < 16 & !schoolage & !is.na(age), ]

us60$nserv <- us60$fatherservants
us60$servant <- us60$fatherservants > 0
us70$nserv <- us70$fatherservants
us70$servant <- us70$fatherservants > 0

enrldats <- list(zu70, sa27, ro67, ro00, gr51, en81, sc81, ie01, ie11, us50,
                 # us60, # leave out to fit on 3x7 panel
  us70, us80, us00, us10, ca91, ca01, ja84, kr18, hu69, au32, au10)

schoolages <- as.data.frame(matrix(NA, ncol=4, nrow=length(enrldats)))
names(schoolages) <- c('iso3', 'year', 'sample', 'age')
for (i in 1:length(enrldats)){
  schoolages$sample[i] <- unique(enrldats[[i]]$smpl)
  schoolages$iso3[i] <- unique(enrldats[[i]]$iso3)
  schoolages$year[i] <- unique(enrldats[[i]]$year)
  schoolages$age[i] <- min(enrldats[[i]]$age) - 1
}
# write.csv(schoolages, 'tabfigs/schoolages.csv', row.names=F)

au10lit <- au1910[age > 6 & age < 16 & !is.na(age), ]
ca91lit <- ca1891[age > 6 & age < 16 & !is.na(age), ]
ca01lit <- ca1901[age > 6 & age < 16 & !is.na(age), ]
hu69lit <- hu1869[age > 6 & age < 16 & !is.na(age), ]
ie01lit <- ie1901[age > 6 & age < 16 & !is.na(age), ]
ie11lit <- ie1911[age > 6 & age < 16 & !is.na(age), ]
ja84lit <- ja1884[age > 6 & age < 16 & !is.na(age), ]
kr18lit <- kr1918[age > 6 & age < 16 & !is.na(age), ]
# us50lit <- us1850[age > 6], na.rm=T)), by=nsib][nsib < 10][order(nsib)]
# us60lit <- us1860[age > 6], na.rm=T)), by=nsib][nsib < 10][order(nsib)]
us70lit <- us1870[age > 9 & age < 16 & !is.na(age), ]
us80lit <- us1880[age > 9 & age < 16 & !is.na(age), ]
us00lit <- us1900[age > 9 & age < 16 & !is.na(age), ]
us10lit <- us1910[age > 9 & age < 16 & !is.na(age), ]

litdats <- list(au10lit, ca91lit, ca01lit, hu69lit, ie01lit, ie11lit, 
  ja84lit, kr18lit, us70lit, us80lit, us00lit, us10lit)

pdf('tabfigs/qqt_enrol_allsmpls.pdf', paper='a4r', width=0, height=0)
par(mfrow=c(3, 7), mar=c(1, 2, 3, 0.5), font.main=1)
for (dat in enrldats){
  ttl <- paste(unique(dat$iso3), paste(unique(range(dat$year)), collapse='-'))
  dat <- dat[, list(iso3=unique(iso3), 
                    year=paste(unique(range(year)), collapse='-'),
                    mean_enrol=mean(enrol, na.rm=T), 
                    ci_enrol=sd(enrol, na.rm=T)/sqrt(sum(!is.na(enrol)))), 
    by=nsib][order(nsib)][nsib < 10]
  ciplot(dat$nsib, dat$mean_enrol, dat$ci_enrol, type='l', col=2, bty='l', main='')
  title(main=ttl, line=-0.7)
}
dev.off()

pdf('tabfigs/qqt_lit_allsmpls.pdf', width=8, height=7)
par(mfrow=c(3, 4), mar=c(1, 2, 3, 0.5), font.main=1)
for (dat in litdats){
  ttl <- paste(unique(dat$iso3), paste(unique(range(dat$year)), collapse='-'))
  dat <- dat[, list(iso3=unique(iso3), 
                    year=paste(unique(range(year)), collapse='-'),
                    mean_literate=mean(literate, na.rm=T), 
                    ci_literate=sd(literate, na.rm=T)/sqrt(sum(!is.na(literate)))), 
    by=nsib][order(nsib)][nsib < 10]
  ciplot(dat$nsib, dat$mean_literate, dat$ci_literate, type='l', col=2, bty='l', main='')
  title(main=ttl, line=-0.7)
}
dev.off()

enrl <- NULL
for (dat in enrldats){  
  m <- glm(enrol ~ nsib - hhid + age + male + nserv + urban
    + factor(birthorder)
    + motheragefirstchild + spousagegap - hhid, 
    data=dat, family=binomial(link='logit'))
  mbas <- logitmfxest2(m, clustervar1='hhid')
  male <- mbas$mfx['male', 1:2]
  nsib <- mbas$mfx['nsib', 1:2]
  nsrv <- mbas$mfx['nserv', 1:2]
  sags <- mbas$mfx['spousagegap', 1:2]

  m <- glm(enrol ~ nbro + nsis - hhid + age + male + nserv + urban
    + factor(birthorder)
    + motheragefirstchild + spousagegap - hhid, 
    data=dat, family=binomial(link='logit'))

  ngnd <- logitmfxest2(m, clustervar1='hhid')
  nbro <- ngnd$mfx['nbro', 1:2]
  nsis <- ngnd$mfx['nsis', 1:2]

  ests <- unlist(c(nsib, nbro, nsis, male, nsrv, sags))
  enrl <- rbind(enrl, ests)
}

rownames(enrl) <- sapply(enrldats, function(x) unique(x$smpl))
enrl <- as.data.frame(enrl)
names(enrl) <- c('nsib_est', 'nsib_stder', 'nbro_est', 'nbro_stder', 
                 'nsis_est', 'nsis_stder', 'male_est', 'male_stder',
                 'nserv_est', 'nserv_stder', 'sag_est', 'sag_stder')
enrl$iso3 <- sapply(enrldats, function(x) unique(x$iso3))
enrl$year <- sapply(enrldats, function(x) max(x$year))
enrl$ccode <- countrycode(enrl$iso3, 'iso3c', 'iso3n')
enrl <- cbind(enrl, clio[match(paste0(enrl$ccode, enrl$year), paste0(clio$ccode, clio$year)), ])
enrl <- cbind(enrl, ss[match(rownames(enrl), ss$sample), ])

enrl <- enrl[rev(rownames(enrl)), ]

pdf('tabfigs/nsib_v_gdp.pdf')
plot(nsib_est ~ gdp_ipl, data=enrl, 
  bty='l', xlab='GDPpc', ylab='nsib coef.', col=2)
abline(lm(nsib_est ~ gdp_ipl, data=enrl), untf=T)
dev.off()

pdf('tabfigs/nsib_v_edu.pdf')
plot(nsib_est ~ edu_ipl, data=enrl, 
  bty='l', xlab='Av. years of education', ylab='nsib coef.', col=2)
abline(lm(nsib_est ~ edu_ipl, data=enrl), untf=T)
dev.off()

pdf('tabfigs/nsib_v_lif.pdf')
plot(nsib_est ~ lif_ipl, data=enrl, 
  bty='l', xlab='lif', ylab='nsib coef.', col=2)
abline(lm(nsib_est ~ lif_ipl, data=enrl), untf=T)
dev.off()

pdf('tabfigs/nsib_v_patr.pdf')
plot(nsib_est ~ patri, data=enrl,
  bty='l', xlab='Patr. Index', ylab='nsib coef.', col=2)
# pigs if factor
m <- lm(nsib_est ~ patri, data=enrl)
summary(m)
abline(lm(m), untf=T)
dev.off()

pdf('tabfigs/nsib_v_maledom.pdf')
plot(nsib_est ~ maledom, data=enrl,
  bty='l', xlab='Male dom. index', ylab='nsib coef.', col=2)
# pigs if factor
m <- lm(nsib_est ~ maledom, data=enrl)
summary(m)
abline(lm(m), untf=T)
dev.off()

pdf('tabfigs/male_v_patr.pdf')
plot(male_est ~ patri, data=enrl,
  bty='l', xlab='Patr. Index', ylab='Male coef.', col=2)
abline(lm(male_est ~ patri, data=enrl), untf=T)
dev.off()

pdf('tabfigs/male_v_maledom.pdf')
plot(male_est ~ maledom, data=enrl,
  bty='l', xlab='Male dom. index', ylab='Male coef.', col=2)
abline(lm(male_est ~ maledom, data=enrl), untf=T)
dev.off()

pdf('tabfigs/nsibcoefs.pdf')
par(mfrow=c(1,1))
dotchart(enrl$nsib_est, labels=rownames(enrl))
segments(enrl$nsib_est + 2*enrl$nsib_stder, 1:nrow(enrl), enrl$nsib_est - 2*enrl$nsib_stder, 1:nrow(enrl))
# dotplot(x=enrl$nsib_est, ses=enrl$nsib_stder*2, labels=rownames(enrl),
#   mar=c(4, 9, 3.5, 0.5))
abline(v=0)
dev.off()

pdf('tabfigs/malecoefs.pdf')
par(mfrow=c(1,1))
dotchart(enrl$male_est, labels=rownames(enrl))
segments(enrl$male_est + 2*enrl$male_stder, 1:nrow(enrl), enrl$male_est - 2*enrl$male_stder, 1:nrow(enrl))
abline(v=0)
dev.off()

enrlsv <- enrl[!is.na(enrl$nserv_est), ]
pdf('tabfigs/nservcoefs.pdf')
par(mfrow=c(1,1))
dotchart(enrlsv$nserv_est, labels=rownames(enrlsv))
segments(enrlsv$nserv_est + 2*enrlsv$nserv_stder, 1:nrow(enrlsv), enrlsv$nserv_est - 2*enrlsv$nserv_stder, 1:nrow(enrlsv))
abline(v=0)
dev.off()

pdf('tabfigs/sagcoefs.pdf')
par(mfrow=c(1,1))
dotchart(enrl$sag_est, labels=rownames(enrl))
segments(enrl$sag_est + 2*enrl$sag_stder, 1:nrow(enrl), enrl$sag_est - 2*enrl$sag_stder, 1:nrow(enrl))
abline(v=0)
dev.off()


pdf('tabfigs/brosiscoefs.pdf', width=10)
par(mfrow=c(1, 2))
dotchart(enrl$nbro_est, labels=rownames(enrl), col=1, xlim=c(-0.07, 0.07)) #, pch='B')
segments(enrl$nbro_est + 2*enrl$nbro_stder, 1:nrow(enrl), enrl$nbro_est - 2*enrl$nbro_stder, 1:nrow(enrl))
title(main='Brothers')
abline(v=0)
# points(enrl$nsis_est, 1:nrow(enrl) - 0.2, col='red', pch='S')
# segments(enrl$nsis_est + 2*enrl$nsis_stder, 1:nrow(enrl)-0.2, enrl$nsis_est - 2*enrl$nsis_stder, 1:nrow(enrl)-0.2, col=2)
dotchart(enrl$nsis_est, labels=rownames(enrl), col=1, xlim=c(-0.07, 0.07)) #, pch='B')
segments(enrl$nsis_est + 2*enrl$nsis_stder, 1:nrow(enrl), enrl$nsis_est - 2*enrl$nsis_stder, 1:nrow(enrl))
title(main='Sisters')
abline(v=0)
dev.off()

pdf('tabfigs/brosiscoefs_alt.pdf')
par(mfrow=c(1, 1))
dotchart(enrl$nbro_est, labels=rownames(enrl), col=1, xlim=c(-0.07, 0.07), pch='')
points(enrl$nbro_est, 1:nrow(enrl) + 0.25, pch='B')
segments(enrl$nbro_est + 2*enrl$nbro_stder, 1:nrow(enrl)+0.25, enrl$nbro_est - 2*enrl$nbro_stder, 1:nrow(enrl)+0.25, col=1)
points(enrl$nsis_est, 1:nrow(enrl) - 0.25, col='red', pch='S')
segments(enrl$nsis_est + 2*enrl$nsis_stder, 1:nrow(enrl)-0.25, enrl$nsis_est - 2*enrl$nsis_stder, 1:nrow(enrl)-0.25, col=2)
abline(v=0)
dev.off()

parlits <- NULL
names <- NULL
for (dat in enrldats){
  if (!'fatherliterate' %in% names(dat)){
    next
  }
  m <- glm(enrol ~ nsib - hhid + age + male + nserv + urban
  + factor(birthorder) 
  + fatherliterate + motherliterate
  + motheragefirstchild + spousagegap - hhid, data=dat, family=binomial(link='logit'))
  cfs <- logitmfx2(m, clustervar1='hhid')
  dad <- cfs$mfx['fatherliterate', 1:2]
  mom <- cfs$mfx['motherliterate', 1:2]
  ests <- unlist(c(dad, mom))
  parlits <- rbind(parlits, ests)
  names <- c(names, unique(dat$smpl))
}
rownames(parlits) <- names
parlits <- as.data.frame(parlits)
names(parlits) <- c('dadlit_est', 'dadlit_stder', 'momlit_est', 'momlit_stder')
parlits <- parlits[rev(rownames(parlits)), ]


pdf('tabfigs/parentlitcoefs.pdf', width=10)
par(mfrow=c(1, 2))
dotchart(parlits$momlit_est, labels=rownames(parlits), main='mother', xlim=c(0, 0.3), mar=c(1, 1, 1, 1))
points(parlits$dadlit_est, 1:nrow(parlits), col='gray')
segments(parlits$momlit_est + 2*parlits$momlit_stder, 1:nrow(parlits), parlits$momlit_est - 2*parlits$momlit_stder, 1:nrow(parlits))
dotchart(parlits$dadlit_est, labels=rownames(parlits), main='father', xlim=c(0, 0.3))
points(parlits$momlit_est, 1:nrow(parlits), col='gray')
segments(parlits$dadlit_est + 2*parlits$dadlit_stder, 1:nrow(parlits), parlits$dadlit_est - 2*parlits$dadlit_stder, 1:nrow(parlits))
dev.off()

extensions <- NULL
names <- NULL
for (dat in enrldats){
  if (sum(is.na(dat$nupw)) > 0){
    next
  }
  m <- glm(enrol ~ nsib - hhid + age + male + nserv + urban
  + factor(birthorder) 
  + nupw + nlat
  + motheragefirstchild + spousagegap - hhid, data=dat, family=binomial(link='logit'))
  cfs <- logitmfx2(m, clustervar1='hhid')
  nupw <- cfs$mfx['nupw', 1:2]
  nlat <- cfs$mfx['nlat', 1:2]
  ests <- unlist(c(nupw, nlat))
  extensions <- rbind(extensions, ests)
  names <- c(names, unique(dat$smpl))
}
rownames(extensions) <- names
extensions <- as.data.frame(extensions)
names(extensions) <- c('nupw_est', 'nupw_stder', 'nlat_est', 'nlat_stder')
extensions <- extensions[rev(rownames(extensions)), ]

pdf('tabfigs/nupwcoefs.pdf')
par(mfrow=c(1,1))
dotchart(extensions$nupw_est, labels=rownames(extensions))
segments(extensions$nupw_est + 2*extensions$nupw_stder, 1:nrow(extensions), extensions$nupw_est - 2*extensions$nupw_stder, 1:nrow(extensions))
abline(v=0)
dev.off()

pdf('tabfigs/nlatcoefs.pdf')
par(mfrow=c(1,1))
dotchart(extensions$nlat_est, labels=rownames(extensions))
segments(extensions$nlat_est + 2*extensions$nlat_stder, 1:nrow(extensions), extensions$nlat_est - 2*extensions$nlat_stder, 1:nrow(extensions))
abline(v=0)
dev.off()


lits <- NULL
for (dat in litdats){  
  m <- glm(literate ~ nsib - hhid + age + male + nserv + urban
  + factor(birthorder)
  + motheragefirstchild + spousagegap - hhid, data=dat, family=binomial(link='logit'))
  mbas <- logitmfx2(m, clustervar1='hhid')
  # male <- mbas$mfx['male', 1:2]
  nsib <- mbas$mfx['nsib', 1:2]
  lits <- rbind(lits, nsib)
}
rownames(lits) <- sapply(litdats, function(x) unique(x$smpl))
lits <- as.data.frame(lits)
names(lits) <- c('nsib_est', 'nsib_stder') #, 'nbro_est', 'nbro_stder', 'nsis_est', 'nsis_stder', 'male_est', 'male_stder')
lits$iso3 <- sapply(litdats, function(x) unique(x$iso3))
lits$year <- sapply(litdats, function(x) max(x$year))
lits$ccode <- countrycode(lits$iso3, 'iso3c', 'iso3n')
lits <- cbind(lits, clio[match(paste0(lits$ccode, lits$year), paste0(clio$ccode, clio$year)), ])
lits <- cbind(lits, ss[match(rownames(lits), ss$sample), ])

pdf('tabfigs/nsibcoefs_literate.pdf')
par(mfrow=c(1,1))
dotchart(lits$nsib_est, labels=rownames(lits))
segments(lits$nsib_est + 2*lits$nsib_stder, 1:24, lits$nsib_est - 2*lits$nsib_stder, 1:24)
# dotplot(x=lits$nsib_est, ses=lits$nsib_stder*2, labels=rownames(lits),
#   mar=c(4, 9, 3.5, 0.5))
abline(v=0)
dev.off()

###################
###  appendix #####
### regressions ###
###################
note <- '%stars. Standard errors clustered at household-level. Occup. and region controls included'
modnames <- c('OLS', 'OLS', 'IV', 'Logit', 'Mfx')
modnames_lit <- c('OLS', 'OLS', 'IV', 'OLS-lit', 'Logit-enrol', 'Mfx')
# Austria 1910
# ------------
au10$fatheroccupation <- uniformise_string(au10$fatheroccupation)
au10$fatheroccupation[lowfreqind(au10$fatheroccupation, 3)] <- 'other'
au10$motheroccupation <- uniformise_string(au10$motheroccupation)
au10$motheroccupation[lowfreqind(au10$motheroccupation, 3)] <- 'other'
au10$motherreligion [grepl('prot|jew', au10$motherreligion)] <- 'other'
au10$region[au10$region=='Habsburg Empire/Archduchy of Lower Austria'] <- 
  "Habsburg Empire/Archduchy of Upper Austria"

mb_au10 <- lm(enrol ~ nsib + age + male + nserv + urban 
 + nupw + nlat 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation + motheroccupation 
 + region
 - hhid, data=au10)
mli_au10 <- lm(literate ~ nsib + age + male + nserv + urban 
 + nupw + nlat 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation + motheroccupation 
 + region
 - hhid, data=au10)
mi_au10 <- ivreg(enrol ~ nsib + age + male + nserv + urban 
 + nupw + nlat 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation + motheroccupation 
 + region 
 - hhid | . - nsib + hhtwin, data=au10)
mg_au10 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban 
 + nupw + nlat 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation + motheroccupation 
 + region
 - hhid, data=au10)
ml_au10 <- glm(enrol ~ nsib + age + male + nserv + urban 
 + nupw + nlat 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation + motheroccupation 
 + region
 - hhid, data=au10, family=binomial(link='logit'))
models <- reglist(list(mb_au10, mg_au10, mi_au10, mli_au10, ml_au10))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  omit.coef='occup|region|race', stars=c(0.1, 0.05, 0.01), digits=3,
  caption=paste0(gsub('[-_]', ' ', unique(au10$smpl)), ', enrolment'), 
  custom.note=note,
  file='tabfigs/au10_enrol.tex',
  custom.model.names=modnames_lit)

# austria 1632-
#--------------
# ml model fails on region
ftable(au32$motherreligion, au32$enrol) # leave out 
ftable(au32$motheroccupation, au32$enrol)
au32$fatheroccupation <- uniformise_string(au32$fatheroccupation)
au32$motheroccupation <- uniformise_string(au32$motheroccupation)
au32$motheroccupation[lowfreqind(au32$motheroccupation, 13)] <- 'Other'
au32$fatheroccupation[lowfreqind(au32$fatheroccupation, 50)] <- 'Other'
au32$birthorder[au32$birthorder > 5] <- '> 5' 
au32$region[au32$region %in% c("Herrengasse", "Gaudenzdorf", "Ringturm",
          "Schottenfeld", "Leopoldsdorf","Gumpendorf",
          "Kaiserebersdorf", "Josefstadt",  "Neubau")] <- 'Vienna'
au32$century <- substr(au32$year, 1, 2)


mb_au32 <- lm(enrol ~ nsib + age + male + nserv + urban 
 + nextd 
 + factor(birthorder) + 
  motheragefirstchild + spousagegap - hhid
 # + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation # + motheroccupation 
 # + region 
 + factor(century)
 , data=au32)
mg_au32 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban 
 + nextd 
 + factor(birthorder) + 
  motheragefirstchild + spousagegap - hhid
 # + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation # + motheroccupation 
 # + region 
 + factor(century)
 , data=au32)
mi_au32 <- ivreg(enrol ~ nsib + age + male + nserv + urban 
 + nextd 
 + factor(birthorder) + 
  motheragefirstchild + spousagegap - hhid
 # + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation # + motheroccupation 
 # + region 
 + factor(century) | . - nsib + hhtwin
 , data=au32)
ml_au32 <- glm(enrol ~ nsib + age + male + nserv + urban 
 + nextd 
 + factor(birthorder) + 
  motheragefirstchild + spousagegap - hhid
 # + fatherliterate + motherliterate 
 # + fatherreligion + motherreligion
 + fatheroccupation # + motheroccupation 
 # + region 
 + factor(century)
 , data=au32, family=binomial(link='logit'))
models <- reglist(list(mb_au32, mg_au32, mi_au32, ml_au32))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  omit.coef='occup|region|race', stars=c(0.1, 0.05, 0.01), digits=3,
  caption='Austria 1632-1947, enrolment', 
  custom.note='%stars. Standard errors clustered at household-level. Occup. controls included.',
  file='tabfigs/au32_enrol.tex',
  custom.model.names=modnames)

# canada 1891
#------------
# canada 1891 is very difficult to estimate with age > 13
# ftable(ca91$fatheroccupation, ca91$enrol)
# ftable(ca91$motheroccupation, ca91$enrol)
ca91$fatheroccupation <- substring(ca91$fatherocchisco, 1, 1)
ca91$motheroccupation <- substring(ca91$motherocchisco, 1, 1)
ca91$motheroccupation[ca91$motheroccupation <= 6] <- 1
ca91$motheroccupation[ca91$motheroccupation >= 7] <- 7
# ftable(ca91$region, ca91$enrol)
# ftable(ca91$fatherreligion, ca91$enrol)
# ftable(ca91$motherreligion, ca91$enrol)
ca91$fatherreligion[grepl('catholic', ca91$fatherreligion)] <- 'other'
ca91$motherreligion[grepl('catholic', ca91$motherreligion)] <- 'other'
# ftable(ca91$fatherliterate, ca91$enrol)
# ftable(ca91$motherliterate, ca91$enrol)
# ftable(ca91$fatheroccupation, ca91$enrol)
# ftable(ca91$birthorder, ca91$enrol)
ca91$birthorder[ca91$birthorder > 4] <- '> 4'

mb_ca91 <- lm(enrol ~ nsib + age + male + nserv + # urban + 
  nlat + nupw + # factor(birthorder) + 
  motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion
 + factor(fatheroccupation) + factor(motheroccupation) 
 # + region
 - hhid
 , data=ca91)
mli_ca91 <- lm(literate ~ nsib + age + male + nserv + # urban + 
  nlat + nupw + # factor(birthorder) + 
  motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion
 + factor(fatheroccupation) + factor(motheroccupation) 
 # + region
 - hhid
 , data=ca91)
mi_ca91 <- ivreg(enrol ~ nsib + age + male + nserv + # urban + 
  nlat + nupw + # factor(birthorder) + 
  motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion
 + factor(fatheroccupation) + factor(motheroccupation) 
 # + region
 - hhid | . - nsib + hhtwin
 , data=ca91)
mg_ca91 <- lm(enrol ~ nbro + nsis + age + male + nserv + # urban + 
  nlat + nupw + # factor(birthorder) + 
  motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion
 + factor(fatheroccupation) + factor(motheroccupation) 
 # + region
 - hhid
 , data=ca91)
ml_ca91 <- glm(enrol ~ nsib + age + male + nserv + # urban + 
  nlat + nupw + # factor(birthorder) + 
  motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion
 + factor(fatheroccupation) + factor(motheroccupation) 
 # + region
 - hhid
 , data=ca91, family=binomial(link='logit'))
models <- reglist(list(mb_ca91, mg_ca91, mi_ca91, mli_ca91, ml_ca91))
texreg(models$m, override.se=models$se, override.pval=models$pv,
  stars=c(0.1, 0.05, 0.01), digits=3,
  caption=paste0(gsub('[-_]', ' ', unique(ca91$smpl)), ', enrolment'), 
  custom.note='%stars. Standard errors clustered at household-level. Occup. controls included.',
  omit.coef='occup|region|race', 
  file='tabfigs/ca91_enrol.tex',
  custom.model.names=modnames_lit)


# canada 1901
# -----------
# ca01$fatherethnic[ca01$fatherethnic < 10000] <- 'USA'
# ca01$fatherethnic[ca01$fatherethnic < 16000 & ca01$fatherethnic > 10000] <- 'Canadian'
# ca01$motherethnic[ca01$motherethnic < 10000] <- 'USA'
# ca01$motherethnic[ca01$motherethnic < 16000 & ca01$motherethnic > 10000] <- 'Canadian'

ca01$fatheroccupation <- substring(ca01$fatheroccode, 1, 1)
ca01$fatheroccupation[ca01$fatheroccupation <= 3] <- 3
ca01$region[grepl('AT|KE', ca01$region)] <- 'ATKE'
ca01$region[lowfreqind(ca01$region, 132)] <- 'other'

ca01$motheroccupation <- substring(ca01$fatheroccode, 1, 1)
ca01$birthorder[ca01$birthorder > 6] <- '> 6'
ca01$fatherreligion[lowfreqind(ca01$fatherreligion, 100)] <- 'other'
ca01$motherreligion[lowfreqind(ca01$motherreligion, 100)] <- 'other'

mb_ca01 <- lm(enrol ~ nsib + age + male + nserv + urban 
 + nlat + nupw 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate # lose sign here
 + fatherreligion + motherreligion 
 + factor(fatherethnic) + factor(motherethnic) 
 + factor(fatheroccupation) # + factor(motheroccupation) 
 + region
 - hhid
 , data=ca01)
mli_ca01 <- lm(literate ~ nsib + age + male + nserv + urban 
 + nlat + nupw 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate # lose sign here
 + fatherreligion + motherreligion 
 + factor(fatherethnic) + factor(motherethnic) 
 + factor(fatheroccupation) # + factor(motheroccupation) 
 + region
 - hhid
 , data=ca01)
mg_ca01 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban 
 + nlat + nupw 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion 
 + factor(fatherethnic) + factor(motherethnic) 
 + factor(fatheroccupation) # + factor(motheroccupation) 
 + region
 - hhid
 , data=ca01)
mi_ca01 <- ivreg(enrol ~ nsib + age + male + nserv + urban 
 + nlat + nupw 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion 
 + factor(fatherethnic) + factor(motherethnic) 
 + factor(fatheroccupation) # + factor(motheroccupation) 
 + region 
 - hhid | . - nsib + hhtwin
 , data=ca01)
ml_ca01 <- glm(enrol ~ nsib + age + male + nserv + urban 
 + nlat + nupw 
 + factor(birthorder) 
 + motheragefirstchild + spousagegap 
 + fatherliterate + motherliterate 
 + fatherreligion + motherreligion 
 + factor(fatherethnic) + factor(motherethnic) 
 + factor(fatheroccupation) # + factor(motheroccupation) 
 + region
 - hhid
 , data=ca01, family=binomial(link='logit'))
models <- reglist(list(mb_ca01, mg_ca01, mi_ca01, mli_ca01, ml_ca01))
texreg(models$m, override.se=models$se, override.pval=models$pv,
  omit.coef='occup|region|race', stars=c(0.1, 0.05, 0.01), digits=3,
  caption=paste0(gsub('[-_]', ' ', unique(ca01$smpl)), ', enrolment'), 
  custom.note=note,
  file='tabfigs/ca01_enrol.tex',
  custom.model.names=modnames_lit)

# hungary 1869
#-------------
ftable(hu69$birthorder, hu69$enrol)
hu69$birthorder[hu69$birthorder > 4] <- '> 4'
ftable(hu69$fatherreligion, hu69$enrol) # keep motherreligion out
ftable(hu69$fatheroccupation, hu69$enrol)
hu69$fatheroccupation <- uniformise_string(hu69$fatheroccupation)
hu69$fatheroccupation[lowfreqind(hu69$fatheroccupation, 2)] <- 'other'
hu69$motheroccupation <- uniformise_string(hu69$motheroccupation)
hu69$motheroccupation[lowfreqind(hu69$motheroccupation, 2)] <- 'other'
hu69$region[grepl('Partium|Transylvania', hu69$region)] <- 'Partium/Transylvania'

mb_hu69 <- lm(enrol ~ nsib + age + male + nserv # + urban
  + nlat 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion # + motherreligion
  + fatheroccupation # motheroccupation
  + region # does better w/o the regional controls
  - hhid
  , data=hu69)
mli_hu69 <- lm(literate ~ nsib + age + male + nserv # + urban
  + nlat 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion # + motherreligion
  + fatheroccupation # motheroccupation
  + region # does better w/o the regional controls
  - hhid
  , data=hu69)
mi_hu69 <- ivreg(enrol ~ nsib + age + male + nserv # + urban
  + nlat 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion # + motherreligion
  + fatheroccupation # motheroccupation
  + region 
  - hhid | . - nsib + hhtwin
  , data=hu69)
mg_hu69 <- lm(enrol ~ nbro + nsis + age + male + nserv # + urban
  + nlat 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion # + motherreligion
  + fatheroccupation # motheroccupation
  + region
  - hhid
  , data=hu69)
ml_hu69 <- glm(enrol ~ nsib + age + male + nserv # + urban
  + nlat 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion # + motherreligion
  # + fatheroccupation # motheroccupation # fatherocc causes clustered SE failure
  + region
  - hhid
  , data=hu69, family=binomial(link='logit'))
models <- reglist(list(mb_hu69, mg_hu69, mi_hu69, mli_hu69, ml_hu69))
texreg(models$m, override.se=models$se, override.pval=models$pv,
  omit.coef='occup|region|race', stars=c(0.1, 0.05, 0.01), digits=3,
  caption=paste0(gsub('[-_]', ' ', unique(hu69$smpl)), ', enrolment'), 
  custom.note=paste0(note, ', except in logit model.'),
  file='tabfigs/hu69_enrol.tex',
  custom.model.names=modnames_lit)

# ireland 1901
#-------------

ie01$fatheroccupation <- uniformise_string(ie01$fatheroccupation)
ie01$motheroccupation <- uniformise_string(ie01$motheroccupation)
ie01$fatheroccupation[lowfreqind(ie01$fatheroccupation, freq=10)] <- 'other'
ie01$motheroccupation[lowfreqind(ie01$motheroccupation, freq=10)] <- 'other'
ie01$motherreligion[grepl('jewish', ie01$motherreligion)] <- 'other'
# # ie01$birthorder[ie01$birthorder > 5] <- '> 5'

mb_ie01 <- lm(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie01)
mli_ie01 <- lm(literate ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie01)
mg_ie01 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie01)
mi_ie01 <- ivreg(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  # + fatheroccupation + motheroccupation
  + region
  - hhid | . - nsib + hhtwin
  , data=ie01)
ml_ie01 <- glm(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie01, family=binomial(link='logit'))
models <- reglist(list(mb_ie01, mg_ie01, mi_ie01, mli_ie01, ml_ie01))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(ie01$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames_lit,
  file='tabfigs/ie01_enrol.tex',
  omit.coef='occup|region|race', stars=c(0.1, 0.05, 0.01), digits=3)

# ireland 1911
#-------------
ie11$fatheroccupation <- uniformise_string(ie11$fatheroccupation)
ie11$fatheroccupation[lowfreqind(ie11$fatheroccupation, freq=10)] <- 'other' # 5?
ie11$motheroccupation <- uniformise_string(ie11$motheroccupation)
ie11$motheroccupation[lowfreqind(ie11$motheroccupation, freq=10)] <- 'other' # 5?
# x$birthorder[x$birthorder > 7] <- '> 7'
# x$motherreligion[x$motherreligion=='jewish'] <- 'other'
# x$fatherreligion[x$fatherreligion=='jewish'] <- 'other'
# ftable(x$enrol, x$motherreligion)

mb_ie11 <- lm(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie11)
mli_ie11 <- lm(literate ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie11)
mg_ie11 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie11)
mi_ie11 <- ivreg(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  # + fatheroccupation + motheroccupation
  + region
  - hhid | . - nsib + hhtwin
  , data=ie11)
ml_ie11 <- glm(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  + fatherreligion + motherreligion
  + fatheroccupation + motheroccupation
  + region
  - hhid
  , data=ie11, family=binomial(link='logit'))
models <- reglist(list(mb_ie11, mg_ie11, mi_ie11, mli_ie11, ml_ie11))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(ie11$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames_lit,
  file='tabfigs/ie11_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# jasenica 1984
#--------------
# ftable(ja84$birthorder, ja84$enrol)
ja84$birthorder[ja84$birthorder > 3] <- '>3'
# ftable(ja84$enrol, ja84$motherliterate)
ftable(ja84$enrol, ja84$fatherreligion)
ftable(ja84$enrol, ja84$motherreligion)
ja84$fatheroccupation <- uniformise_string(ja84$fatheroccupation)

mb_ja84 <- lm(enrol ~ nsib + age + male + nserv # + urban
  + nlat + nupw  
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  # + fatherreligion + motherreligion
  + fatheroccupation # + motheroccupation # all motheroccuption = unknown
  # + region # all jasenica
  - hhid
  , data=ja84)
mli_ja84 <- lm(literate ~ nsib + age + male + nserv # + urban
  + nlat + nupw  
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  # + fatherreligion + motherreligion
  + fatheroccupation # + motheroccupation # all motheroccuption = unknown
  # + region # all jasenica
  - hhid
  , data=ja84)
mg_ja84 <- lm(enrol ~ nbro + nsis + age + male + nserv # + urban
  + nlat + nupw  
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  # + fatherreligion + motherreligion
  + fatheroccupation # + motheroccupation # all motheroccuption = unknown
  # + region # all jasenica
  - hhid
  , data=ja84)
mi_ja84 <- ivreg(enrol ~ nsib + age + male + nserv # + urban
  + nlat + nupw  
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  # + fatherreligion + motherreligion
  + fatheroccupation # + motheroccupation # all motheroccuption = unknown
  # + region # all jasenica
  - hhid | . - nsib + hhtwin
  , data=ja84)
ml_ja84 <- glm(enrol ~ nsib + age + male + nserv # + urban
  + nlat + nupw  
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate
  # + fatherreligion + motherreligion
  + fatheroccupation # + motheroccupation # all motheroccuption = unknown
  # + region # all jasenica
  - hhid
  , data=ja84, family=binomial(link='logit'))
models <- reglist(list(mb_ja84, mg_ja84, mi_ja84, mli_ja84, ml_ja84))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(ja84$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames_lit,
  file='tabfigs/ja84_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# Kruja 1918
#-----------
kr18$motheroccupation[grepl('bulk|bauer|grund$', kr18$motheroccupation)] <- 'Farmer'
kr18$motheroccupation[grepl('rbeiter', kr18$motheroccupation)] <- 'Labourer'
kr18$motheroccupation[lowfreqind(kr18$motheroccupation, freq=80)] <- 'Other'
kr18$fatheroccupation[grepl('bulk|bauer|grund$', kr18$fatheroccupation)] <- 'Farmer'
kr18$fatheroccupation[grepl('rbeiter', kr18$fatheroccupation)] <- 'Labourer'
kr18$fatheroccupation[lowfreqind(kr18$fatheroccupation, freq=80)] <- 'Other'
# kr18$fatheroccupation[kr18$fatheroccupation=='*'] <- NA
kr18$birthorder[kr18$birthorder > 4] <- '> 4'
# ftable(kr18$enrol, kr18$birthorder)
# ftable(kr18$enrol, kr18$fatherethnic)
# ftable(kr18$enrol, kr18$fatheroccupation)
# ftable(kr18$enrol, kr18$fatherreligion)
# ftable(kr18$enrol, kr18$motherethnic)
# ftable(kr18$enrol, kr18$fatherreligion)
# ftable(x$enrol, x$fatheroccupation) # perfect predictors, not in case of lit
# f <- enrol ~ nsib + age + male + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid

mb_kr18 <- lm(enrol ~ nsib + age + male + nserv  + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate # + motherliterate # all mother illiterate
  + fatherreligion # + motherreligion
  # + fatherethnic # + motherethnic
  + fatheroccupation + motheroccupation
  # + region
  - hhid
  , data=kr18)
mli_kr18 <- lm(literate ~ nsib + age + male + nserv  + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate # + motherliterate # all mother illiterate
  + fatherreligion # + motherreligion
  # + fatherethnic # + motherethnic
  + fatheroccupation + motheroccupation
  # + region
  - hhid
  , data=kr18)
mg_kr18 <- lm(enrol ~ nbro + nsis + age + male + nserv  + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate # + motherliterate # all mother illiterate
  + fatherreligion # + motherreligion
  # + fatherethnic # + motherethnic
  + fatheroccupation + motheroccupation
  # + region
  - hhid
  , data=kr18)
mi_kr18 <- ivreg(enrol ~ nsib + age + male + nserv  + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate # + motherliterate # all mother illiterate
  + fatherreligion # + motherreligion
  # + fatherethnic # + motherethnic
  + fatheroccupation + motheroccupation
  # + region
  - hhid | . - nsib + hhtwin
  , data=kr18)
ml_kr18 <- glm(enrol ~ nsib + age + male + nserv  + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatherliterate # + motherliterate # all mother illiterate
  + fatherreligion # + motherreligion
  # + fatherethnic # + motherethnic
  + fatheroccupation # + motheroccupation # should really leave out fatheroc
  # + region
  - hhid
  , data=kr18, family=binomial(link='logit'))
models <- reglist(list(mb_kr18, mg_kr18, mi_kr18, mli_kr18, ml_kr18))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(kr18$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames_lit,
  file='tabfigs/kr18_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# usa 1850
#---------
# no servants information
# imputed incomes are only option because occhisco is missing

# x$fatheroccupation <- uniformise_string(x$fatheroccupation)
# sort(table(x$fatheroccupation))
# x$fatheroccupation[lowfreqind(x$fatheroccupation, 25)] <- 'other'
us50$fatherincome <- us50$fatherocscorus
us50$motherincome <- us50$motherocscorus
usnote <- '%stars. Standard errors clustered at household-level. Regional and race controls included.'
# ftable(x$age, x$enrol) / rowSums(ftable(x$age, x$enrol))
us50$birthorder[us50$birthorder > 9] <- '>9'
us50$fatherrace <- substring(us50$fatherrace, 1, 1)
us50$motherrace <- substring(us50$motherrace, 1, 1)

mb_us50 <- lm(enrol ~ nsib + age + male + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap
  + fatherliterate + motherliterate
  + factor(fatherrace) + factor(motherrace)
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes positive coef
  - hhid
  , data=us50)
# literacy only recorded age > 20
mg_us50 <- lm(enrol ~ nsis + nbro + age + male + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap
  + fatherliterate + motherliterate
  + factor(fatherrace) + factor(motherrace)
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes positive coef
  - hhid
  , data=us50)
mi_us50 <- ivreg(enrol ~ nsib + age + male + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap
  + fatherliterate + motherliterate
  + factor(fatherrace) + factor(motherrace)
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region)
  - hhid | . - nsib + hhtwin
  , data=us50)
ml_us50 <- glm(enrol ~ nsib + age + male + urban # no servants in this dataset
  + nlat + nupw 
  + factor(birthorder) # bo halves nsib effect here
  + motheragefirstchild + spousagegap # motheragefirst child lesses further to ~ 0
  + fatherliterate + motherliterate # and from here on it's positive (but insg)
  + factor(fatherrace) + factor(motherrace)
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes positive coef
  - hhid
  , data=us50, family=binomial(link='logit'))
models <- reglist(list(mb_us50, mg_us50, mi_us50, ml_us50))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(us50$smpl)), ', enrolment'), 
  custom.note=usnote,
  custom.model.names=modnames,
  file='tabfigs/us50_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# USA 1860
#---------
us60$birthorder[us60$birthorder > 9] <- '>9'
us60$fatherincome <- us60$fatherocscorus
us60$motherincome <- us60$motherocscorus
us60$fatherrace <- substring(us60$fatherrace, 1, 1)
us60$motherrace <- substring(us60$motherrace, 1, 1)

mb_us60 <- lm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) # + factor(motherrace)
  # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) 
  - hhid
  , data=us60)
# literacy only age > 20
mg_us60 <- lm(enrol ~ nsib + nbro + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) # + factor(motherrace)
  # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) 
  - hhid
  , data=us60)
mi_us60 <- ivreg(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) # + factor(motherrace)
  # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) 
  - hhid | . - nsib + hhtwin
  , data=us60)
ml_us60 <- glm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo does not diminish nsib here
  + motheragefirstchild + spousagegap # motheragefirstchild makes ~ 0
  + fatherliterate + motherliterate # and from here on it's positive (but insg)
  + factor(fatherrace) # + factor(motherrace) # race = 300 ID for mother and father
  # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes positive coef
  - hhid
  , data=us60, family=binomial(link='logit'))
models <- reglist(list(mb_us60, mg_us60, mi_us60, ml_us60))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(us60$smpl)), ', enrolment'), 
  custom.note=paste0(usnote),
  custom.model.names=modnames,
  file='tabfigs/us60_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# USA 1870
#---------
us70$birthorder[us70$birthorder > 9] <- '>9'
us70$fatherincome <- us70$fatherocscorus
us70$motherincome <- us70$motherocscorus
us70$fatherrace <- substring(us70$fatherrace, 1, 1)
us70$motherrace <- substring(us70$motherrace, 1, 1)

mb_us70 <- lm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) # + factor(motherrace) 
  # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes ~ 0
  - hhid
  , data=us70)

ftable(us70$age, us70$literate) # no recorded lit < age 10
mli_us70 <- lm(literate ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) # + factor(motherrace) 
  # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes ~ 0
  - hhid
  , data=us70[age > 9, ]) # no recorde literacy before age 9
mg_us70 <- lm(enrol ~ nbro + nsis + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) # + factor(motherrace) 
  # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes ~ 0
  - hhid
  , data=us70)
mi_us70 <- ivreg(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) # + factor(motherrace) 
  # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes ~ 0
  - hhid | . - nsib + hhtwin
  , data=us70)
ml_us70 <- glm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo does not diminish nsib here
  + motheragefirstchild + spousagegap # motheragefirstchild diminishes, but qqt present
  + fatherliterate + motherliterate # makes it ~ 0
  + factor(fatherrace) # + factor(motherrace) # makes it negative again; motherrace400 aliased
  # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes ~ 0
  - hhid
  , data=us70, family=binomial(link='logit'))
models <- reglist(list(mb_us70, mg_us70, mi_us70, mli_us70, ml_us70))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(us70$smpl)), ', enrolment'), 
  custom.note=paste0(usnote),
  custom.model.names=modnames_lit,
  file='tabfigs/us70_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# USA 1880
#----------
us80$birthorder[us80$birthorder > 9] <- '>9'
us80$fatherincome <- us80$fatherocscorus
us80$motherincome <- us80$motherocscorus
us80$fatherrace <- substring(us80$fatherrace, 1, 1)
us80$motherrace <- substring(us80$motherrace, 1, 1)

mb_us80 <- lm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo makes nsib more negative
  + motheragefirstchild + spousagegap # motheragefirstchild diminishes effect slightly
  + fatherliterate + motherliterate # makes it ~ 0
  + factor(fatherrace) + factor(motherrace)
  # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes it positive again...
  - hhid
  , data=us80)
mli_us80 <- lm(literate ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo makes nsib more negative
  + motheragefirstchild + spousagegap # motheragefirstchild diminishes effect slightly
  + fatherliterate + motherliterate # makes it ~ 0
  + factor(fatherrace) + factor(motherrace)
  # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes it positive again...
  - hhid
  , data=us80[age > 9 ,])
mg_us80 <- lm(enrol ~ nsis + nbro + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo makes nsib more negative
  + motheragefirstchild + spousagegap # motheragefirstchild diminishes effect slightly
  + fatherliterate + motherliterate # makes it ~ 0
  + factor(fatherrace) + factor(motherrace)
  # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes it positive again...
  - hhid
  , data=us80)
mi_us80 <- ivreg(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo makes nsib more negative
  + motheragefirstchild + spousagegap # motheragefirstchild diminishes effect slightly
  + fatherliterate + motherliterate # makes it ~ 0
  + factor(fatherrace) + factor(motherrace)
  # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes it positive again...
  - hhid | . - nsib + hhtwin
  , data=us80)
ml_us80 <- glm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo makes nsib more negative
  + motheragefirstchild + spousagegap # motheragefirstchild diminishes effect slightly
  + fatherliterate + motherliterate # makes it ~ 0
  + factor(fatherrace) + factor(motherrace)
  # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome)
  + factor(region) # makes it positive again...
  - hhid
  , data=us80, family=binomial(link='logit'))
models <- reglist(list(mb_us80, mg_us80, mi_us80, mli_us80, ml_us80))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(us80$smpl)), ', enrolment'), 
  custom.note=paste0(usnote),
  custom.model.names=modnames_lit,
  file='tabfigs/us80_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# USA 1900
#---------
us00$birthorder[us00$birthorder > 9] <- '>9'
us00$fatherincome <- us00$fatherocscorus
us00$motherincome <- us00$motherocscorus
us00$fatherrace <- substring(us00$fatherrace, 1, 1)
us00$motherrace <- substring(us00$motherrace, 1, 1)

mb_us90 <- lm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) + factor(motherrace) 
  # # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) 
  + factor(region) 
  - hhid
  , data=us00)
mli_us90 <- lm(literate ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) + factor(motherrace) 
  # # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) 
  + factor(region) 
  - hhid
  , data=us00[age > 9, ])
mg_us90 <- lm(enrol ~ nbro + nsis + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) + factor(motherrace) 
  # # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) 
  + factor(region) 
  - hhid
  , data=us00)
mi_us90 <- ivreg(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) + factor(motherrace) 
  # # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) 
  + factor(region) 
  - hhid | . - nsib + hhtwin
  , data=us00)
ml_us90 <- glm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo strenghtens
  + motheragefirstchild + spousagegap # slight diminishment
  + fatherliterate + motherliterate # slight further diminishment
  + factor(fatherrace) + factor(motherrace) # ditto, motherrace130 aliased
  # # # # # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) # no change
  + factor(region) # makes it positive again...
  - hhid
  , data=us00, family=binomial(link='logit'))
models <- reglist(list(mb_us90, mg_us90, mi_us90, mli_us90, ml_us90))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(us00$smpl)), ', enrolment'), 
  custom.note=paste0(usnote),
  custom.model.names=modnames_lit,
  file='tabfigs/us00_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# USA 1910
#---------
us10$birthorder[us10$birthorder > 9] <- '>9'
us10$fatherincome <- us10$fatherocscorus
us10$motherincome <- us10$motherocscorus
us10$fatherrace <- substring(us10$fatherrace, 1, 1)
us10$motherrace <- substring(us10$motherrace, 1, 1)

mb_us10 <- lm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo strenghtens
  + motheragefirstchild + spousagegap # slight diminishment
  + fatherliterate + motherliterate # further diminishment
  + factor(fatherrace) + factor(motherrace) # ditto, motherrace130 aliased
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) # no change
  + factor(region) # makes it positive again...
  - hhid
  , data=us10)
mli_us10 <- lm(literate ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) # bo strenghtens
  + motheragefirstchild + spousagegap # slight diminishment
  + fatherliterate + motherliterate # further diminishment
  + factor(fatherrace) + factor(motherrace) # ditto, motherrace130 aliased
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) # no change
  + factor(region) # makes it positive again...
  - hhid
  , data=us10[age > 9, ])
mg_us10 <- lm(enrol ~ nbro + nsis + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) + factor(motherrace) 
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) 
  + factor(region) 
  - hhid
  , data=us10)
mi_us10 <- ivreg(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) + factor(motherrace) 
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) 
  + factor(region) 
  - hhid | . - nsib + hhtwin
  , data=us10)
ml_us10 <- glm(enrol ~ nsib + age + male + urban + nserv 
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatherliterate + motherliterate 
  + factor(fatherrace) + factor(motherrace) 
  # + fatheroccupation + motheroccupation
  + log1p(fatherincome) + log1p(motherincome) 
  + factor(region) 
  - hhid
  , data=us10, family=binomial(link='logit'))
models <- reglist(list(mb_us10, mg_us10, mi_us10, mli_us10, ml_us10))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(us10$smpl)), ', enrolment'), 
  custom.note=paste0(usnote),
  custom.model.names=modnames_lit,
  file='tabfigs/us10_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# Rostock 1867
#-------------
ro67$fatheroccupation <- uniformise_string(ro67$fatheroccupation)
ro67$fatheroccupation[lowfreqind(ro67$fatheroccupation, 6)] <- 'other'
ro67$motheroccupation <- uniformise_string(ro67$motheroccupation)
ro67$motheroccupation[lowfreqind(ro67$motheroccupation, 6)] <- 'other'
ro67$birthorder[ro67$birthorder > 5] <- '> 5'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatheroccupation + motheroccupation

mb_ro67 <- lm(enrol ~ nsib + age + male + nserv # + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  - hhid
  , data=ro67)
mg_ro67 <- lm(enrol ~ nbro + nsis + age + male + nserv # + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  - hhid
  , data=ro67)
mi_ro67 <- ivreg(enrol ~ nsib + age + male + nserv # + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  - hhid | . - nsib + hhtwin
  , data=ro67)
ml_ro67 <- glm(enrol ~ nsib + age + male + nserv # + urban
  + nlat + nupw 
  + factor(birthorder)
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  - hhid
  , data=ro67, family=binomial(link='logit'))
models <- reglist(list(mb_ro67, mg_ro67, mi_ro67, ml_ro67))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(ro67$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames,
  file='tabfigs/ro67_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# englang and wales, 1881
#------------------------
en81$fatheroccupation <- en81$fatherordergb
en81$motheroccupation <- en81$motherordergb
# ftable(en81$birthorder, en81$enrol)
# # x$birthorder[x$birthorder > 7] <- '> 7'

mb_en81 <- lm(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder) # strengthens nsib
  + motheragefirstchild + spousagegap # weakens slightly
  + fatheroccupation + motheroccupation # entering it as factor makes little dif
  + factor(region) # strengthens
  - hhid
  , data=en81)
mg_en81 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder) # strengthens nsib
  + motheragefirstchild + spousagegap # weakens slightly
  + fatheroccupation + motheroccupation # entering it as factor makes little dif
  + factor(region) # strengthens
  - hhid
  , data=en81)
mi_en81 <- ivreg(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder) # strengthens nsib
  + motheragefirstchild + spousagegap # weakens slightly
  + fatheroccupation + motheroccupation # entering it as factor makes little dif
  + factor(region) # strengthens
  - hhid | . - nsib + hhtwin
  , data=en81)
ml_en81 <- glm(enrol ~ nsib + age + male + nserv + urban
  + nlat + nupw 
  + factor(birthorder) # strengthens nsib
  + motheragefirstchild + spousagegap # weakens slightly
  + fatheroccupation + motheroccupation # entering it as factor makes little dif
  + factor(region) # strengthens
  - hhid
  , data=en81, family=binomial(link='logit'))
models <- reglist(list(mb_en81, mg_en81, mi_en81, ml_en81))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(en81$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames,
  file='tabfigs/en81_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# great britain 1851
#--------------------

gr51$fatheroccupation <- gr51$fatherordergb
gr51$motheroccupation <- gr51$motherordergb
# ftable(gr51$birthorder, gr51$enrol)
gr51$birthorder[gr51$birthorder > 8] <- '> 8'

mb_gr51 <- lm(enrol ~ nsib + age + male + nserv + urban # negative but insig.
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  + factor(region)
  - hhid
  , data=gr51)
mg_gr51 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban # negative but insig.
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  + factor(region)
  - hhid
  , data=gr51)
mi_gr51 <- ivreg(enrol ~ nsib + age + male + nserv + urban # negative but insig.
  + nlat + nupw 
  + factor(birthorder) 
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  + factor(region)
  - hhid | . - nsib + hhtwin
  , data=gr51)
ml_gr51 <- glm(enrol ~ nsib + age + male + nserv + urban # negative but insig.
  + nlat + nupw 
  + factor(birthorder) # bo makes positive
  + motheragefirstchild + spousagegap 
  + fatheroccupation + motheroccupation
  + factor(region)
  - hhid
  , data=gr51, family=binomial(link='logit'))
models <- reglist(list(mb_gr51, mg_gr51, mi_gr51, ml_gr51))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(gr51$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames,
  file='tabfigs/gr51_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# san marcello 1827
#------------------
sa27$fatheroccupation <- uniformise_string(sa27$fatheroccupation)
sa27$fatheroccupation[lowfreqind(sa27$fatheroccupation, 8)] <- 'other'

mb_sa27 <- lm(enrol ~ nsib + age + male + nserv
  + factor(birthorder) # bo makes negative
  + nextd 
  + motheragefirstchild + spousagegap # motheragefirstchild makes more negative
  # + fatherreligion + motherreligion
  # + fatheroccupation # + motheroccupation
  + fatherberstel
  - hhid
  , data=sa27)
mg_sa27 <- lm(enrol ~ nbro + nsis + age + male + nserv
  + factor(birthorder) # bo makes negative
  + nextd 
  + motheragefirstchild + spousagegap # motheragefirstchild makes more negative
  # + fatherreligion + motherreligion
  # + fatheroccupation # + motheroccupation
  + fatherberstel
  - hhid
  , data=sa27)
mi_sa27 <- ivreg(enrol ~ nsib + age + male + nserv
  + factor(birthorder) # bo makes negative
  + nextd 
  + motheragefirstchild + spousagegap # motheragefirstchild makes more negative
  # + fatherreligion + motherreligion
  # + fatheroccupation # + motheroccupation
  + fatherberstel
  - hhid | . - nsib + hhtwin
  , data=sa27)
ml_sa27 <- glm(enrol ~ nsib + age + male + nserv
  + factor(birthorder) # bo makes negative
  + nextd 
  + motheragefirstchild + spousagegap # motheragefirstchild makes more negative
  # + fatherreligion + motherreligion
  # + fatheroccupation # + motheroccupation
  + fatherberstel
  - hhid
  , data=sa27, family=binomial(link='logit'))
models <- reglist(list(mb_sa27, mg_sa27, mi_sa27, ml_sa27))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(sa27$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames,
  file='tabfigs/sa27_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# scotland 1881
#----------------
sc81$fatheroccupation <- sc81$fatherordergb
sc81$motheroccupation <- sc81$motherordergb
# ftable(sc81$birthorder, sc81$enrol)
# sc81$birthorder[sc81$birthorder > 7] <- '> 7'

mb_sc81 <- lm(enrol ~ nsib + age + male + nserv + urban
  + factor(birthorder)
  + nlat + nupw 
  + motheragefirstchild + spousagegap
  + fatheroccupation + motheroccupation
  + factor(region)
  - hhid
  , data=sc81)
mg_sc81 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban
  + factor(birthorder)
  + nlat + nupw 
  + motheragefirstchild + spousagegap
  + fatheroccupation + motheroccupation
  + factor(region)
  - hhid
  , data=sc81)
mi_sc81 <- ivreg(enrol ~ nsib + age + male + nserv + urban
  + factor(birthorder)
  + nlat + nupw 
  + motheragefirstchild + spousagegap
  + fatheroccupation + motheroccupation
  + factor(region)
  - hhid | . - nsib + hhtwin
  , data=sc81)
ml_sc81 <- glm(enrol ~ nsib + age + male + nserv + urban
  + factor(birthorder) # bo strengthens
  + nlat + nupw 
  + motheragefirstchild + spousagegap # weakens
  + fatheroccupation + motheroccupation # factor makes no diff
  + factor(region)
  - hhid
  , data=sc81, family=binomial(link='logit'))
models <- reglist(list(mb_sc81, mg_sc81, mi_sc81, ml_sc81))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(sc81$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames,
  file='tabfigs/sc81_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# zurich 1870
#------------
zu70$fatherreligion[grepl('catho|jew|other', zu70$fatherreligion)] <- 'other'
zu70$motherreligion[grepl('catho|jew|other', zu70$motherreligion)] <- 'other'
zu70$fatheroccupation[lowfreqind(zu70$fatheroccupation, 7)] <- 'other'
zu70$motheroccupation[lowfreqind(zu70$motheroccupation, 7)] <- 'other'
zu70$birthorder[zu70$birthorder > 4] <- '> 4'

mb_zu70 <- lm(enrol ~ nsib + age + male + nserv 
  + factor(birthorder) # strengthens, but still insig
  + nextd 
  + motheragefirstchild + spousagegap 
  + fatherreligion # + motherreligion
  + fatheroccupation # + motheroccupation
  # + region
  - hhid
  , data=zu70)
mg_zu70 <- lm(enrol ~ nbro + nsis + age + male + nserv 
  + factor(birthorder) # strengthens, but still insig
  + nextd 
  + motheragefirstchild + spousagegap 
  + fatherreligion # + motherreligion
  + fatheroccupation # + motheroccupation
  # + region
  - hhid
  , data=zu70)
mi_zu70 <- ivreg(enrol ~ nsib + age + male + nserv 
  + factor(birthorder) # strengthens, but still insig
  + nextd 
  + motheragefirstchild + spousagegap 
  + fatherreligion # + motherreligion
  + fatheroccupation # + motheroccupation
  # + region
  - hhid | . - nsib + hhtwin
  , data=zu70)
ml_zu70 <- glm(enrol ~ nsib + age + male + nserv 
  + factor(birthorder) # strengthens, but still insig
  + nextd 
  + motheragefirstchild + spousagegap 
  + fatherreligion # + motherreligion
  + fatheroccupation # + motheroccupation
  # + region
  - hhid
  , data=zu70, family=binomial(link='logit'))
models <- reglist(list(mb_zu70, mg_zu70, mi_zu70, ml_zu70))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(gsub('[-_]', ' ', unique(zu70$smpl)), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames,
  file='tabfigs/zu70_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# Rostock 1900
#-------------
ftable(ro00$fatherreligion, ro00$enrol)
ftable(ro00$motherreligion, ro00$enrol)
ro00$fatherreligion[grepl('orthodox|other|jewish', ro00$fatherreligion)] <- 'other'
ro00$motherreligion[grepl('orthodox|other|jewish', ro00$motherreligion)] <- 'other'
ro00$fatheroccupation <- substr(ro00$fatherocchisco, 1, 1)
ro00$motheroccupation <- substr(ro00$motherocchisco, 1, 1)
ro00$birthorder[ro00$birthorder > 6] <- '> 6'

mb_ro00 <- lm(enrol ~ nsib + age + male + nserv
  + factor(birthorder) # turns it negative
  + nlat + nupw 
  + motheragefirstchild + spousagegap # weakens slightly 
  + fatherreligion + motherreligion # weakens somewhat more
  + fatheroccupation + motheroccupation # and a little extra
  - hhid
  , data=ro00)
mg_ro00 <- lm(enrol ~ nbro + nsis + age + male + nserv
  + factor(birthorder) # turns it negative
  + nlat + nupw 
  + motheragefirstchild + spousagegap # weakens slightly 
  + fatherreligion + motherreligion # weakens somewhat more
  + fatheroccupation + motheroccupation # and a little extra
  - hhid
  , data=ro00)
mi_ro00 <- ivreg(enrol ~ nsib + age + male + nserv
  + factor(birthorder) # turns it negative
  + nlat + nupw 
  + motheragefirstchild + spousagegap # weakens slightly 
  + fatherreligion + motherreligion # weakens somewhat more
  + fatheroccupation + motheroccupation # and a little extra
  - hhid
  , data=ro00)
ml_ro00 <- glm(enrol ~ nsib + age + male + nserv
  + factor(birthorder) # turns it negative
  + nlat + nupw 
  + motheragefirstchild + spousagegap # weakens slightly 
  + fatherreligion + motherreligion # weakens somewhat more
  + fatheroccupation + motheroccupation # and a little extra
  - hhid
  , data=ro00, family=binomial(link='logit'))
models <- reglist(list(mb_ro00, mg_ro00, mi_ro00, ml_ro00))
texreg(models$models, override.se=models$se, override.pval=models$pv,
  caption=paste0(unique(ro00$smpl), ', enrolment'), 
  custom.note=paste0(note),
  custom.model.names=modnames,
  file='tabfigs/ro00_enrol.tex',
  omit.coef='occup|region|race', 
  stars=c(0.1, 0.05, 0.01), digits=3)

# # sweden 1890
# #------------
# sw90$fatheroccupation <- substring(sw90$fatherocchisco, 1, 1)
# sw90$motheroccupation <- substring(sw90$motherocchisco, 1, 1)
# sw90$fatheroccupation[sw90$fatheroccupation <= 2] <- 1
# sw90$motheroccupation[sw90$motheroccupation <= 6] <- 1
# sw90$motheroccupation[sw90$motheroccupation >= 7] <- 7
# # ftable(sw90$fatheroccupation, sw90$enrol)
# # ftable(sw90$motheroccupation, sw90$enrol)

# mb_sw90 <- lm(enrol ~ nsib + age + male + nserv + urban
#   + factor(birthorder)
#   + nlat + nupw 
#   + motheragefirstchild + spousagegap 
#   + fatheroccupation + motheroccupation
#   + factor(region)
#   - hhid
#   , data=sw90)
# mg_sw90 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban
#   + factor(birthorder)
#   + nlat + nupw 
#   + motheragefirstchild + spousagegap 
#   + fatheroccupation + motheroccupation
#   + factor(region)
#   - hhid
#   , data=sw90)
# mi_sw90 <- ivreg(enrol ~ nsib + age + male + nserv + urban
#   + factor(birthorder)
#   + nlat + nupw 
#   + motheragefirstchild + spousagegap 
#   + fatheroccupation + motheroccupation
#   + factor(region)
#   - hhid | . - nsib + hhtwin
#   , data=sw90)
# ml_sw90 <- glm(enrol ~ nsib + age + male + nserv + urban
#   + factor(birthorder)
#   + nlat + nupw 
#   + motheragefirstchild + spousagegap 
#   + fatheroccupation + motheroccupation
#   + factor(region)
#   - hhid
#   , data=sw90, family=binomial(link='logit'))
# models <- reglist(list(mb_sw90, mi_sw90, mg_sw90, ml_sw90))
# # models <- reglist(list(ml))
# texreg(models$models, override.se=models$se, override.pval=models$pv,
#   caption=paste0(gsub('[-_]', ' ', unique(sw90$smpl)), ', enrolment'), 
#   custom.note=paste0(note),
#   custom.model.names=modnames,
#   file='tabfigs/sw90_enrol.tex',
#   omit.coef='occup|region|race', 
#   stars=c(0.1, 0.05, 0.01), digits=3)


# # Sweden 1900
# #------------
# sw00$fatheroccupation <- substring(sw00$fatherocchisco, 1, 1)
# sw00$motheroccupation <- substring(sw00$motherocchisco, 1, 1)
# # ftable(x$fatheroccupation, x$enrol)
# # ftable(x$motheroccupation, x$enrol)
# # x$fatheroccupation[x$fatheroccupation <= 6] <- 1
# # x$fatheroccupation[x$fatheroccupation >= 7] <- 7
# # x$motheroccupation[x$motheroccupation <= 6] <- 1
# # x$motheroccupation[x$motheroccupation >= 7] <- 7
# # # ftable(x$region, x$enrol)
# # # ftable(x$fatherreligion, x$enrol)
# # # ftable(x$motherreligion, x$enrol)
# # # ftable(x$birthorder, x$enrol)

# mb_sw00 <- lm(enrol ~ nsib + age + male + nserv + urban
#   + factor(birthorder) 
#   + nupw
#   + motheragefirstchild + spousagegap 
#   + fatherreligion # + motherreligion
#   + factor(fatheroccupation) + factor(motheroccupation)
#   + factor(region)
#   - hhid
#   , data=sw00)
# mg_sw00 <- lm(enrol ~ nbro + nsis + age + male + nserv + urban
#   + factor(birthorder) 
#   + nupw
#   + motheragefirstchild + spousagegap 
#   + fatherreligion # + motherreligion
#   + factor(fatheroccupation) + factor(motheroccupation)
#   + factor(region)
#   - hhid
#   , data=sw00)
# mi_sw00 <- ivreg(enrol ~ nsib + age + male + nserv + urban
#   + factor(birthorder) 
#   + nupw
#   + motheragefirstchild + spousagegap 
#   + fatherreligion # + motherreligion
#   + factor(fatheroccupation) + factor(motheroccupation)
#   + factor(region)
#   - hhid | . - nsib + hhtwin
#   , data=sw00)
# ml_sw00 <- glm(enrol ~ nsib + age + male + nserv + urban
#   + factor(birthorder) # makes negative, but insig
#   + nupw # strengthens, nlat aliased
#   + motheragefirstchild + spousagegap # makes positive again
#   + fatherreligion # + motherreligion
#   + factor(fatheroccupation) + factor(motheroccupation)
#   + factor(region)
#   - hhid
#   , data=sw00, family=binomial(link='logit'))
# models <- reglist(list(mb_sw00, mg_sw00, mi_sw00, ml_sw00))
# texreg(models$models, override.se=models$se, override.pval=models$pv,
#   caption=paste0(gsub('[-_]', ' ', unique(us50$smpl)), ', enrolment'), 
#   custom.note=paste0(note),
#   custom.model.names=modnames,
#   file='tabfigs/sw00_enrol.tex',
#   omit.coef='occup|region|race', 
#   stars=c(0.1, 0.05, 0.01), digits=3)
