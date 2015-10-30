rm(list=ls())
# mosaic datasets
# Hungary 1869
setwd('~/downloads/data/qqt')
source('~/dropbox/viennapaper/scripts/qqtheader.r')

library(bit64)
library(data.table)
library(foreign)
library(stringdist)
library(lmtest)
library(texreg)

########################
### general datasets ###
########################
edv <- read.csv('~/dropbox/viennapaper/data/comped.csv', stringsAsFactors=FALSE)
ec <- read.csv('~/dropbox/viennapaper/data/europeancities.csv', stringsAsFactors=FALSE)

vrbs <- c('smpl', 'year', 'iso3', 'age', 'child', 'schoolage', 'occupation', 'enrol', 
  'literate', 'male', 'female', 'married', 'relate', 'son', 'daughter', 
  'lateral', 'upward', 'ext', 'servant', 'head', 'spouse', 'religion', 
  'ethnic', 'race', 'hhid', 'persid', 'urban', 'region', 'occhisco')

#######################
### mosaic datasets ###
#######################
# bulgaria 1900 has no occupations
# french villages have too few student occupations
# st emilion has very few educ obs.
# st jean de luz has very few useful occs or lit
# germany 1846 ids only 10 students
# jasenica 1863 ids only 6 students
# zurich 1870 gives few useful obs
# kujavia=poland has no occ, no lit
# rostock 1819 has too few students
# schleswig-holstein has a handful of students
# hirzel too many missing obs in literacy
# colomera does not work
# german villages: all have very few students; one hohscheid has a few, but are all apprentices
# Canada 1911- messy data

#-------------
# hungary 1869
#-------------
hu <- fread('mosaic/hungary_1869/Datafile_152 Version 1.0_labels.csv',
  sep=';', data.table=F, colClasses='character') 

hu$smpl <- 'mos-hungary-1869'
hu$iso3 <- 'HUN'
hu$age[hu$age=='less than 1 year old'] <- 0
hu$age <- as.numeric(hu$age)

hu$child <- hu$age < 16
hu$schoolage <- hu$age > 5 & hu$age < edv$edageendlowerbound[edv$iso3 %in% hu$iso3 & edv$year %in% hu$year]

hu$occupat <- iconv(hu$occupat, 'utf8', 'utf8', sub='_') # no correct encoding available, _ removes inv. multibyate stings
hu$occupation <- tolower(hu$occupat)
hu$occhisco <- NA

# group_strings(hu$occupation[hu$child])
schlrgx <- 'tan|kol|\\Winas|^inas|növ|gya'
hu$enrol <- grepl(schlrgx, hu$occupation) & hu$child

hu$literate <- grepl('read|^liter', hu$lit)
hu$literate[hu$lit=='unknown'] <- NA

hu$male <- hu$sex=='male'
hu$female <- hu$sex=='female'
hu$married <- grepl('^married|widow|separate|divorced', hu$marst)

hu$relate <- tolower(hu$relate)
hu$son <- hu$relate %in% 'child' & hu$male
hu$daughter <- hu$relate %in% 'child' & hu$female
hu$lateral <- grepl('sibling|nephew|niece|aunt|uncle|cousin', hu$relate)
hu$upward <- grepl('parent', hu$relate)
hu$ext <- hu$lateral | hu$upward
hu$servant <- grepl('servant|maid|cook', hu$relate)
hu$head <- hu$relate %in% 'head/householder'
hu$spouse <- hu$relate %in% c('spouse', 'unmarried partner')

hu$religion[grepl('Catholic', hu$relig)] <- 'catholic'
hu$religion[grepl('Luther|Unit|Reform', hu$relig)] <- 'protestant'
hu$religion[grepl('Orthodox', hu$relig)] <- 'orthodox'
hu$religion[grepl('Jewish', hu$relig)] <- 'jewish'

hu$ethnic <- NA
hu$race <- NA

hu$urban <- hu$urban=='urban'
hu$region <- hu$region

hu$hhid <- hu$id_hhold
hu$persid <- hu$id_pers

hu <- hu[hu$gq %in% c('Household with 0-4 persons unrelated to the head', 
                      'Household with 5-9 persons unrelated to the head'), ]
hu <- rm_dualhh(hu)

write.csv(hu[vrbs], paste0('cleandata/', unique(hu$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(hu), extravrbs='literate')

x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]
ss <- calcsumstats(hu, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', row.names=F)

rm(hu)
rm(x)

#-------------
# rostock 1867
#-------------
ro <- read.spss('mosaic/_Rostock_1867_v0.1/Rostock 1867.sav')
ro <- data.frame(ro)

ro$smpl <- 'mos-rostock-1867'
ro$iso3 <- 'DEU'

ro$firstname <- gsub('\\s+$', '', ro$firstname)
ro$lastname <- gsub('\\s+$', '', ro$lastname)
ro$occupation <- gsub('\\s+$', '', ro$occupation)
ro$relationship <- gsub('\\s+$', '', ro$relationship)

ro$age[ro$age > 800] <- NA
ro$child <- ro$age < 16
ro$schoolage <- ro$age > 5 & ro$age < edv$edageendlowerbound[edv$iso3 %in% ro$iso3 & edv$year %in% ro$year]

ro$occupation <- tolower(ro$occupation)
ro$occhisco <- NA

# group_strings(ro$occupation[ro$child])
schlrgx <- 'sch.+l|leh?r|g[yÿiuü]m|burs|element'
ro$enrol <- grepl(schlrgx, ro$occupation) & ro$child
ro$literate <- NA

ro$male <- ro$sex=='male'
ro$female <- ro$sex=='female'
ro$married <- grepl('^married|divorced|widowed', ro$MarrStatus)

ro$relate <- ro$relationship_coded

ro$son <- ro$relate %in% 'child' & ro$male
ro$daughter <- ro$relate %in% 'child' & ro$female
ro$lateral <- ro$relate %in%'sibling and other lateral kin'
ro$upward <- ro$relate %in% 'parent'
ro$ext <- ro$lateral | ro$upward
ro$servant <- ro$relate %in% 'servant'
ro$head <- ro$relate %in% 'household head'
ro$spouse <- ro$relate %in% 'spouse'

ro$religion <- NA
ro$ethnic <- NA
ro$race <- NA

ro$hhid <-ro$householdno
ro$persid <- ro$person_id
ro$urban <- TRUE
ro$region <- 'rostock'

ro$gq <- makegq(ro)
ro <- ro[ro$gq < 5 & !is.na(ro$gq), ]
ro <- rm_dualhh(ro)

write.csv(ro[vrbs], paste0('cleandata/', unique(ro$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(ro))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ro$religion <- 'unknown'
ss <- calcsumstats(ro, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv',
  sep=',', append=T, col.names=F, row.names=F)
rm(ro)
rm(x)

# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$motheroccupation <- Uniformize(x$motheroccupation)
# x$birthorder[x$birthorder > 5] <- '> 5'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatheroccupation + motheroccupation

#---------------------
### san marcello, 1827
#---------------------
sa <- read.spss('mosaic/_San_Marcello_1827_v0.1/San Marcello 1827.sav',
  to.data.frame=TRUE)
names(sa) <- tolower(names(sa))
sa <- factor2char(sa)
sa$smpl <- 'mos-san_marcello-1827'
sa$year <- 1827
sa$iso3 <- 'ITA'

sa$age <- sa$alter
sa$child <- sa$age < 16
sa$schoolage <- sa$age > 5 & sa$age < edv$edageendlowerbound[edv$iso3 %in% sa$iso3 & edv$year %in% sa$year]

sa$occupation <- tolower(sa$berufs)
sa$occhisco <- NA

# group_strings(sa$occupation[sa$child])
sa$enrol <- (grepl('ausbildung', sa$occupation) | sa$berstel=='Lehrling/Lehrmaedche') | sa$beruf %in% c('IM COLLEGIO', 'IM SEMINAR', 'STUDENT') 
sa$enrol <- sa$enrol & sa$child
# sa$enrol[sa$occupation=='unbekannt'] <- NA
sa$literate <- NA

sa$male <- grepl('^m', sa$sexfama)
sa$female <- grepl('^f', sa$sexfama)
sa$married <- grepl('verhei|getrennt|verwit', sa$sexfama)

sa$relate <- sa$stellung
sa$son <- sa$stellung %in% c('SOHN', 'KIND') & sa$male
sa$daughter <- sa$stellung %in% c('TOCHTER', 'KIND') & sa$female
sa$lateral <- NA
sa$upward <- NA
sa$ext <- sa$stellung %in% 'VERWANDT'
sa$servant <- sa$stellung %in% 'GESINDE'
sa$head <- sa$stellung %in% 'HVORSTAND'
sa$spouse <- sa$stellung %in% 'HAUSFRAU'

sa$religion <- ifelse(sa$hvkonfes=='evangelisch' & !is.na(sa$hvkonfes), 'protestant', 'catholic')
sa$ethnic <- tolower(sa$hvherkod)
sa$race <- NA

sa$hhid <- as.numeric(as.factor(paste(sa$strasse, sa$wohnung, sa$hausnr)))
sa$persid <- sa$fallnr
sa$urban <- TRUE # Roman parish
sa$region <- 'Rome'

sa$gq <- makegq(sa)
sa <- sa[sa$gq < 5 & !is.na(sa$gq), ]
sa <- rm_dualhh(sa)

write.csv(sa[c(vrbs, 'berstel')], paste0('cleandata/', unique(sa$smpl), '.csv'), row.names=F)

sa$lateral <- sa$upward <- sa$ext
x <- hhaggregate(data.table(sa), extravrbs='berstel')
x$nextd <- x$nlat
x$nlat <- x$nupw <- NA
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(sa, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nextd - hhid + 
#   fatherberstel
rm(sa)
rm(x)

#-----------
# kruja 1918
#-----------
kr <- read.spss('mosaic/_Kruja_1918_v0.1/Kruja 1918.sav',
  to.data.frame=TRUE)
kr <- factor2char(kr)
kr$smpl <- 'mos-kruja-1918'
kr$year <- 1918
kr$iso3 <- 'ALB'

kr$firstnam <- gsub('\\s+$', '', kr$firstnam)
kr$lastname <- gsub('\\s+$', '', kr$lastname)
kr$sex <- gsub('\\s+$', '', kr$sex)
kr$occupat <- gsub('\\s+$', '', kr$occupat)

kr$age[kr$age > 900] <- NA
kr$child <- kr$age < 16
kr$schoolage <- kr$age > 5 & kr$age < edv$edageendlowerbound[edv$iso3 %in% kr$iso3 & edv$year %in% kr$year]

kr$occupation <- tolower(kr$occupat)
kr$occhisco <- NA

# group_strings(kr$occupation[kr$child])
schlrgx <- 'lehr|shk|schü'
kr$enrol <- grepl(schlrgx, kr$occupation) & kr$child
kr$literate <- ifelse(kr$turk=='no' & kr$latin=='no', FALSE, TRUE)

kr$male <- kr$sex=='male'
kr$female <- kr$sex=='female'
kr$married <- grepl('^married|widow', kr$mst)

kr$relate <- kr$rhh

kr$son <- kr$relate %in% 'son'
kr$daughter <- kr$relate %in% 'daughter'
kr$lateral <- grepl('brother|sister|cousin|nephew|niece|aunt|uncle', kr$rhh)
kr$upward <- grepl('mother|father', kr$rhh)
kr$ext <- kr$lateral | kr$upward
kr$servant <- grepl("servant$|maid", kr$relate)
kr$head <- grepl('male household head', kr$relate) # includes 42 female hhh
kr$spouse <- kr$relate %in% c('wife', '2nd wife')

kr$religion[grepl('Catholic', kr$confessi)] <- 'catholic'
kr$religion[grepl('Orthodox', kr$confessi)] <- 'orthodox'
kr$religion[grepl('Muslim', kr$confessi)] <- 'muslim'
kr$ethnic <- NA
kr$ethnic[grepl('Albanian', kr$ethnicit)] <- 'albanian'
kr$ethnic[grepl('ypsy', kr$ethnicit)] <- 'gypsy'
kr$ethnic[grepl('Arab', kr$ethnicit)] <- 'arab'
kr$ethnic[grepl('erbo', kr$ethnicit)] <- 'serbo-croat'
kr$ethnic[grepl('Vlach', kr$ethnicit)] <- 'vlach'
kr$race <- NA

kr$hhid <- kr$id_hh
kr$persid <- kr$id_person
kr$urban <- kr$place_re %in% 'O Kruja' # 3000+ inh, not rly enough
kr$region <- kr$place_re

kr$gq <- makegq(kr)
kr <- kr[kr$gq < 5 & !is.na(kr$gq), ]
kr <- rm_dualhh(kr)

write.csv(kr[vrbs], paste0('cleandata/', unique(kr$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(kr), extravrbs=c('literate', 'ethnic'))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(kr, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation[grepl('bulk|bauer|grund$', x$fatheroccupation)] <- 'Farmer'
# x$fatheroccupation[grepl('rbeiter', x$fatheroccupation)] <- 'Labourer'
# x$fatheroccupation[lowfreql(x$fatheroccupation, freq=80)] <- 'Other'
# x$fatheroccupation[x$fatheroccupation=='*'] <- NA
# x$birthorder[x$birthorder > 4] <- '> 4'
# ftable(x$enrol, x$fatherethnic)
# ftable(x$enrol, x$fatherreligion)
# ftable(x$enrol, x$fatheroccupation) # perfect predictors, not in case of lit
# f <- enrol ~ nsib + age + male + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid
rm(kr)
rm(x)

#-------------
# rostock 1900
#-------------
ro <- fread('mosaic/rostock 1900 germany/Datafile_06 Version 1.0_labels.csv',
  sep=';', data.table=F, colClasses='character')
ro_numeric <- fread('mosaic/rostock 1900 germany/Datafile_06 Version 1.0_codes.csv',
  sep=';', data.table=F, colClasses='character')
ro$occhisco <- ro_numeric$occhisco
rm(ro_numeric)
ro$smpl <- 'mos-rostock-1900'
ro$iso3 <- 'DEU'

ro$age[ro$age=='less than 1 year old'] <- 0
ro$age <- as.numeric(ro$age)
ro$child <- ro$age < 16
ro$schoolage <- ro$age > 5 & ro$age < edv$edageendlowerbound[edv$iso3 %in% ro$iso3 & edv$year %in% ro$year]

ro$occupation <- tolower(ro$occupat)

# group_strings(ro$occupation[ro$age])
schlrgx <- 'sch[u|ü]|lehr|gym'
ro$enrol <- grepl(schlrgx, ro$occupation) & ro$child
ro$literate <- NA

ro$male <- ro$sex %in% 'male'
ro$female <- ro$sex %in% 'female'
ro$married <- grepl('^married|divorced|widow|separated', ro$marst)

ro$son <- ro$relate %in% 'child' & ro$male
ro$daughter <- ro$relate %in% 'child' & ro$female
ro$lateral <- grepl('sibling|niece|nephew|cousin|aunt|uncle', ro$relate)
ro$upward <- grepl('parent', ro$relate)
ro$ext <- ro$lateral | ro$upward
ro$servant <- grepl('ser|cook|housek|nurse', ro$relate)
ro$head <- ro$relate %in% 'head/householder'
ro$spouse <- ro$relate %in% 'spouse'

ro$religion[grepl('Luther|Angli|other Chris|Pent|Prot|Ref', ro$relig)] <- 'protestant'
ro$religion[grepl('Catholic', ro$relig)] <- 'catholic'
ro$religion[grepl('Orthodox', ro$relig)] <- 'orthodox'
ro$religion[grepl('Jewish', ro$relig)] <- 'jewish'
ro$religion[grepl('Buddhist|no religion|unknown', ro$relig)] <- 'other'

ro$ethnic <- NA
ro$race <- NA
ro$hhid <- ro$id_hhold
ro$persid <- ro$id_pers
ro$urban <- ro$urban %in% 'urban'
ro$region <- 'rostock'

ro <- ro[ro$gq %in% c('household with 0-4 persons unrelated to the head', 
                      'household with 5-9 persons unrelated to the head'), ]
ro <- rm_dualhh(ro)

write.csv(ro[vrbs], paste0('cleandata/', unique(ro$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(ro), extravrbs='occhisco')
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(ro, x, 'hhid')

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# ftable(x$fatherreligion, x$enrol)
# ftable(x$motherreligion, x$enrol)
# x$fatheroccupation <- substr(x$fatherocchisco, 1, 1)
# x$motheroccupation <- substr(x$motherocchisco, 1, 1)
# x$fatherreligion[grepl('orthodox|other|jewish', x$fatherreligion)] <- 'other'
# x$motherreligion[grepl('orthodox|other|jewish', x$motherreligion)] <- 'other'
# x$birthorder[x$birthorder > 6] <- '> 6'
# f <- enrol ~ nsib + age + male + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatherreligion + motherreligion + 
#   factor(fatheroccupation)
rm(ro)
rm(x)

#--------------
# Jasenica 1884 
#--------------
ja <- fread('mosaic/Jasenica 1884 serbia/Datafile_148 Version 1.1_labels.csv', 
  sep=';', colClasses='character', header=T, data.table=F)
# colClasses=char because id_pers > 2e9, exceeds max integer value
ja_num <- fread('mosaic/Jasenica 1884 serbia/Datafile_148 Version 1.1_codes.csv', 
  sep=';', colClasses='character', header=T, data.table=F)
ja$occhisco <- ja_num$occhisco
rm(ja_num)
ja$smpl <- 'mos-jasenica-1884'
ja$iso3 <- 'SRB'

ja$age[ja$age=='less than 1 year old'] <- 0
ja$age <- as.numeric(ja$age)
ja$child <- ja$age < 16
ja$schoolage <- ja$age > 5 & ja$age < edv$edageendlowerbound[edv$iso3 %in% ja$iso3 & edv$year==1880]

ja$occupation <- tolower(ja$occupat)

# group_strings(ja$occupation[ja$child])
ja$enrol <- ja$occupation %in% 'djak' & ja$child
ja$literate <- ja$lit %in% 'literate'

ja$male <- ja$sex=='male'
ja$female <- ja$sex=='female'
ja$married <- grepl('^married|widow|separated', ja$marst)

ja$son <- grepl('^child$', ja$relate) & ja$male
ja$daughter <- grepl('child$', ja$relate) & ja$female
ja$lateral <- grepl('sibling|niece|nephew|cousin|aunt|uncle', ja$relate)
ja$upward <- grepl('parent', ja$relate)
ja$ext <- ja$lateral | ja$upward
ja$servant <- grepl('ser|cook|housek|nurse', ja$relate)
ja$head <- ja$relate %in% 'head/householder'
ja$spouse <- ja$relate %in% 'spouse'

ja$religion <- 'unknown'
ja$ethnic <- NA
ja$race <- NA
ja$hhid <- ja$id_hhold
ja$persid <- ja$id_pers
ja$urban <- ja$urban %in% 'urban'
ja$region <- 'jasenica'

ja <- ja[ja$gq %in% c("household with 0-4 persons unrelated to the head",
                      "household with 5-9 persons unrelated to the head"), ]
ja <- rm_dualhh(ja)

write.csv(ja[vrbs], paste0('cleandata/', unique(ja$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(ja), extravrbs=c('literate', 'occhisco'))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(ja, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# ftable(x$enrol, x$birthorder)
# ftable(x$enrol, x$motherliterate)
# # x$fatheroccupation <- substring(x$fatherocchisco, 1, 1)
# # x$motheroccupation <- substring(x$motherocchisco, 1, 1)
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$motheroccupation <- Uniformize(x$motheroccupation)
# f <- enrol ~ nsib + age + male + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatherliterate + motherliterate  + 
#   fatheroccupation
rm(ja)
rm(x)

###################
### zurich 1870 ###
###################
zu <- read.spss('mosaic/_Zuerich_1870_v0.1/ZÅrich 1870.sav', 
  to.data.frame=TRUE)
zu <- factor2char(zu)
names(zu) <- tolower(names(zu))

zu$smpl <- 'mos-zurich-1870'
zu$year <- 1870
zu$iso3 <- 'CHE'

zu$age <- zu$alter - 1000
zu$child <- zu$age < 16
zu$schoolage <- zu$age > 5 & zu$age < edv$edageendlowerbound[edv$iso3 %in% zu$iso3 & edv$year %in% zu$year]

zu$occupation <- tolower(zu$beruf)
zu$occhisco <- NA

enrolled <- grepl('stud|schu|gym|lehr', zu$occupation)
zu$occupation <- tolower(zu$berufs)
zu$enrol <- (grepl('ausbildung', zu$occupation) | enrolled) & zu$child
# zu$enrol[is.na(zu$beruf)] <- NA
# few enrolled; NAs setting NA or TRUE changes little
zu$literate <- NA

zu$male <- grepl('^m', zu$sexfama)
zu$female <- grepl('^f', zu$sexfama)
zu$married <- grepl('verheiratet|verwitwet', zu$sexfama)

zu$relate <- tolower(zu$stellung)

zu$son <- zu$relate %in% 'sohn'
zu$daughter <- zu$relate %in% 'tochter'
zu$ext <- zu$relate %in% 'verwandt'
zu$lateral <- zu$upward <- NA
zu$servant <- zu$relate %in% 'gesinde'
zu$head <- zu$relate %in% 'hvorstand'
zu$spouse <- zu$relate %in% 'hausfrau'

zu$religion[zu$klauselc==11] <- 'catholic'
zu$religion[zu$klauselc==12] <- 'protestant'
zu$religion[zu$klauselc==13] <- 'jewish'
zu$religion[zu$klauselc==15] <- 'orthodox'
zu$religion[zu$klauselc==14] <- 'catholic' # assuming griech uniert is greek catholic
zu$religion[zu$klauselc==0] <- 'other'

# zu$herkod <- gsub('\\d', '', zu$herkod)
# regs <- unique(tolower(zu$herkod))
# regsg <- lapply(regs, geocode, reg='de')
# regsd <- do.call(rbind, regsg)
# regsd$ctr <- sapply(regsd$loc_frmtd, function(x) regexprr('[A-z ]+$', x))
# regsd$ctr <- gsub('^\\s', '', regsd$ctr)
# regsd$ctr[is.na(regsd$ctr)] <- 'Germany'
# regsd$ctr[regsd$ctr=='Namibia'] <- 'Germany' # schl.-holtst in 1870 
# regsd$ctr[regsd$ctr=='The Bahamas'] <- 'Germany' # nassau

# zu$ethnic <- regsd$ctr[match(tolower(zu$herkod), regsd$loc)]
zu$ethnic <- tolower(zu$herkod)
zu$race <- NA
zu$hhid <- as.numeric(as.factor(paste(zu$strasse, zu$wohnung, zu$hausnr)))
zu$persid <- zu$fallnr
zu$urban <- TRUE
zu$region <- 'zurich'

zu$gq <- makegq(zu)
zu <- zu[zu$gq < 5 & !is.na(zu$gq), ]
zu <- rm_dualhh(zu)

write.csv(zu[vrbs], paste0('cleandata/', unique(zu$smpl), '.csv'), row.names=F)

zu$lateral <- zu$upward <- zu$ext
x <- hhaggregate(data.table(zu), extravrbs='ethnic')
zu$lateral <- zu$upward <- NA
x$nlat <- x$nupw <- NA
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(zu, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# ftable(x$fatherreligion, x$enrol)
# ftable(x$motherreligion, x$enrol)
# ftable(x$birthorder, x$enrol)
# x$fatherreligion[grepl('catho|jew|other', x$fatherreligion)] <- 'other'
# x$motherreligion[grepl('catho|jew|other', x$motherreligion)] <- 'other'
# f <- enrol ~ nsib + age  + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nextd - hhid + 
#   fatherreligion + 
#   fatheroccupation + motheroccupation
# didn't get it to estimate in earlier file, estimates unreliable
rm(zu)
rm(x)

#------------------
# Austria 1632-1947
#------------------
au <- read.spss('mosaic/_Austrian_Numeric_Datafile_v0.1/Austrian numeric data.sav',
  to.data.frame=TRUE)
au <- factor2char(au)

au$smpl <- 'mos-austria-1632_1947'
au$year <- au$erjahr
au$iso3 <- 'AUT'

au$age <- au$alter # gives awful lot of people aged 99...
au$age[au$age > 900] <- NA

au$child <- au$age < 16
au$schoolage <- NA
au$schoolage[au$year < 1774] <- 5
au$schoolage[au$year >= 1774] <- 12
au$schoolage[au$year >= 1869] <- 13
au$schoolage <- au$age > 5 & au$age < au$schoolage

au$occupation <- tolower(au$berufs) # careful: berufs/occ says nothing about pupil status
au$occhisco <- NA

# group_strings(au$berufs[au$child])
# group_strings(au$berstel[au$child])
au$enrol <- grepl('LEHRLING|GESELLE', au$berstel) & au$child
au$literate <- NA

au$male <- grepl('MAENN', au$sex)
au$female <- grepl('WEIB', au$sex)
au$married <- au$fam %in% c(2, 3, 4) # maybe set 9 & 5 to NA

au$relate <- au$alstel

au$son <- au$alstel %in% 3
au$daughter <- au$alstel %in% 4
au$ext <- au$alstel %in% 5
au$lateral <- NA
au$upward <- NA
au$servant <- au$alstel %in% 7
au$head <- au$alstel %in% 1
au$spouse <- au$alstel %in% 2

au$religion[au$konfess %in% 1] <- 'catholic'
au$religion[au$konfess %in% 2] <- 'protestant'
au$religion[au$konfess %in% 3] <- 'jewish'
au$religion[au$konfess %in% 4] <- 'orthodox'

au$ethnic <- tolower(au$herkss)
# lapply(unique(au$ethnic), geocode)
au$race <- NA
au$hhid <- as.numeric(as.factor(paste(au$gmde, au$erjahr, au$hausnr, au$wohnnr)))
au$persid <- 1:nrow(au)

austria_hungary <- ec[ec$Code %in% c(40, 276), ]
urbs <- c(austria_hungary$City[ain(austria_hungary$City, unique(au$gmd))],
          "Herrengasse", "Gaudenzdorf", "Ringturm", # these are neighbourhoods in Vienna
          "Schottenfeld", "Leopoldsdorf","Gumpendorf",
          "Kaiserebersdorf", "Josefstadt",  "Neubau")
au$urban <- au$gmd %in% urbs
au$region <- au$gmd

au$gq <- makegq(au)
au <- au[au$gq < 5 & !is.na(au$gq), ]
au <- rm_dualhh(au)

write.csv(au[vrbs], paste0('cleandata/', unique(au$smpl), '.csv'), row.names=F)

au$lateral <- au$upward <- au$ext
x <- hhaggregate(data.table(au))
au$lateral <- au$upward <- au$lateral <- NA
x$nlat <- x$nupw <- NA
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(au, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# ftable(x$enrol, x$motherreligion)
# ftable(x$enrol, x$motheroccupation)
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$motheroccupation <- Uniformize(x$motheroccupation)
# x$motheroccupation[lowfreql(x$motheroccupation, 13)] <- 'Other'
# x$fatheroccupation[lowfreql(x$fatheroccupation, 50)] <- 'Other'
# x$birthorder[x$birthorder > 5] <- '> 5' 
# x$region[x$region %in% c("Herrengasse", "Gaudenzdorf", "Ringturm",
#           "Schottenfeld", "Leopoldsdorf","Gumpendorf",
#           "Kaiserebersdorf", "Josefstadt",  "Neubau")] <- 'Vienna'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nextd - hhid + factor(year) + 
#   fatheroccupation + motheroccupation
# negative but insignificant, interesting to get year/occup dummies right
rm(au)
rm(x)

#-------------
# austria 1910
#-------------
au <- fread('mosaic/austria_1910/Datafile_219 Version 1.0_labels.csv',
  colClasses='character', sep=';', data.table=F)
au$smpl <- 'mos-austria-1910'
au$iso3 <- 'AUT'

au$age[au$age=='less than 1 year old'] <- 0
au$age <- as.numeric(au$age)

au$child <- au$age < 16
au$schoolage <- au$age > 5 & au$age < edv$edageendlowerbound[edv$iso3 %in% au$iso3 & edv$year %in% au$year]

au$occupation <- tolower(au$occupat)
au$occhisco <- NA # not coded yet

# group_strings(au$occupation[au$child])
au$enrol <- grepl('lerl|chul|chül|stud|gym|lehr|lerj', au$occupation) & au$child
au$enrol[grepl('schulunfähig', au$occupation)] <- FALSE
au$literate <- !grepl('illiterate|unknown', au$lit)

au$male <- au$sex %in% 'male'
au$female <- au$sex %in% 'female'
au$married <- grepl('^married|div|separ|widow', au$marst)

au$son <- au$relate %in% 'child' & au$male
au$daughter <- au$relate %in% 'child' & au$female
au$lateral <- grepl('nephew|niece|sibling|aunt|uncle|cousin', au$relate)
au$upward <- grepl('parent', au$relate)
au$ext <- au$lateral | au$upward
au$servant <- grepl('ser|cook|housek|nurse', au$relate)
au$head <- au$relate %in% 'head/householder'
au$spouse <- au$relate %in% 'spouse'

au$religion[grepl('Protest|Reform|Luther', au$relig)] <- 'protestant'
au$religion[grepl('Catholic', au$relig)] <- 'catholic'
au$religion[grepl('Orthodox', au$relig)] <- 'orthodox'
au$religion[grepl('Jewish', au$relig)] <- 'jewish'
au$religion[grepl('no relig|unknown', au$relig)] <- 'other'

au$ethnic <- NA
au$race <- NA
au$hhid <- au$id_hhold
au$persid <- au$id_pers
au$urban <- au$urban %in% 'urban'

au <- au[au$gq %in% c("Household with 0-4 persons unrelated to the head",
                      "Household with 5-9 persons unrelated to the head") ,]
au <- rm_dualhh(au)

write.csv(au[vrbs], paste0('cleandata/', unique(au$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(au), extravrbs='literate')
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(au, x, 'hhid', sample=nam)
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# ftable(x$enrol, x$birthorder)
# ftable(x$enrol, x$motherreligion)
# ftable(x$enrol, x$fatherreligion)
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$fatheroccupation[lowfreql(x$fatheroccupation, 3)] <- 'Other'
# x$motheroccupation <- Uniformize(x$motheroccupation)
# sort(table(x$motheroccupation))
# x$birthorder[x$birthorder > 5] <- '> 5'
# x$fatherreligion[grepl('jew|prot', x$fatherreligion)] <- 'other'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatherliterate + motherliterate  + 
#   fatherreligion + # motherreligion + 
#   fatheroccupation + motheroccupation + region
rm(au)
rm(x)

####################
### irish census ###
####################
#-------------
# ireland 1911
#-------------
ir <- fread('ireland19115pc.csv', 
  sep=',', colClasses='character', data.table=F)
setnames(ir, gsub(' ', '.', names(ir)))
ir <- ir[!duplicated(ir), ] # 99% of observations are duplicates
ir$smpl <- 'archie-ireland-1911'
ir$year <- 1911
ir$iso3 <- 'IRL'

ir$Age[ir$Age==''] <- 0
ir$age <- as.numeric(ir$Age)
ir$child <- ir$age < 16
ir$schoolage <- ir$age > 5 & ir$age < edv$edageendlowerbound[edv$iso3 %in% ir$iso3 & edv$year %in% ir$year]
# NB edv's compulsory ages differ from Fernihough's

ir$occupation <- tolower(ir$Occupation)
ir$occhisco <- NA

# group_strings(ir$occupation[ir$child])
ir$enrol <- grepl('cho|sco|shol|schl|pupil|appr|instruc|stud', ir$occupation) & ir$child
ir$enrol[grepl('no|moni|l assis', ir$occupation) & ir$child] <- FALSE

litrgx <- '^r|can [^n]|c r|can w|^rd|both|yes|^w|r read|^educ|engl|only r|instructed'
ir$literate <- grepl(litrgx, ir$Literacy, ignore.case=T)
ir$literate[grepl('Read None|nor', ir$Literacy)] <- FALSE

ir$male <- ir$Sex %in% 'Male'
ir$female <- ir$Sex %in% 'Female'
ir$male[ir$Sex=='-'] <- NA
ir$female[ir$Sex=='-'] <- NA

ir$married <- grepl('^Married|Wido|Wife|Yes', ir$Marital.Status)
ir$married[ir$Marital.Status=='-'] <- NA

ir$fid <- gsub('/pages/1911/', '', ir$fid)
ir$region <- vstrsplit(ir$fid, '/')[, 1]
ir$ded <- vstrsplit(ir$fid, '/')[, 2]
ir$street <- vstrsplit(ir$fid, '/')[, 3]
ir$hhid <- vstrsplit(ir$fid, '/')[, 4]
ir$persid <- 1:nrow(ir)

ir$nhhid <- as.numeric(ir$hhid) 
ids <- which(!diff(ir$nhhid)==0)
ir$newhhid <- 1:nrow(ir) %in% (ids + 1) # to check hhh

ir$relate <- tolower(ir$Relation.to.head)

sonrgx <- "^son$|^sons$|^son[^'].*(wife|head|illeg|husb)|step.*son|ad[oa]pt.*(son|child)|^child"
ir$son <- grepl(sonrgx, ir$relate)
dgtrgx <- "^daughter$|daughter[^'].*(wife|head|illeg|husb)|step.*daught|ad.*p.*(daug|child)|his d|^child"
ir$daughter <- grepl(dgtrgx, ir$relate)
ir$daughter[ir$relate=="Daughter and Wife to no 3"] <- FALSE
ir[ir$relate=='Childrens Governess', c('son', 'daughter')] <- FALSE
ir$lateral <-  grepl('niece|nephew|brother|sister|uncle|aunt', ir$relate)
ir$upward <- grepl('father|mother|parent', ir$relate)
ir$ext <- ir$lateral | ir$upward
srvrgx <- "coach|cook|serv.*nt|keep|help|maid|nurse|porter|stew|wait|butl|domestic|govern"
ir$servant <- grepl(srvrgx, ir$relate)
ir$head <- grepl('^head|(widow|husband) head|householder|son head', ir$relate)
ir$head[grepl('matron|head wife|head of family mother in law|head of son', ir$relate)] <- FALSE
ir$spouse <- grepl('^wife$|wife [^ot]|second wife|(house|mother|family|farmer) wife', ir$relate)

catrgx <- 'r.*(ca[tch]|chat)|rom|cat|roam|calith|chat|cac|^r c|^st'
prtrgx <- "ir.*|ang.*|eng.*|c.*of (i|e)|scot|pro|germ|pr(e|i)|pyres|me|un|ep|esp|ba|con|chris|wes|br.*t|i c|u f|friend|cov|sal|mora|c (i|e)|^l|clare|pil|apo|swe|adv|qua|wel|zwing"
jewrgx <- "jew|heb|isr|jen|jsea"
ir$religion[grepl(prtrgx, ir$Religion, ignore.case=TRUE)] <- 'protestant'
ir$religion[grepl(catrgx, ir$Religion, ignore.case=TRUE)] <- 'catholic'
ir$religion[grepl(jewrgx, ir$Religion, ignore.case=TRUE)] <- 'jewish'
ir$religion[is.na(ir$religion)] <- 'other'

ir$ethnic <- 'willfully ignored'
ir$race <- NA

deds <- table(paste0(ir$ded, ir$region))
urbandeds <- deds[deds > 200] # workaround: ded not straightfwd to match to list of urb locs
ir$urban <- paste0(ir$ded, ir$region) %in% names(urbandeds)
# geocode('Aghadown_SouthCork', reg='ir')
ir$region <- tolower(ir$region)

ir$gq <- makegq(ir)
ir <- ir[ir$gq < 5 & !is.na(ir$gq), ]
ir <- rm_dualhh(ir)

write.csv(ir[vrbs], paste0('cleandata/', unique(ir$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(ir), extravrbs='literate')
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(ir, x, 'hhid')
x <- x[x$age > 13 & !x$schoolage & x$age < 16 & !is.na(x$age), ]

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$fatheroccupation[lowfreql(x$fatheroccupation, freq=5)] <- 'other'
# x$motheroccupation <- Uniformize(x$motheroccupation)
# x$motheroccupation[lowfreql(x$motheroccupation, freq=5)] <- 'other'
# x$motherreligion[x$motherreligion=='jewish'] <- 'other'
# x$fatherreligion[x$fatherreligion=='jewish'] <- 'other'
# ftable(x$enrol, x$motherreligion)
# x$birthorder[x$birthorder > 7] <- '> 7'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatherliterate + motherliterate  + 
#   fatherreligion + motherreligion + 
#   fatheroccupation + motheroccupation + region
# mlin <- lm(f, data=x)
# mlgt <- glm(f, data=x, family=binomial(link='logit'))
# modlist <- RegressionList(list(mlin, mlgt))
# screenreg(modlist$models, digits=3)
# screenreg(modlist$models, omit.coef='occupation', digits=3)
# screenreg(modlist$models, override.se=modlist$se, override.pval=modlist$pv,
#   omit.coef='occupation', digits=3)
rm(ir)
rm(x)

#-------------
# ireland 1901 
#-------------
ir <- fread('ireland19015pc.csv', sep=',',
  data.table=FALSE, colClasses='character')
setnames(ir, gsub(' ', '.', names(ir)))
ir <- ir[!duplicated(ir), ] # for some reason 99% of families are double
ir$smpl <- 'archie-ireland-1901'
ir$year <- 1901
ir$iso3 <- 'IRL'

ir$Age[ir$Age==''] <- 0
ir$age <- as.numeric(ir$Age)
ir$child <- ir$age < 16

ir$schoolage <- ir$age > 5 & ir$age < edv$edageendlowerbound[edv$iso3 %in% ir$iso3 & edv$year %in% ir$year]

ir$occupation <- tolower(ir$Occupation)
ir$occhisco <- NA

schlrgx <- "sch|scol|cho+l|sho+l|sco|sca|pupil|appr|apr|^ap|cool|appern|sclol|sllol"
ir$enrol <- grepl(schlrgx, ir$occupation) & ir$child
ir$enrol[grepl('monit|(not|never).*scho', ir$occupation)] <- FALSE

litrgx <- "^r|^c r|^w r|wre|^can [^n]|both|ye|can r|^w|^fre|only|educ|^able|all|engl|good|grad|litt|can$"
ir$literate <- grepl(litrgx, ir$Literacy, ignore.case=T)
# unique(grepr(, ir$Literacy[ir$literate], ignore.case=T))
ir$literate[grepl('not$|not read|nor', ir$Literacy, ignore.case=T)] <- FALSE

ir$male <- ir$Sex=='Male'
ir$male[ir$Sex=='-'] <- NA
ir$female <- ir$Sex=='Female'
ir$female[ir$Sex=='-'] <- NA

ir$married <- grepl('^Married|Wido|Wife|Yes', ir$Marital.Status)
ir$married[ir$Marital.Status %in% c('-', '?')] <- NA

ir$relate <- tolower(ir$Relation.to.head)
sonrgx <- "^sons?$|son.*head|[0-9]nd son|step.?son|eldest son|^child( |r)|child$|adopt|farm.*son|son wife"
ir$son <- grepl(sonrgx, ir$relate) & ir$male

# look again, starting fro d.*t.*r|da.*r
dgtrgx <- "^daughter?$|daughter.*head|[0-9]nd daughter|step.?daughter|eldest daughter|^child( |r)|adopt|farm.*daughter|daughter wife|^child$"
ir$daughter <- grepl(dgtrgx, ir$relate) & ir$female
ir$lateral <- grepl('niece|nephew|brother|sister|uncle|aunt', ir$relate)
ir$upward <- grepl('father|mother|parent', ir$relate)
ir$ext <- ir$lateral | ir$upward
srvrgx <- "coach|cook|serv.*nt|keep|help|maid|nurse|porter|stew|wait|butl|domestic|govern"
ir$servant <- grepl(srvrgx, ir$relate)
ir$head <- grepl('^head ?[^n]', ir$relate)
ir$spouse <- grepl('^wife$|^wife.*(head|family|mohter)|house wife', ir$relate)

catrgx <- "r.*(ca[tch]|chat)|rom|cat|roam|calith|chat|cac|^r c|^st|cotho|caatl"
prtrgx <- "ir.*|ang.*|eng.*|c.*of (i|e)|scot|prot|germ|pr[sey]|pyres|pers|pes|pris|un|ep|pisc|esp|ba|con|chris|wes|br.*t?h?|ih? c|u f|friend|cov|sal|mora|^c (i|e)|^l|clare|pil|swe|adv|qua|wel|zwing|m[ie]a?[td]|inde?p|plym|esta"
jewrgx <- "jew|[ij]sr|heb|istr?a"
ir$religion[grepl(catrgx, ir$Religion, ignore.case=T)] <- 'catholic'
ir$religion[grepl(prtrgx, ir$Religion, ignore.case=T)] <- 'protestant'
ir$religion[grepl(jewrgx, ir$Religion, ignore.case=T)] <- 'jewish'
ir$religion[is.na(ir$religion)] <- 'other'

ir$ethnic <- 'willfully ignored'
ir$race <- NA

ir$fid <- gsub('/pages/1901/', '', ir$fid)
ir$region <- vstrsplit(ir$fid, '/')[, 1]
ir$ded <- vstrsplit(ir$fid, '/')[, 2]
ir$street <- vstrsplit(ir$fid, '/')[, 3]
ir$hhid <- vstrsplit(ir$fid, '/')[, 4]
ir$persid <- 1:nrow(ir)

deds <- table(paste0(ir$ded, ir$region))
urbandeds <- deds[deds > 200]
ir$urban <- paste0(ir$ded, ir$region) %in% names(urbandeds)

ir$region <- tolower(ir$region)

ir$gq <- makegq(ir)
ir <- ir[ir$gq < 5 & !is.na(ir$gq), ]

ir <- rm_dualhh(ir)

write.csv(ir[vrbs], paste0('cleandata/', unique(ir$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(ir), extravrbs='literate')
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(ir, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x <- x[x$age > 13 & !x$schoolage & x$age < 16 & !is.na(x$age), ]

# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$motheroccupation <- Uniformize(x$motheroccupation)
# x$fatheroccupation[lowfreql(x$fatheroccupation, freq=10)] <- 'other'
# x$motheroccupation[lowfreql(x$motheroccupation, freq=10)] <- 'other'
# # x$birthorder[x$birthorder > 5] <- '> 5'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatherliterate + motherliterate  + 
#   fatherreligion + motherreligion + 
#   fatheroccupation + motheroccupation + region
rm(ir)
rm(x)

############
### NAPP ###
############

# iceland1901.csv # no scholars
# mecklschwer1819.csv # no relate
# canada1881.csv # no relate
# norwaynapp.csv # very few student strings

# napp relate codes
mothercodes <- c(0201, 0202, 0203)
childcodes <- c(301, 302, 306)
latcodes <- c(701:704, 801:802, 1021:1044) 
upwcodes <- c(501:503, 601:602, 1011:1013)
servantcodes <- c(1210:1217)

#--------------
# Scotland 1881
#--------------
sc81 <- fread('scotland1881.csv', header=T,
  sep=',', data.table=F) 
names(sc81) <- tolower(names(sc81))
dim(sc81)
sc81 <- sc81[sc81$gq < 5, ]
dim(sc81)

set.seed(2781)
smplserials <- sample(sc81$serial, round(length(unique(sc81$serial)) / 10), replace=F)
sc81 <- sc81[sc81$serial %in% smplserials, ]

sc81$smpl <- 'napp-scotland-1881-10pc'
sc81$iso3 <- 'GBR'

sc81$age[sc81$age > 900] <- NA
sc81$child <- sc81$age < 16
sc81$schoolage <- sc81$age > 5 & sc81$age < edv$edageendlowerbound[edv$iso3 %in% sc81$iso3 & edv$year %in% sc81$year]

sc81$occupation <- tolower(sc81$occstr)

schlrgx <- "sch|^(sc|sh)o.?l|app|pup|[ (]ap[)]?$|^[(]?ap[ )]|apren|stud|apr"
sc81$enrol <- grepl(schlrgx, sc81$occupation) & sc81$child
sc81$enrol[grepl("[lcr]app|teach|govern|^[(]?not|just", sc81$occupation)] <- FALSE
sc81$literate <- NA

sc81$male <- sc81$sex==1
sc81$female <- sc81$sex==2
sc81$male[sc81$sex > 2] <- NA
sc81$female[sc81$sex > 2] <- NA

sc81$married <- sc81$marst <= 5
sc81$married[sc81$marst == 9] <- NA

sc81$son <- (sc81$relate %in% childcodes) & sc81$male
sc81$daughter <- (sc81$relate %in% childcodes)  & sc81$female
sc81$lateral <- (sc81$relate %in% latcodes) 
sc81$upward <- (sc81$relate %in% upwcodes) 
sc81$ext <- sc81$upward | sc81$lateral
sc81$servant <- sc81$relate %in% servantcodes
sc81$spouse <- sc81$relate %in% mothercodes
sc81$head <- sc81$relate %in% 0101

sc81$religion <- NA 
sc81$ethnic <- sc81$bplcntry
sc81$race <- NA
sc81$hhid <- sc81$serial
sc81$persid <- 1:nrow(sc81)
sc81$urban <- sc81$urban %in% 2
sc81$region <- sc81$countygb

sc81 <- rm_dualhh(sc81)

write.csv(sc81[c(vrbs, 'ordergb')], paste0('cleandata/', unique(sc81$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(sc81), extravrbs='ordergb')
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(sc81, x, 'hhid')

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation <- x$fatherordergb
# x$motheroccupation <- x$motherordergb
# table(x$birthorder, x$enrol)
# x$birthorder[x$birthorder > 7] <- '> 7'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   factor(fatheroccupation) + factor(motheroccupation) + region
rm(sc81)
rm(x)

#-----------------------
# England and Wales 1881
#-----------------------
en81 <- fread('englandwales1881.csv', header=T, 
  sep=',', data.table=F)
setnames(en81, tolower(names(en81)))
en81 <- en81[en81$gq < 5, ]

set.seed(2781)
smplserials <- sample(en81$serial, round(length(unique(en81$serial)) / 100), replace=F)
en81 <- en81[en81$serial %in% smplserials, ]
dim(en81)

en81$smpl <- 'napp-englandwales-1881-1pc'
en81$iso3 <- 'GBR'

en81$age[en81$age > 900] <- NA
en81$child <- en81$age < 16
en81$schoolage <- en81$age > 5 & en81$age < edv$edageendlowerbound[edv$iso3 %in% en81$iso3 & edv$year %in% en81$year]

en81$occupation <- tolower(en81$occstr)

# group_strings(en81$occupation[en81$child])
schlrgx <- "sch|^(sc|sh)[ao].?l|app|pup|[ (]ap[)]?$|^[(]?ap[ )]|apren|stud|apr|^sco?$|ysgol|[(]sc?r?[)]|apic|^s$"
en81$enrol <- grepl(schlrgx, en81$occupation) & en81$child
en81$enrol[grepl("[lcntr]app|teach|monit|govern|shawl|scaler|^[(]?not|just", en81$occupation)] <- FALSE
en81$literate <- NA

en81$male <- en81$sex==1
en81$female <- en81$sex==2
en81$male[en81$sex > 2] <- NA
en81$female[en81$sex > 2] <- NA

en81$married <- en81$marst <= 5
en81$married[en81$marst == 9] <- NA

en81$son <- (en81$relate %in% childcodes) & en81$male
en81$daughter <- (en81$relate %in% childcodes)  & en81$female
en81$lateral <- (en81$relate %in% latcodes) 
en81$upward <- (en81$relate %in% upwcodes) 
en81$ext <- en81$upward | en81$lateral
en81$servant <- en81$relate %in% servantcodes
en81$spouse <- en81$relate %in% mothercodes
en81$head <- en81$relate %in% 0101

en81$religion <- NA
en81$ethnic <- en81$bplcntry
en81$race <- NA
en81$hhid <- en81$serial
en81$persid <- 1:nrow(en81)
en81$urban <- en81$urban %in% 2
en81$region <- en81$countygb

en81 <- rm_dualhh(en81)

write.csv(en81[c(vrbs, 'ordergb')], paste0('cleandata/', unique(en81$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(en81), extravrbs='ordergb')
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]
ss <- calcsumstats(en81, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation <- x$fatherordergb
# x$motheroccupation <- x$motherordergb
# table(x$birthorder, x$enrol)
# # x$birthorder[x$birthorder > 7] <- '> 7'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   factor(fatheroccupation) + factor(motheroccupation) + region

rm(en81)
rm(x)

#-------------------
# Great Britain 1851
#-------------------
gbr <- fread('greatbritain1851.csv', header=T, 
  sep=',', data.table=F)
setnames(gbr, tolower(names(gbr)))
dim(gbr)
table(gbr$gq)
gbr <- gbr[gbr$gq < 5 & !is.na(gbr$gq), ]
dim(gbr)

set.seed(2781)
smplserials <- sample(gbr$serial, round(length(unique(gbr$serial)) / 10), replace=F)
gbr <- gbr[gbr$serial %in% smplserials, ]
dim(gbr)

gbr$smpl <- 'napp-great_britain-1851'
gbr$iso3 <- 'GBR'

gbr$age[gbr$age > 900] <- NA
gbr$child <- gbr$age < 16
gbr$schoolage <- gbr$age > 5 & gbr$age < edv$edageendlowerbound[edv$iso3 %in% gbr$iso3 & edv$year %in% gbr$year]

gbr$occupation <- tolower(gbr$occstr)
# occhisco already NA

# group_strings(gbr$occupation[gbr$child])
schlrgx <- "sch|^(sc|sh)[ao].?l|app|pup|[ (]ap[)]?$|^[(]?ap[ )]|apren|stud|apr|^sco?$|ysgol|[(]sc?r?[)]|apic|^s$"
gbr$enrol <- grepl(schlrgx, gbr$occupation) & gbr$child
gbr$enrol[grepl("[lcntr]app|teach|monit|govern|shawl|scaler|^[(]?not|just", gbr$occupation)] <- FALSE
# TODO(check if this creates crazy enrols)
# no, but it is false for non-children.

gbr$literate <- NA

gbr$male <- gbr$sex==1
gbr$female <- gbr$sex==2
gbr$male[gbr$sex > 2] <- NA
gbr$female[gbr$sex > 2] <- NA

gbr$married <- gbr$marst <= 5
gbr$married[gbr$marst == 9] <- NA

gbr$son <- (gbr$relate %in% childcodes) & gbr$male
gbr$daughter <- (gbr$relate %in% childcodes)  & gbr$female
gbr$lateral <- (gbr$relate %in% latcodes) 
gbr$upward <- (gbr$relate %in% upwcodes) 
gbr$ext <- gbr$upward | gbr$lateral
gbr$servant <- gbr$relate %in% servantcodes
gbr$spouse <- gbr$relate %in% mothercodes
gbr$head <- gbr$relate %in% 0101

gbr$religion <- NA
gbr$ethnic <- gbr$bplcntry
gbr$race <- NA
gbr$hhid <- gbr$serial
gbr$persid <- 1:nrow(gbr)
gbr$urban <- gbr$urban %in% 2
gbr$region <- gbr$countygb

gbr <- rm_dualhh(gbr)

write.csv(gbr[c(vrbs, 'ordergb')], paste0('cleandata/', unique(gbr$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(gbr), extravrbs='ordergb')
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(gbr, x, 'hhid')

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation <- x$fatherordergb
# x$motheroccupation <- x$motherordergb
# table(x$birthorder, x$enrol)
# # x$birthorder[x$birthorder > 10] <- '> 10'
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   factor(fatheroccupation) + factor(motheroccupation) + region

rm(gbr)
rm(x)

#------------
# Canada 1891
#------------
ca <- fread('canada1891.csv', header=T, 
  sep=',', data.table=F)
setnames(ca, tolower(names(ca)))
ca <- ca[ca$gq < 5, ]

set.seed(2781)
smplserials <- sample(ca$serial, round(length(unique(ca$serial)) / 10), replace=F)
ca <- ca[ca$serial %in% smplserials, ]
dim(ca)

ca$smpl <- 'napp-canada-1891'
ca$iso3 <- 'CAN'

ca$age[ca$age > 900] <- NA
ca$child <- ca$age < 16
ca$schoolage <- ca$age > 5 & ca$age < edv$edageendlowerbound[edv$iso3 %in% ca$iso3 & edv$year %in% ca$year]

ca$occupation <- tolower(ca$occstr)
# occhisco already coded

# group_strings(ca$occupation[ca$child])
schlrgx <- 'stud|pup|elev|sch|ecol|etud|novi|coll|appr|\\bapp|apr|apt'
ca$enrol <- grepl(schlrgx, ca$occupation) & ca$child
ca$enrol[ca$occstr==''] <- NA
# not many

ca$literate <- ca$lit > 2
ca$literate[ca$lit==9] <- NA

ca$male <- ca$sex==1
ca$female <- ca$sex==2
ca$male[ca$sex > 2] <- NA
ca$female[ca$sex > 2] <- NA

ca$married <- ca$marst <= 5
ca$married[ca$marst == 9] <- NA

ca$son <- (ca$relate %in% childcodes) & ca$male
ca$daughter <- (ca$relate %in% childcodes)  & ca$female
ca$lateral <- (ca$relate %in% latcodes) 
ca$upward <- (ca$relate %in% upwcodes) 
ca$ext <- ca$upward | ca$lateral
ca$servant <- ca$relate %in% servantcodes
ca$spouse <- ca$relate %in% mothercodes
ca$head <- ca$relate %in% 0101

religion <- NA
religion[ca$religion < 2000] <- 'catholic'
religion[ca$religion >= 2000] <- 'protestant'
religion[ca$religion >= 8900] <- 'other'
religion[ca$religion == 8900] <- 'orthodox'
religion[ca$religion > 9000 & ca$religion < 9200] <- 'jewish'
religion[ca$religion ==9200] <- 'muslim'
ca$religion <- religion

ca$ethnic <- ca$bplcntry
ca$race <- NA
ca$hhid <- ca$serial
ca$persid <- 1:nrow(ca)
ca$urban <- ca$urban %in% 2
ca$region <- ca$provca

ca <- rm_dualhh(ca)

write.csv(ca[vrbs], paste0('cleandata/', unique(ca$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(ca), extravrbs=c('occhisco', 'literate'))
# x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]
x <- x[x$age > 5 & x$age < 16 & !is.na(x$age), ]
dim(x)
ss <- calcsumstats(ca, x, 'hhid')

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation <- substring(x$fatherocchisco, 1, 1)
# x$motheroccupation <- substring(x$motherocchisco, 1, 1)
# ftable(x$fatheroccupation, x$enrol)
# ftable(x$motheroccupation, x$enrol)
# x$motheroccupation[x$motheroccupation <= 6] <- 1
# x$motheroccupation[x$motheroccupation >= 7] <- 7
# ftable(x$region, x$enrol)
# ftable(x$fatherreligion, x$enrol)
# ftable(x$motherreligion, x$enrol)
# # x$motherreligion[x$motherreligion=='catholic'] <- 'other'
# ftable(x$fatherliterate, x$enrol)
# ftable(x$motherliterate, x$enrol)
# ftable(x$fatheroccupation, x$enrol)
# table(x$birthorder, x$enrol)
# x$birthorder[x$birthorder > 6] <- '> 6'
# f <- enrol ~ nsib + age + male + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat - hhid + 
#   fatherliterate + motherliterate + 
#   fatherreligion + motherreligion + 
#   factor(fatheroccupation) + factor(motheroccupation) # + factor(region)

rm(ca)
rm(x)

#-----------
### USA 1860
#-----------
us <- fread('usa1860.csv', header=T, sep=',')
us <- as.data.frame(us)

names(us) <- tolower(names(us))
us <- us[us$gq < 5, ]

# set.seed(2781)
# smplserials <- sample(us$serial, round(length(unique(us$serial)) / 10), replace=F)
# us <- us[us$serial %in% smplserials, ]
# dim(us)

us$smpl <- 'napp-usa-1860'
us$iso3 <- 'USA'

us$age[us$age > 900] <- NA
us$child <- us$age < 16
us$schoolage <- us$age > 5 & us$age < edv$edageendlowerbound[edv$iso3 %in% us$iso3 & edv$year %in% us$year]

us$occupation <- tolower(us$occstr)
# occhiso already NA

us$enrol <- us$school==2
us$enrol[us$school==9] <- NA

us$literate <- us$lit > 2
us$literate[us$lit==9] <- NA

us$male <- us$sex==1
us$female <- us$sex==2
us$male[us$sex > 2] <- NA
us$female[us$sex > 2] <- NA

# us$married <- us$marst <= 5
# us$married[us$marst == 9] <- NA
us$married <- NA # no marst in US < 1880

us$son <- (us$relate %in% childcodes) & us$male
us$daughter <- (us$relate %in% childcodes)  & us$female
us$lateral <- (us$relate %in% latcodes) 
us$upward <- (us$relate %in% upwcodes) 
us$ext <- us$upward | us$lateral
us$servant <- us$relate %in% servantcodes
us$spouse <- us$relate %in% mothercodes
us$head <- us$relate %in% 0101

us$religion <- NA

us$ethnic <- NA
us$ethnic[us$hispa > 0] <- 'hispanic'
us$ethnic[us$hispa==900] <- NA
us$ethnic[us$francnam==2] <- 'french'

us$hhid <- us$serial
us$persid <- paste0(us$serial, us$pernum)
us$urban <- us$urban %in% 2
us$region <- us$stateus

us <- rm_dualhh(us)

write.csv(us[c(vrbs, 'ocscorus', 'servants')], 
  paste0('cleandata/', unique(us$smpl), '.csv'), row.names=F)

us$servant <- TRUE
x <- hhaggregate(data.table(us), extravrbs=c('occhisco', 'ocscorus', 'literate', 'race', 'servants'))
x$nserv <- x$fatherservants
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

us$married <- FALSE
ss <- calcsumstats(us, x, 'hhid')
ss$sample_servants <- sum(x$nserv > 0, na.rm=T) / length(na.omit(x$nserv))

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# imputed incomes are only possibility because occhisco is missing
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# sort(table(x$fatheroccupation))
# x$fatheroccupation[lowfreql(x$fatheroccupation, 25)] <- 'other'
# x$fatherincome <- x$fatherocscorus
# x$motherincome <- x$motherocscorus
# ftable(x$age, x$enrol) / rowSums(ftable(x$age, x$enrol))
# f <- enrol ~ nsib + age + male + urban + # factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat + nupw - hhid + 
#   fatherliterate + motherliterate + 
#   factor(fatheroccupation) + 
#   factor(fatherrace) + factor(motherrace) + 
#   log1p(fatherincome) + log1p(motherincome) + factor(region)
# resulting nsib is stubbornly positive

rm(us)
rm(x)

#-----------
### USA 1850
#-----------
us <- fread('usa1850.csv', header=T, sep=',')
us <- as.data.frame(us)

names(us) <- tolower(names(us))
us <- us[us$gq < 5, ]

# set.seed(2781)
# smplserials <- sample(us$serial, round(length(unique(us$serial)) / 10), replace=F)
# us <- us[us$serial %in% smplserials, ]

us$smpl <- 'napp-usa-1850'
us$iso3 <- 'USA'

us$age[us$age > 900] <- NA
us$child <- us$age < 16
us$schoolage <- us$age > 5 & us$age < edv$edageendlowerbound[edv$iso3 %in% us$iso3 & edv$year %in% us$year]

us$occupation <- tolower(us$occstr)
# occhisco already NA

us$enrol <- us$school==2
us$enrol[us$school==9] <- NA

us$literate <- us$lit > 2
us$literate[us$lit==9] <- NA

us$male <- us$sex==1
us$female <- us$sex==2
us$male[us$sex > 2] <- NA
us$female[us$sex > 2] <- NA

# us$married <- us$marst <= 5
# us$married[us$marst == 9] <- NA
us$married <- NA # no marst < 1880

us$son <- (us$relate %in% childcodes) & us$male
us$daughter <- (us$relate %in% childcodes)  & us$female
us$lateral <- (us$relate %in% latcodes) 
us$upward <- (us$relate %in% upwcodes) 
us$ext <- us$upward | us$lateral
us$servant <- us$relate %in% servantcodes
us$spouse <- us$relate %in% mothercodes
us$head <- us$relate %in% 0101

us$religion <- NA

us$ethnic <- NA
us$ethnic[us$hispa > 0] <- 'hispanic'
us$ethnic[us$hispa==900] <- NA
us$ethnic[us$francnam==2] <- 'french'

us$hhid <- us$serial
us$persid <- paste0(us$serial, us$pernum)
us$urban <- us$urban %in% 2
us$region <- us$stateus

us <- rm_dualhh(us)

write.csv(us[c(vrbs, 'ocscorus', 'servants')], 
  paste0('cleandata/', unique(us$smpl), '.csv'), row.names=F)

us$servant <- TRUE
x <- hhaggregate(data.table(us), extravrbs=c('occhisco', 'ocscorus', 'literate', 'race', 'servants'))
x$nserv <- x$fatherservants
x$servant <- x$nserv > 0
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

us$married <- FALSE
ss <- calcsumstats(us, x, 'hhid')
ss$sample_servants <- sum(x$nserv > 0, na.rm=T) / length(na.omit(x$nserv))
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# imputed incomes are only option because occhisco is missing
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# sort(table(x$fatheroccupation))
# x$fatheroccupation[lowfreql(x$fatheroccupation, 25)] <- 'other'
# x$fatherincome <- x$fatherocscorus
# x$motherincome <- x$motherocscorus
# ftable(x$age, x$enrol) / rowSums(ftable(x$age, x$enrol))
# f <- enrol ~ nsib + age + male + urban + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat + nupw - hhid + 
#   fatherliterate + motherliterate + 
#   factor(fatheroccupation) + 
#   factor(fatherrace) + factor(motherrace) + 
#   log1p(fatherincome) + log1p(motherincome) + factor(region)
# resulting nsib is stubbornly positive

rm(us)
rm(x)

#-----------
### USA 1870
#-----------
us <- fread('usa1870.csv', header=T, sep=',')
us <- as.data.frame(us)

names(us) <- tolower(names(us))
us <- us[us$gq < 5, ]

# set.seed(2781)
# smplserials <- sample(us$serial, round(length(unique(us$serial)) / 10), replace=F)
# us <- us[us$serial %in% smplserials, ]
# dim(us)

us$smpl <- 'napp-usa-1870'
us$iso3 <- 'USA'

us$age[us$age > 900] <- NA
us$child <- us$age < 16
us$schoolage <- us$age > 5 & us$age < edv$edageendlowerbound[edv$iso3 %in% us$iso3 & edv$year %in% us$year]

us$occupation <- tolower(us$occstr)
# occhisco already NA

us$enrol <- us$school==2
us$enrol[us$school==9] <- NA

us$literate <- us$lit > 2
us$literate[us$lit==9] <- NA

us$male <- us$sex==1
us$female <- us$sex==2
us$male[us$sex > 2] <- NA
us$female[us$sex > 2] <- NA

# us$married <- us$marst <= 5
# us$married[us$marst == 9] <- NA
us$married <- NA # no marst < 1880

us$son <- (us$relate %in% childcodes) & us$male
us$daughter <- (us$relate %in% childcodes)  & us$female
us$lateral <- (us$relate %in% latcodes) 
us$upward <- (us$relate %in% upwcodes) 
us$ext <- us$upward | us$lateral
us$servant <- us$relate %in% servantcodes
us$spouse <- us$relate %in% mothercodes
us$head <- us$relate %in% 0101

us$religion <- NA

us$ethnic <- NA
us$ethnic[us$hispa > 0] <- 'hispanic'
us$ethnic[us$hispa==900] <- NA
us$ethnic[us$francnam==2] <- 'french'

us$hhid <- us$serial
us$persid <- paste0(us$serial, us$pernum)
us$urban <- us$urban %in% 2
us$region <- us$stateus

us <- rm_dualhh(us)

write.csv(us[c(vrbs, 'ocscorus', 'servants')], 
  paste0('cleandata/', unique(us$smpl), '.csv'), row.names=F)

us$servant <- TRUE
x <- hhaggregate(data.table(us), extravrbs=c('occhisco', 'ocscorus', 'literate', 'race', 'servants'))
x$nserv <- x$fatherservants
x$servant <- x$nserv > 0
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

us$married <- FALSE
ss <- calcsumstats(us, x, 'hhid')
ss$sample_servants <- sum(x$nserv > 0, na.rm=T) / length(na.omit(x$nserv))

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# imputed incomes are only option because occhisco is missing
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$fatheroccupation[lowfreql(x$fatheroccupation, 25)] <- 'other'
# x$fatherincome <- x$fatherocscorus
# x$motherincome <- x$motherocscorus
# ftable(x$age, x$enrol) / rowSums(ftable(x$age, x$enrol))
# f <- enrol ~ nsib + age + male + urban + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat + nupw - hhid + 
#   fatherliterate + motherliterate + 
#   factor(fatheroccupation) + 
#   factor(fatherrace) + factor(motherrace) + 
#   log1p(fatherincome) + log1p(motherincome) + factor(region)
# resulting nsib is stubbornly positive

rm(us)
rm(x)
#------------
### USA 1880b
#------------
us <- fread('usa1880b.csv', header=T, sep=',')
us <- as.data.frame(us)

names(us) <- tolower(names(us))
us <- us[us$gq < 5, ]

set.seed(2781)
smplserials <- sample(us$serial, round(length(unique(us$serial)) / 10), replace=F)
us <- us[us$serial %in% smplserials, ]

us$smpl <- 'napp-usa-1880b'
us$iso3 <- 'USA'

us$age[us$age > 900] <- NA
us$child <- us$age < 16
us$schoolage <- us$age > 5 & us$age < edv$edageendlowerbound[edv$iso3 %in% us$iso3 & edv$year %in% us$year]

us$occupation <- tolower(us$occstr)
# occhisco already NA

us$enrol <- us$school==2
us$enrol[us$school==9] <- NA

us$literate <- us$lit > 2
us$literate[us$lit==9] <- NA

us$male <- us$sex==1
us$female <- us$sex==2
us$male[us$sex > 2] <- NA
us$female[us$sex > 2] <- NA

us$married <- us$marst <= 5
us$married[us$marst == 9] <- NA

us$son <- (us$relate %in% childcodes) & us$male
us$daughter <- (us$relate %in% childcodes)  & us$female
us$lateral <- (us$relate %in% latcodes) 
us$upward <- (us$relate %in% upwcodes) 
us$ext <- us$upward | us$lateral
us$servant <- us$relate %in% servantcodes
us$spouse <- us$relate %in% mothercodes
us$head <- us$relate %in% 0101

us$religion <- NA

us$ethnic <- NA
us$ethnic[us$hispa > 0] <- 'hispanic'
us$ethnic[us$hispa==900] <- NA
us$ethnic[us$francnam==2] <- 'french'

us$hhid <- us$serial
us$persid <- paste0(us$serial, us$pernum)
us$urban <- us$urban %in% 2
us$region <- us$stateus

us <- rm_dualhh(us)

write.csv(us[c(vrbs, 'ocscorus', 'servants')], 
  paste0('cleandata/', unique(us$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(us), extravrbs=c('occhisco', 'ocscorus', 'literate', 'race', 'servants'))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(us, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# imputed incomes are only possibility because occhisco is missing
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$fatheroccupation[lowfreql(x$fatheroccupation, 25)] <- 'other'
# x$fatherincome <- x$fatherocscorus
# x$motherincome <- x$motherocscorus
# ftable(x$age, x$enrol) / rowSums(ftable(x$age, x$enrol))
# f <- enrol ~ nsib + age + male + urban + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat + nupw - hhid + 
#   fatherliterate + motherliterate + 
#   factor(fatheroccupation) + 
#   factor(fatherrace) + factor(motherrace) + 
#   log1p(fatherincome) + log1p(motherincome) + factor(region)

rm(us)
rm(x)
#------------
### USA 1900
#------------
us <- fread('usa1900.csv', header=T, sep=',')
us <- as.data.frame(us)

names(us) <- tolower(names(us))
us <- us[us$gq < 5, ]

set.seed(2781)
smplserials <- sample(us$serial, round(length(unique(us$serial)) / 10), replace=F)
us <- us[us$serial %in% smplserials, ]

us$smpl <- 'napp-usa-1900'
us$iso3 <- 'USA'

us$age[us$age > 900] <- NA
us$child <- us$age < 16
us$schoolage <- us$age > 5 & us$age < edv$edageendlowerbound[edv$iso3 %in% us$iso3 & edv$year %in% us$year]

us$occupation <- tolower(us$occstr)
# occhisco already NA

us$enrol <- us$school==2
us$enrol[us$school==9] <- NA

us$literate <- us$lit > 2
us$literate[us$lit==9] <- NA

us$male <- us$sex==1
us$female <- us$sex==2
us$male[us$sex > 2] <- NA
us$female[us$sex > 2] <- NA

us$married <- us$marst <= 5
us$married[us$marst == 9] <- NA

us$son <- (us$relate %in% childcodes) & us$male
us$daughter <- (us$relate %in% childcodes)  & us$female
us$lateral <- (us$relate %in% latcodes) 
us$upward <- (us$relate %in% upwcodes) 
us$ext <- us$upward | us$lateral
us$servant <- us$relate %in% servantcodes
us$spouse <- us$relate %in% mothercodes
us$head <- us$relate %in% 0101

us$religion <- NA

us$ethnic <- NA
us$ethnic[us$hispa > 0] <- 'hispanic'
us$ethnic[us$hispa==900] <- NA
us$ethnic[us$francnam==2] <- 'french'

us$hhid <- us$serial
us$persid <- paste0(us$serial, us$pernum)
us$urban <- us$urban %in% 2
us$region <- us$stateus

us <- rm_dualhh(us)

write.csv(us[c(vrbs, 'ocscorus', 'servants')], 
  paste0('cleandata/', unique(us$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(us), extravrbs=c('occhisco', 'ocscorus', 'literate', 'race', 'servants'))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(us, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# imputed incomes are only possibility because occhisco is missing
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$fatheroccupation[lowfreql(x$fatheroccupation, 25)] <- 'other'
# x$fatherincome <- x$fatherocscorus
# x$motherincome <- x$motherocscorus
# ftable(x$age, x$enrol) / rowSums(ftable(x$age, x$enrol))
# f <- enrol ~ nsib + age + male + urban + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat + nupw - hhid + 
#   fatherliterate + motherliterate + 
#   factor(fatheroccupation) + 
#   factor(fatherrace) + factor(motherrace) + 
#   log1p(fatherincome) + log1p(motherincome) + factor(region)

rm(us)
rm(x)
#------------
### USA 1910
#------------
us <- fread('usa1910.csv', header=T, sep=',')
us <- as.data.frame(us)

names(us) <- tolower(names(us))
us <- us[us$gq < 5, ]

us$smpl <- 'napp-usa-1910'
us$iso3 <- 'USA'

us$age[us$age > 900] <- NA
us$child <- us$age < 16
us$schoolage <- us$age > 5 & us$age < edv$edageendlowerbound[edv$iso3 %in% us$iso3 & edv$year %in% us$year]

us$occupation <- tolower(us$occstr)
# occhisco already NA

us$enrol <- us$school==2
us$enrol[us$school==9] <- NA

us$literate <- us$lit > 2
us$literate[us$lit==9] <- NA

us$male <- us$sex==1
us$female <- us$sex==2
us$male[us$sex > 2] <- NA
us$female[us$sex > 2] <- NA

us$married <- us$marst <= 5
us$married[us$marst == 9] <- NA

us$son <- (us$relate %in% childcodes) & us$male
us$daughter <- (us$relate %in% childcodes)  & us$female
us$lateral <- (us$relate %in% latcodes) 
us$upward <- (us$relate %in% upwcodes) 
us$ext <- us$upward | us$lateral
us$servant <- us$relate %in% servantcodes
us$spouse <- us$relate %in% mothercodes
us$head <- us$relate %in% 0101

us$religion <- NA

us$ethnic <- NA
us$ethnic[us$hispa > 0] <- 'hispanic'
us$ethnic[us$hispa==900] <- NA
us$ethnic[us$francnam==2] <- 'french'

us$hhid <- us$serial
us$persid <- paste0(us$serial, us$pernum)
us$urban <- us$urban %in% 2
us$region <- us$stateus

us <- rm_dualhh(us)

write.csv(us[c(vrbs, 'ocscorus', 'servants')], 
  paste0('cleandata/', unique(us$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(us), extravrbs=c('occhisco', 'ocscorus', 'literate', 'race', 'servants'))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

ss <- calcsumstats(us, x, 'hhid')

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# imputed incomes are only possibility because occhisco is missing
# x$fatheroccupation <- Uniformize(x$fatheroccupation)
# x$fatheroccupation[lowfreql(x$fatheroccupation, 25)] <- 'other'
# x$fatherincome <- x$fatherocscorus
# x$motherincome <- x$motherocscorus
# ftable(x$age, x$enrol) / rowSums(ftable(x$age, x$enrol))
# f <- enrol ~ nsib + age + male + urban + servant + factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat + nupw - hhid + 
#   fatherliterate + motherliterate + 
#   factor(fatheroccupation) + 
#   factor(fatherrace) + factor(motherrace) + 
#   log1p(fatherincome) + log1p(motherincome) + factor(region)

rm(us)
rm(x)

#------------
# Canada 1901
#------------
ca <- read.spss('canada 1901/1901.sav', to.data.frame=TRUE)
dim(ca)
ca$smpl <- 'canfamilies-canada-1901'
ca$iso3 <- 'CAN'
ca$year <- 1901

ca$hhid <- as.character(ca$hhdid)

ca$gq <- as.character(tolower(gsub('\\s+$', '', ca$binst)))

killids <- unique(ca$hhid[!ca$gq==''])
ca <- ca[!ca$hhid %in% killids, ]
dim(ca)

killids <- unique(ca$hhid[grepl('HD[0-9]', ca$relhead)])
ca <- ca[!ca$hhid %in% killids, ]
dim(ca)

ca$age <- ca$ageyr
ca$birthmy <- paste0(ca$byear, ca$bmonth)
ca$birthmy[is.na(ca$byear)] <- NA
ca$birthmy[ca$bmonth=='   '] <- NA
ca$child <- ca$age < 16
ca$schoolage <- ca$age > 5 & ca$age < edv$edageendlowerbound[edv$iso3 %in% ca$iso3 & edv$year %in% ca$year]
# 16, do not use with < 16

ca$occupation <- tolower(gsub('\\s+$', '', ca$occ))
ca$occode <- ca$occ1 # not hisco!
ca$occscale <- ca$occ2 # not hiscam!
ca$occhisco <- NA

ca$enrol <- ca$moschool > 0 | (ca$occupation >= 24000 & ca$occupation < 25000)
# no child condition because schoolage problem and no regex troubles

ca$literate <- ca$canwrite=='Y' | ca$canread=='Y'
ca$literate[grepl('[^YN]', ca$canwrite)] <- NA
ca$literate[grepl('[^YN]', ca$canread)] <- NA

ca$male <- ca$sex=='M'
ca$male[grepl('[^MF]', ca$sex)] <- NA
ca$female <- ca$sex=='F'
ca$female[grepl('[^MF]', ca$sex)] <- NA

ca$married <- grepl('[MDWP]', ca$marst)
ca$married[!ca$married & ca$marst!='S'] <- NA

ca$relate <- gsub('\\s+$', '', ca$relhead)
ca$son <- ca$relate %in% c("SON" , "ADOPTED SON", "SON OF #MOM", "W.SON", "SON OF WF", "SON OF #WF", "#SON") & ca$male
ca$daughter <- ca$relate %in% c("DAU", "ADOPTED DAU", "DAU OF WF", "DAU OF PARTNER", "#DAU", "# DAU") & ca$female
ca$lateral <- grepl("NEICE|NIECE|UNCLE|COUSIN|AUNT|NEPHEW|BRO|SIS", ca$relate)
ca$upward <- grepl("FAT|MOTHER|MOM", ca$relate)
ca$ext <- ca$upward | ca$lateral
ca$servant <- ca$relate %in% c("SERVANT", "DOM", "COOK", "SERV")
ca$head <- ca$relate %in% c('HD', 'HHD', 'HEAD', '#HD')
ca$spouse <- ca$relate %in% c("WF", "SPOUSE", "WF OF HUSBAND", "WIFE", "WF OF PARTNER", "#WF", "WF 1ST", "WF 2ND", "WF 3RD")

ca$religion <- NA
ca$religion[ca$religio2 >= 10 & ca$religio2 <=13]  <- 'catholic'
ca$religion[ca$religio2 >= 51 & ca$religio2 <= 499] <- 'protestant'
ca$religion[ca$religio2 >= 601 & ca$religio2 <= 607] <- 'jewish'
ca$religion[ca$religio2 == 501] <- 'orthodox'
ca$religion[is.na(ca$religion)] <- 'other'

# ca$ethnic <- ca$bpl2
ca$ethnic <- NA
ca$ethnic[ca$mtongue2==100] <- 'english'
ca$ethnic[ca$mtongue2==1100] <- 'french'
ca$ethnic[ca$mtongue2==200] <- 'german'
ca$ethnic[is.na(ca$ethnic)] <- 'other'
ca$race <- as.character(ca$colour)
ca$race[ca$race %in% c(' ', '!', '?', '#')] <- NA
ca$persid <- 1:nrow(ca)
ca$urban <- ca$urbpop > 5000 & !is.na(ca$urbpop)
ca$region <- as.character(ca$province)

# additional
ca$income <- ca$earnings
ca$property <- ca$propownr=='Y'

ca <- rm_dualhh(ca)

write.csv(ca[c(vrbs, 'income', 'property', 'occode', 'occscale')], 
  paste0('cleandata/', unique(ca$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(ca), extravrbs=c('literate', 'income', 'property', 'ethnic', 'occode', 'occscale'))
x <- x[x$age > 5 & x$age < 16 & !is.na(x$age), ]
# x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

# check oldlats
ss <- calcsumstats(ca, x, 'hhid')
write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatherethnic[x$fatherethnic < 10000] <- 'USA'
# x$fatherethnic[x$fatherethnic < 16000 & x$fatherethnic > 10000] <- 'Canadian'
# x$motherethnic[x$motherethnic < 10000] <- 'USA'
# x$motherethnic[x$motherethnic < 16000 & x$motherethnic > 10000] <- 'Canadian'

# x$fatheroccode <- substring(x$fatheroccode, 1, 1)
# x$motheroccode <- substring(x$fatheroccode, 1, 1)
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatherliterate + motherliterate  + 
#   fatherreligion + motherreligion + 
#   factor(fatherethnic) + factor(motherethnic) + 
#   # log(fatherincome) + log(motherincome) + 
#   fatherproperty + motherproperty + 
#   # factor(fatheroccode) + # factor(motheroccode) + # any occupation/income measure loses
#   region

rm(ca)
rm(x)


#------------
# Sweden 1890
#------------
# too few scholars registered in sweden to make meaningful estimates
se <- fread('sweden1890.csv', 
  header=T, sep=',',  data.table=F)
setnames(se, tolower(names(se)))
se <- se[se$gq < 5, ]

set.seed(2781)
smplserials <- sample(se$serial, round(length(unique(se$serial)) / 10), replace=F)
se <- se[se$serial %in% smplserials, ]

se$smpl <- 'napp-sweden-1890'
se$iso3 <- 'SWE'

se$age[se$age > 900] <- NA
se$child <- se$age < 16
se$schoolage <- se$age > 5 & se$age < edv$edageendlowerbound[edv$iso3 %in% se$iso3 & edv$year %in% se$year]

se$occupation <- tolower(se$occstr)
# occhisco already coded

# group_strings(se$occupation[se$child])
schlrgx <- 'stud|sko[^fgnm]|spel|lar[ln]|elev|scho|lar[ia]|gym'
se$enrol <- grepl(schlrgx, se$occupation) & se$child
se$enrol[se$occstr==''] <- NA # not many

se$literate <- NA

se$male <- se$sex==1
se$female <- se$sex==2
se$male[se$sex > 2] <- NA
se$female[se$sex > 2] <- NA

se$married <- se$marst <= 5
se$married[se$marst == 9] <- NA

se$son <- (se$relate %in% childcodes) & se$male
se$daughter <- (se$relate %in% childcodes)  & se$female
se$lateral <- (se$relate %in% latcodes) 
se$upward <- (se$relate %in% upwcodes) 
se$ext <- se$upward | se$lateral
se$servant <- se$relate %in% servantcodes
se$spouse <- se$relate %in% mothercodes
se$head <- se$relate %in% 0101

religion <- NA
religion[se$religion < 2000] <- 'catholic'
religion[se$religion >= 2000] <- 'protestant'
religion[se$religion >= 8900] <- 'other'
religion[se$religion == 8900] <- 'orthodox'
religion[se$religion > 9000 & se$religion < 9200] <- 'jewish'
religion[se$religion ==9200] <- 'muslim'
se$religion <- religion

se$ethnic <- se$bplcntry
se$race <- NA
se$hhid <- se$serial
se$persid <- 1:nrow(se)
se$urban <- se$urban %in% 2
se$region <- se$countyse

se <- rm_dualhh(se)

write.csv(se[vrbs], paste0('cleandata/', unique(se$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(se), extravrbs=c('occhisco'))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]
# x <- x[x$age > 5 & x$age < 16 & !is.na(x$age), ]
dim(x)
ss <- calcsumstats(se, x, 'hhid')

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation <- substring(x$fatherocchisco, 1, 1)
# x$motheroccupation <- substring(x$motherocchisco, 1, 1)
# ftable(x$fatheroccupation, x$enrol)
# ftable(x$motheroccupation, x$enrol)
# x$motheroccupation[x$motheroccupation <= 6] <- 1
# x$motheroccupation[x$motheroccupation >= 7] <- 7
# f <- enrol ~ nsib + age + male + servant + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap + nupw + nlat - hhid + 
#   fatheroccupation + motheroccupation + region

rm(se)
rm(x)

#------------
# Sweden 1900
#------------
se <- fread('sweden1900.csv', 
  header=T, sep=',', data.table=F)
setnames(se, tolower(names(se)))
se <- se[se$gq < 5, ]

set.seed(2781)
smplserials <- sample(se$serial, round(length(unique(se$serial)) / 10), replace=F)
se <- se[se$serial %in% smplserials, ]

se$smpl <- 'napp-sweden-1900'
se$iso3 <- 'SWE'

se$age[se$age > 900] <- NA
se$child <- se$age < 16
se$schoolage <- se$age > 5 & se$age < edv$edageendlowerbound[edv$iso3 %in% se$iso3 & edv$year %in% se$year]

se$occupation <- tolower(se$occstr)
# occhisco already coded

# group_strings(se$occupation[se$child])
schlrgx <- 'stud|sko[^fgnm]|spel|lar[ln]|elev|scho|lar[ia]|gym'
se$enrol <- grepl(schlrgx, se$occupation) & se$child
se$enrol[se$occstr==''] <- NA
# not many

se$literate <- NA

se$male <- se$sex==1
se$female <- se$sex==2
se$male[se$sex > 2] <- NA
se$female[se$sex > 2] <- NA

se$married <- se$marst <= 5
se$married[se$marst == 9] <- NA

se$son <- (se$relate %in% childcodes) & se$male
se$daughter <- (se$relate %in% childcodes)  & se$female
se$lateral <- (se$relate %in% latcodes) 
se$upward <- (se$relate %in% upwcodes) 
se$ext <- se$upward | se$lateral
se$servant <- se$relate %in% servantcodes
se$spouse <- se$relate %in% mothercodes
se$head <- se$relate %in% 0101

religion <- NA
religion[se$religion < 2000] <- 'catholic'
religion[se$religion >= 2000] <- 'protestant'
religion[se$religion >= 8900] <- 'other'
religion[se$religion == 8900] <- 'orthodox'
religion[se$religion > 9000 & se$religion < 9200] <- 'jewish'
religion[se$religion ==9200] <- 'muslim'
se$religion <- religion

se$ethnic <- se$bplcntry
se$race <- NA
se$hhid <- se$serial
se$persid <- 1:nrow(se)
se$urban <- se$urban %in% 2
se$region <- se$countyse

se <- rm_dualhh(se)

write.csv(se[vrbs], paste0('cleandata/', unique(se$smpl), '.csv'), row.names=F)

x <- hhaggregate(data.table(se), extravrbs=c('occhisco'))
x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]
dim(x)
ss <- calcsumstats(se, x, 'hhid')

write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  sep=',', append=T, col.names=F, row.names=F)

# x$fatheroccupation <- substring(x$fatherocchisco, 1, 1)
# x$motheroccupation <- substring(x$motherocchisco, 1, 1)
# ftable(x$fatheroccupation, x$enrol)
# ftable(x$motheroccupation, x$enrol)
# x$fatheroccupation[x$fatheroccupation <= 6] <- 1
# x$fatheroccupation[x$fatheroccupation >= 7] <- 7
# x$motheroccupation[x$motheroccupation <= 6] <- 1
# x$motheroccupation[x$motheroccupation >= 7] <- 7
# # ftable(x$region, x$enrol)
# # ftable(x$fatherreligion, x$enrol)
# # ftable(x$motherreligion, x$enrol)
# # ftable(x$birthorder, x$enrol)
# f <- enrol ~ nsib + age + male + urban + factor(birthorder) + 
#   motheragefirstchild + spousagegap  + nlat - hhid + 
#   factor(fatheroccupation) + factor(motheroccupation) # + factor(region)

rm(se)
rm(x)


#############
### tmplt ###
#############

# smpl
# year
# age
# child
# schoolage
# occupation
# occhisco
# enrol
# literate
# male
# female
# married
# relate
# son
# daughter
# lateral
# upward
# ext
# servant
# head
# spouse
# religion
# ethnic
# race
# hhid
# persid
# urban
# region

# gq
# rm_dualhh()

# write.csv(, paste0('cleandata/', unique(##$smpl), '.csv'), row.names=F)

# x <- hhaggregate(data.table(,) extravrbs='literate')
# x <- x[x$age > 5 & x$age < 16 & !x$schoolage & !is.na(x$age), ]

# ss <- calcsumstats(, x, 'hhid')

# write.table(ss, '~/dropbox/viennapaper/tabfigs/sumstats_all_harm.csv', 
  # sep=',', append=T, col.names=F, row.names=F)
