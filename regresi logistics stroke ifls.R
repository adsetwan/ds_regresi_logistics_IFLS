library(plyr)
library(foreign)
dat <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\bk_sc1.dta")
dat2 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3a_cov.dta")
dat3 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_cd3.dta")
dat4 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3a_dl1.dta")
dat5 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_km.dta")
dat6 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b1_ks1.dta")
dat7 <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_kk2.dta")
#tingkat pendidikan
edu <- dat4[,c("dl06","hhid14","pidlink")]
str(edu)
d <- read.dta(file= "D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\bt_time.dta")
head(d)
levels(edu$dl06)
edu$dl06 <- revalue(edu$dl06, c("2:Elementary school" = "basic", "3:Junior high general" = "basic", "4:Junior high vocational" = "basic", "5:Senior high general" = "secondary", "6:Senior high vocational" = "secondary", "11:Adult education A" = "basic", "12:Adult education B" = "basic", "13:Open university" = "higher", "15:Adult education C" = "secondary", "60:College (D1,D2,D3)" = "higher", "61:University S1" = "higher", "62:University S2" = "higher", "63:University S3" = "higher", "72:Islamic Elementary School (Madrasah Ibtidaiyah)" = "basic", "73:Islamic Junior/High School (Madrasah Tsanawiyah)" = "basic", "74:Islamic Senior/High School (Madrasah Tsanawiyah)" = "secondary"))
levels(edu$dl06)
edu <- edu[edu$dl06 == "basic" | edu$dl06 == "secondary" | edu$dl06 == "higher",]
edu <- na.omit(edu)
sapply(edu, function(x) sum(is.na(x)))
nrow(edu)
#hitung frekuensi level pendidikan
d <- dat4[,"dl06"]
a <- count(d)
a[order(-a$freq),]
#stroke, hipertensi, diabetes, kanker otak
d <- dat3[dat3$cdtype == "A" | dat3$cdtype == "B" | dat3$cdtype == "H" | dat3$cdtype == "I" | dat3$cdtype == "Q",]
da <- d
head(da)
unique(da$cd06)
peny <- da[, c("cdtype","cd05","hhid14", "pidlink")]
levels(peny$cd05)
peny$cd05 <- revalue(peny$cd05, c("1:Yes" = "Ya", "3:No" = "Tidak"))
head(peny)
peny <- peny[peny$cd05 == "Ya" | peny$cd05 == "Tidak",]
sapply(peny, function(x) sum(is.na(x)))
nrow(peny)
#kebiasaan merokok
head(dat5)
km <- dat5[dat5$km01a == "1:Yes" | dat5$km01a == "3:No",c("km01a", "hhid14", "pidlink")]
sapply(km, function(x) sum(is.na(x)))
nrow(km)
#umur, gender, & marstat
head(dat2)
ugm <- dat2[, c("age", "marstat", "sex","pidlink", "hhid14")]
sapply(ugm, function(x) sum(is.na(x)))
ugm <- na.omit(ugm)
nrow(ugm)
#desa atau kota
head(dat)
dk <- dat[,c("sc05","hhid14")]
sapply(dk, function(x) sum(is.na(x)))
nrow(dk)
#kegiatan fisik
kf <- dat7[dat7$kk02m == "1:Yes", c("kktype","kk02m","hhid14", "pidlink")]
sapply(kf, function(x) sum(is.na(x)))
head(kf)
nrow(kf)
#merge data
library(tidyverse)
mdat <- peny %>%
  left_join(y=edu, by=c("hhid14","pidlink")) %>%
  left_join(y=km, by=c("hhid14","pidlink")) %>%
  left_join(y= ugm, by=c("hhid14","pidlink")) %>%
  left_join(y=kf, by=c("hhid14","pidlink"))
nrow(mdat)
sapply(mdat, function(x) sum(is.na(x)))
head(mdat)
levels(mdat$kk02m)
str(mdat)
mdat <- mdat[mdat$kk02m == "1:Yes",]
head(mdat)
mdat <- na.omit(mdat)
nrow(mdat)
str(mdat)
table(mdat$marstat)
mdat$marstat <- revalue(mdat$marstat, c("1:Not yet married" = "Belum Menikah", "2:Married"= "Menikah", "4:Divorced" = "Bercerai", "5:Widowed" = "Bercerai"))
unique(mdat$marstat)
mdat <- mdat[mdat$marstat == "Menikah" | mdat$marstat == "Belum Menikah" | mdat$marstat == "Bercerai", ]
head(mdat)
nrow(mdat)
str(mdat)
mdat$cdtype <- as.factor(mdat$cdtype)
mdat$kktype <- as.factor(mdat$kktype)
str(mdat)
levels(mdat$cd05)
mdat <- mdat[mdat$cd05 == "Ya" | mdat$cd05 == "Tidak",]
pix <- mdat %>% left_join(y=dk, by = "hhid14")
head(pix)
str(pix)
table(pix$km01a)
pix$km01a <- revalue(pix$km01a, c("1:Yes" = "Ya", "3:No" = "Tidak"))
pix$sex <- revalue(pix$sex, c("1:Male" = "Laki-laki", "3:Female" = "Perempuan"))
pix$sc05 <- revalue(pix$sc05, c("1:Urban" = "Perkotaan", "2:Rural" = "Pedesaan"))
head(pix)
unique(pix$kk02m)
str(pix)
pix <- pix[,-11]
sapply(pix, function(x) sum(is.na(x)))
tdr <- read.dta("D:\\Adi\\Kuliah\\Semester\\Semester 4\\Komputasi Statistika\\hh14_all_dta\\b3b_tdr.dta")
head(tdr)
unique(tdr$tdr01)
str(tdr)
tdr1 <- tdr[tdr$tdrtype == 1 | tdr$tdrtype == 2,]
tdr1 <- tdr1[,c(4:7)]
head(tdr1)
str(tdr1)
tdr1$tdrtype <- as.factor(tdr1$tdrtype)
tdr1$tdrtype <- revalue(tdr1$tdrtype, c("1" = "GangguanTidur", "2" = "KualitasTidur"))
tdr1$tdr01 <- as.factor(tdr1$tdr01)
tdr1 <- tdr1 %>% spread(tdrtype, tdr01)
head(tdr1)
table(tdr1$GangguanTidur)
tdr1$GangguanTidur <- revalue(tdr1$GangguanTidur, c("1" = "tidak pernah", "2" = "jarang", "3" = "kadang-kadang", "4" = "sering", "5" = "selalu"))
tdr1$KualitasTidur <- revalue(tdr1$KualitasTidur, c("1" = "sangat buruk", "2" = "buruk", "3" = "cukup", "4" = "baik", "5" = "sangat baik"))
head(tdr1)
fix <- pix %>%
  left_join(y=tdr1, by=c("hhid14","pidlink"))
fix <- na.omit(fix)
write.csv2(fix, file = "D:\\reglog.csv")
###
dat <- read.csv2("D:\\reglog.csv")
head(dat)
unique(dat$penyakit)
library(plyr)
dat$penyakit <- revalue(dat$penyakit, c("A" = "Hipertensi", "B" = "Diabetes", "H" = "Stroke", "I" = "Kanker", "Q" = "Psikis"))
str(dat)
library(tidyverse)
dat <- dat %>% spread(penyakit, status)
head(dat)
dat[dat$KualitasTidur == "9",]
dat <- dat[-c(11056,11057),]
head(dat)
write.csv2(dat, file = "D:\\fixreglog.csv")
#####################
###Mulai dari sini###
#####################

dat <- read.csv2("D:\\fixreglog.csv")
str(dat)
datnew <- dat[dat$Hipertensi == "Ya",]
table(datnew$Stroke)
nrow(datnew)
dat <- dat[,-c(1:3, 11:12, 17)]
str(dat)
summary(dat)
library(caret)
set.seed(41004)
index <- createDataPartition(dat$Stroke, p = .80, list = FALSE)
train <- dat[index,]
test <- dat[-index,]
table(train$Stroke)
table(test$Stroke)
#SMOTE
library(smotefamily)
str(train)
train[,-c(3,10)] <- lapply(train[,-c(3,10)], function(x) as.numeric(x))
dat.smote <- SMOTE(train[,-10],train[,10])$data
prop.table(table(dat.smote$class))
nrow(dat.smote)
library(caret)
level3 <- function(t){
  a <- vector()
  for(i in 1:length(t)){
    if(t[i] <= 1){
      a[i] = 1
    }
    else if(t[i] > 1 & t[i] < 3){
      a[i] = 2
    }
    else a[i] = 3
  }
  return(a)
}
level2 <- function(t){
  a <- vector()
  for(i in 1:length(t)){
    if(t[i] <= 1){
      a[i] = 1
    }
    else a[i] = 2
  }
  return(a)
}
str(dat.smote)
dat.smote[,-c(2:3,5,7:11)] <- lapply(dat.smote[,-c(2:3,5,7:11)], function(x) as.factor(level3(x)))
str(dat.smote)
dat.smote[,-c(1,3:4,6,11)] <- lapply(dat.smote[,-c(1,3:4,6,11)], function(x) as.factor(level2(x)))
str(dat.smote)
yatidak <- function(x){
  revalue(x, c("1" = "Tidak", "2" = "Ya"))
}
str(dat.smote)
library(plyr)
dat.smote[,-c(1,3:7,11)] <- lapply(dat.smote[,-c(1,3:7,11)], function(x) yatidak(x))
str(dat.smote)
dat.smote$pendidikan <- revalue(dat.smote$pendidikan, c("1" = "Basic", "2" = "Higher", "3" = "Secondary"))
dat.smote$marstat <- revalue(dat.smote$marstat, c("1" = "Belum menikah", "2" = "Bercerai", "3" = "Menikah"))
dat.smote$gender <- revalue(dat.smote$gender, c("1" = "Laki-laki", "2" = "Perempuan"))
dat.smote$aktivitas <- revalue(dat.smote$aktivitas, c("1" = "Berat", "2" = "Sedang", "3" = "Ringan"))
dat.smote$asal <- revalue(dat.smote$asal, c("1" = "Pedesaan", "2" = "Perkotaan"))
str(dat.smote)
write.csv2(dat.smote,"D:\\Adi\\Kuliah\\Penelitian\\Penelitian10_Reglog_Jurnal_Pelita\\resamplingdata.csv")
dat.smote <- read.csv2("D:\\Adi\\Kuliah\\Penelitian\\Penelitian10_Reglog_Jurnal_Pelita\\resamplingdata.csv")
dat.smote <- dat.smote[,-1]
str(dat.smote)
dat.smote$class <- as.character(dat.smote$class)
dat.smote$class <- ifelse(dat.smote$class == "Ya",1,0)
dat.smote$class <- as.integer(dat.smote$class)
str(dat.smote)
mod <- glm(class ~ . , family = binomial, data = dat.smote)
summary(mod)
str(test)
str(dat.smote)
test$pendidikan <- revalue(test$pendidikan, c("basic" = "Basic", "higher" = "Higher", "secondary" = "Secondary"))
test$aktivitas <- revalue(test$aktivitas, c("A" = "Berat", "B" = "Sedang", "C" = "Ringan"))
test$marstat <- revalue(test$marstat, c("Belum Menikah" = "Belum menikah"))

head(test)
pred <- predict(mod, test, type = "response")
test$predict <- ifelse( pred > 0.5,"Ya","Tidak")
confusionMatrix(table(test$Stroke, test$predict))
library(pROC)
str(dat.smote)
rocplot <- roc(class ~ fitted(mod), data=dat.smote)
plot.roc(rocplot, legacy.axes=TRUE)
auc(rocplot)
