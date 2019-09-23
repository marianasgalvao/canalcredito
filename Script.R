#Importando os dados
library("readxl")
base=read_excel("C:\\Users\\User\\Documents\\UNB\\PET\\Monografia\\Dados\\basefagreg.xlsx")
basets <- ts(base, start=c(2011,3), end=c(2017,7), frequency=12)

#Identificando as variaveis
data <- basets[,1]
credtot <-basets[,2]
credreclivres <- basets[,3]
juroslivetot <- basets[,4]
spreadmed <- basets[,5]
m1 <- basets[,6]
titpub <- basets[,7]
selic <- basets[,8]
igpdi <- basets[,9]
benscap <- basets[,10]
bensint <- basets[,11]
bensconsd <- basets[,12]
bensconsnd <- basets[,13]
indgeral <- basets[,14]
ibc <- basets[,15]

#Deflacionando
deflator <- basets[71,9]/basets[,9]
credtotr <- credtot*deflator
credreclr <- credreclivres*deflator
juroslr <- juroslivetot*deflator
spreadmr <- spreadmed*deflator
m1r <- m1*deflator
titpubr <- titpub*deflator
benscapr <- benscap*deflator
bensintr<- bensint*deflator
bensconsdr <- bensconsd*deflator
bensconsndr <- bensconsnd*deflator
indgr <- indgeral*deflator

#Base deflacionada
library(vars)
basedefl <- cbind(credtotr, credreclr, juroslr, spreadmr, m1r, titpubr, selic, benscapr, bensintr, bensconsdr, bensconsndr, indgr )
basetsd <- ts(basedefl, start=c(2011,03), end=c(2017,7), frequency=12)
descdefl <- summary(basetsd)
write.table(descdefl, "C:\\Users\\User\\Documents\\UNB\\PET\\Monografia\\descritivadeflacinadanlog.txt", sep="\t") 

#Desvios padrao
sd(credtotr)
sd(selic)
sd(credreclr)
sd(spreadmr)
sd(juroslr)
sd(m1r)
sd(titpubr)
sd(benscapr)
sd(bensconsdr)
sd(bensconsndr)
sd(bensintr)
sd(indgr)

#Plotagem
plot(credreclr)
plot(credtotr)
plot(juroslr)
plot(spreadmr)
plot(m1r)
plot(titpubr)
plot(selic)

#Log
lcredt <- log(credtotr)
lcredrl <- log(credreclr)
ljurosl <- log(juroslr)
lspreadmr <- log(spreadmr)
lm1 <- log(m1r)
ltitp <- log(titpubr)
lselic <- log(selic)
lint <- log(bensintr)
lcap <- log(benscapr)
ldur <- log(bensconsdr)
lndur <- log(bensconsndr)
lind <- log(indgr)

#Credito total
plot(stl(lcredt, "periodic"))
glcredt <- decompose(lcredt)
plot(glcredt)

#Crédito Recursos Livres
plot(stl(lcredrl, "periodic"))
glcredrl <- decompose(lcredrl)
plot(glcredrl)

#Juros
plot(stl(ljurosl, "periodic"))
gljurosl <- decompose(ljurosl)
plot(gljurosl)

#Spread
plot(stl(lspreadmr, "periodic"))
glspreadmr <- decompose(lspreadmr)
plot(glspreadmr)

#M1
plot(stl(lm1, "periodic"))
glm1 <- decompose(lm1)
plot(glm1)

#Titulos públicos
plot(stl(ltitp, "periodic"))
gltitp <- decompose(ltitp)
plot(gltitp)

#Bens Intermediarios
plot(stl(lint, "periodic"))
glint <- decompose(lint)
plot(glint)

#Bens de Capital
plot(stl(lcap, "periodic"))
glcap <- decompose(lcap)
plot(glcap)

#Bens duráveis
plot(stl(ldur, "periodic"))
gldur <- decompose(ldur)
plot(gldur)

#Bens não duráveis
plot(stl(lndur, "periodic"))
glndur <- decompose(lndur)
plot(glndur)

#Industria
plot(stl(lind, "periodic"))
glind <- decompose(lind)
plot(glind)

#Selic
plot(stl(lselic, "periodic"))
glselic <- decompose(lselic)
plot(glselic)

##Sazonalidade
saz_ldur <- ordered(cycle(ldur))
sazldur.reg <- lm(ldur~saz_ldur)
summary(sazldur.reg)

saz_lcap <- ordered(cycle(lcap))
sazlcap.reg <- lm(lcap~saz_lcap)
summary(sazlcap.reg)

##Tem
saz_lcredrl <- ordered(cycle(lcredrl))
sazlcredrl.reg <- lm(lcredrl~saz_lcredrl)
summary(sazlcredrl.reg)

##Tem
saz_lndur <- ordered(cycle(lndur))
sazlndur.reg <- lm(lndur~saz_lndur)
summary(sazlndur.reg)

##Tem
saz_lcredt <- ordered(cycle(lcredt))
sazlcredt.reg <- lm(lcredt~saz_lcredt)
summary(sazlcredt.reg)

##Tem
saz_lint <- ordered(cycle(lint))
sazlint.reg <- lm(lint~saz_lint)
summary(sazlint.reg)

saz_lm1 <- ordered(cycle(lm1))
sazlm1.reg <- lm(lm1~saz_lm1)
summary(sazlm1.reg)

saz_ljurosl <- ordered(cycle(ljurosl))
sazljurosl.reg <- lm(ljurosl~saz_ljurosl)
summary(sazljurosl.reg)

saz_lselic <- ordered(cycle(lselic))
sazlselic.reg <- lm(lselic~saz_lselic)
summary(sazlselic.reg)

saz_ltitp <- ordered(cycle(ltitp))
sazltitp.reg <- lm(ltitp~saz_ltitp)
summary(sazltitp.reg)

saz_lspreadmr <- ordered(cycle(lspreadmr))
sazlspreadmr.reg <- lm(lspreadmr~saz_lspreadmr)
summary(sazlspreadmr.reg)

#####Sazonalidade- Ajustes para as séries de qtde
#Hipótese nula: não há sazonalidade
library("seasonal")

ajuste1 <- seas(x = lcredrl, transform.function = 'none')
qs(ajuste1)
summary(ajuste1)
des_lcredrld <- final(seas(lcredrl))
plot(des_lcredrld)
#des_lcredrld <- decompose(lcredrl, type = c("additive"), filter = NULL)
#lcredrld <- lcredrl - des_lcredrld$seasonal

library(seasonal)
ajuste2 <- seas(x = lcredt, transform.function= 'none')
qs(ajuste2)
summary(ajuste2)
des_lcredtd <- final(seas(lcredt))
#des_lcredtd <- decompose(lcredt, type = c("additive"), filter = NULL)
#lcredtd <- lcredrl - des_lcredtd$seasonal

library(seasonal)
ajuste3 <- seas(x = lndur, transform.function = 'none')
qs(ajuste3)
summary(ajuste3)
des_lndur <- final(seas(lndur))

library(seasonal)
ajuste4 <- seas(x = lint, transform.function = 'none')
qs(ajuste4)
summary(ajuste4)
des_lint <- final(seas(lint))


#Salvando base dessazonalizada
library(vars)
baseatsd <- cbind(ldur, lcap, des_lcredrld, des_lndur, des_lcredtd, des_lint, lm1, ljurosl, lselic, ltitp, lspreadmr, lind)
datad <- ts(baseatsd, start=c(2011,03), end=c(2017,7), frequency=12)
write.table(datad, "C:\\Users\\User\\Documents\\UNB\\PET\\Monografia\\basedess.txt", sep="\t")

#Analise descritiva
tabela <- summary(datad)
write.table(tabela, "C:\\Users\\User\\Documents\\UNB\\PET\\Monografia\\descritivadesscomlog.txt", sep="\t") 

sd(lcap)
sd(lcredrld)
sd(lndur)
sd(lcredtd)
sd(lint)
sd(lm1)
sd(ljurosl)
sd(lselic)
sd(ltitp)
sd(lspreadmr)
sd(ldur)
sd(lind)

# Correlação e covariância
cor(datad)
cov(datad)


#1 Não tem raiz unitária
library(urca)
adf.juros <- ur.df(ljurosl, type='trend', lags=12)
acf(adf.juros@res, ci.type='ma', main='ACF Resíduos - ADF com 12 defasagens',
    xlab='Defasagem')
summary(adf.juros)


#2 - Não tem raiz
library(urca)
adf.m1 <- ur.df(lm1, type='drift', lags=24)
acf(adf.m1@res, ci.type='ma', main='ACF Resíduos - ADF com 24 defasagens',
    xlab='Defasagem')
summary(adf.m1)


#3 - Tem raiz
library(urca)
adf.int <- ur.df(des_lint, type='drift', lags=3)
acf(adf.int@res, ci.type='ma', main='ACF Resíduos - ADF com 3 defasagens',
    xlab='Defasagem')
summary(adf.int)

#Sem raiz
dd_lint <- diff(des_lint)
library(urca)
adf.ddint <- ur.df(dd_lint, type='drift', lags=3)
acf(adf.ddint@res, ci.type='ma', main='ACF Resíduos - ADF com 3 defasagens',
    xlab='Defasagem')
summary(adf.ddint)

#4- Não tem raiz
library(urca) 
adf.lind <- ur.df(lind, type='none', lags=0)
acf(adf.lind@res, ci.type='ma', main='ACF Resíduos - ADF com sem defasagens',
    xlab='Defasagem')
summary(adf.lind)

#5 - Tem raiz
library(urca)
adf.b <- ur.df(ltitp, type='trend', lags=6)
acf(adf.b@res, ci.type='ma', main='ACF Resíduos - ADF com 6 defasagens',
    xlab='Defasagem')
summary(adf.b)

###Sem raiz 
d_ltitp <-diff(ltitp)
adf.dtitp <- ur.df(dif_ltitp, type='trend', lags=6)
acf(adf.dtitp@res, ci.type='ma', main='ACF Resíduos - ADF com 6 defasagens',
    xlab='Defasagem')
summary(adf.dtitp)

#6- Sem raiz
library(urca)
adf.c <- ur.df(lspreadmr, type='trend', lags=12)
acf(adf.c@res, ci.type='ma', main='ACF Resíduos - ADF com 12 defasagens',
    xlab='Defasagem')
summary(adf.c)

#7- Tem raiz
library(urca)
adf.q <- ur.df(des_lcredtd, type='drift', lags=0)
acf(adf.q@res, ci.type='ma', main='ACF Resíduos - ADF com 0 defasagens',
    xlab='Defasagem')
summary(adf.q)

##Sem raiz
dd_credt <-diff(des_lcredtd)
adf.dcredt <- ur.df(dd_credt, type='drift', lags=3)
acf(adf.dcredt@res, ci.type='ma', main='ACF Resíduos - ADF com 6 defasagens',
    xlab='Defasagem')
summary(adf.dcredt)

#8 - Def 6 - Não rejeita raiz unitária
library(urca)
adf.t <- ur.df(des_lcredrld, type='drift', lags=3)
acf(adf.t@res, ci.type='ma', main='ACF Resíduos - ADF com 3 defasagens',
    xlab='Defasagem')
summary(adf.t)

## Sem raiz
dd_credrl <-diff(des_lcredrld)
adf.dcredrl<- ur.df(dd_credrl, type='drift', lags=3)
acf(adf.dcredrl@res, ci.type='ma', main='ACF Resíduos - ADF com 6 defasagens',
    xlab='Defasagem')
summary(adf.dcredrl)

#9 Def 12 - Não rejeita raiz unitária
library(urca)
adf.selic<- ur.df(lselic, type='none', lags=9)
acf(adf.selic@res, ci.type='ma', main='ACF Resíduos - ADF com 9 defasagens',
    xlab='Defasagem')
summary(adf.selic)

##Sem raiz
d_selic <-diff(lselic)
adf.dselic <- ur.df(d_selic, type='none', lags=9)
acf(adf.dselic@res, ci.type='ma', main='ACF Resíduos - ADF com 9 defasagens',
    xlab='Defasagem')
summary(adf.dselic)


#10 - Não há unitária
library(urca)
adf.i<- ur.df(ldur, type='trend', lags=0)
acf(adf.u@res, ci.type='ma', main='ACF Resíduos - ADF com 0 defasagens',
    xlab='Defasagem')
summary(adf.i)

#11 - Tem raiz
library(urca)
adf.o<- ur.df(des_lndur, type='drift', lags=3)
acf(adf.o@res, ci.type='ma', main='ACF Resíduos - ADF com 3 defasagens',
    xlab='Defasagem')
summary(adf.o)

##Sem raiz
dd_ndur  <-diff(des_lndur)
adf.dndur <- ur.df(dd_ndur, type='trend', lags=0)
acf(adf.dndurl@res, ci.type='ma', main='ACF Resíduos - ADF com 0 defasagens',
    xlab='Defasagem')
summary(adf.dndur)

#12 - Tem raiz
library(urca)
adf.j<- ur.df(lcap, type='none', lags=12)
acf(adf.j@res, ci.type='ma', main='ACF Resíduos - ADF com 12 defasagens',
    xlab='Defasagem')
summary(adf.j)

#Sem
d_lcap <-diff(lcap)
adf.dcap<- ur.df(d_lcap, type='none', lags=11)
acf(adf.dcap@res, ci.type='ma', main='ACF Resíduos - ADF com 11 defasagens',
    xlab='Defasagem')
summary(adf.dcap)

#PP
PP.test(lselic)
PP.test(ljurosl)
PP.test(lm1)
PP.test(des_lint)
PP.test(dd_lint)
#Não tem raiz unitária
PP.test(des_lcredtd)

#Não tem raiz unitaria 
PP.test(lndur)

#Não tem raiz unitária 
PP.test(des_lcredrld)

#Não tem raiz unitaria 
PP.test(lcap)

#Não tem raiz unitária
PP.test(ldur)


#Diferenciando as séries e testando estacionaridade
d_jurl <- diff(ljurosl)
plot(djurl)
library(tseries)
pp.test(d_jurl, alternative=c("stationary"))

d_spread <- diff(lspreadmr)
plot(dspread)
library(tseries)
pp.test(d_spread, alternative=c("stationary"))


d_lm1 <- diff(lm1)
plot(dlm1)
library(tseries)
pp.test(d_lm1, alternative=c("stationary"))

d_ind <- diff(lind)
plot(d_ind)
library(tseries)
pp.test(d_ind,alternative=c("stationary"))

library(tseries)
pp.test(d_lcap, alternative=c("stationary"))

library(tseries)
pp.test(dd_ndur, alternative=c("stationary"))

library(tseries)
pp.test(d_selic, alternative=c("stationary"))

library(tseries)
pp.test(dd_credrl, alternative=c("stationary"))

library(tseries)
pp.test(dd_credt, alternative=c("stationary"))

library(tseries)
pp.test(d_ltitp, alternative=c("stationary"))

library(tseries)
pp.test(dd_lint, alternative=c("stationary"))

#Teste de normalidade
shapiro.test(d_selic)
shapiro.test(d_lm1)#não é normal
shapiro.test(dd_credt)
shapiro.test(d_jurl)

#Plotar Estacionaridade
plot(d_selic, xlab='Tempo', ylab='Selic')
plot(dd_credrl, xlab='Tempo', ylab='Crédito com recursos livres')
plot(dd_credt, xlab='Tempo', ylab="Crédito total")
plot(d_ltitp, xlab='Tempo', ylab="Títulos públicos")

#Salvando base final
library(vars)
baseajust <- cbind(d_selic, dd_credrl, dd_credt, d_ltitp, d_jurl, d_selic, d_lm1, d_spread, d_ind)
base_macro <- ts(baseajust, start=c(2011,03), end=c(2017,7), frequency=12)
write.table(base_macro, "C:\\Users\\User\\Documents\\UNB\\PET\\Monografia\\final.txt", sep="\t") 


######\o/\o/\o/\o/\o/\o/\o/\o/\o/############
#Teste de Causalidade de Granger
d_ibc <- diff(ibc)

grangertest(d_ind, d_lm1,order=2)
grangertest(d_ind, d_spread, order=2)
grangertest(d_ind, d_selic, order=2)
grangertest(d_ind,dd_credt, order=2)
grangertest(d_ind, d_jurl, order=2)

grangertest(d_ind, d_lm1,order=3)
grangertest(d_ind, d_spread, order=3)
grangertest(d_ind, d_selic, order=3)
grangertest(d_ind,dd_credt, order=3)
grangertest(d_ind, d_jurl, order=3)

grangertest(d_ind, d_lm1,order=6)
grangertest(d_ind, d_spread, order=6)
grangertest(d_ind, d_selic, order=6)
grangertest(d_ind,dd_credt, order=6)
grangertest(d_ind, d_jurl, order=6)

grangertest(d_ind, d_lm1,order=9)
grangertest(d_ind, d_spread, order=9)
grangertest(d_ind, d_selic, order=9)
grangertest(d_ind,dd_credt, order=9)
grangertest(d_ind, d_jurl, order=9)

#Sentido contrário
grangertest(d_lm1,d_ind, order=2)
grangertest(d_spread,d_ind, order=2)
grangertest(d_selic,d_ind, order=2)
grangertest(dd_credt, d_ind, order=2)
grangertest(d_jurl, d_ind, order=2)

grangertest(d_lm1,d_ind, order=3)
grangertest(d_spread,d_ind, order=3)
grangertest(d_selic,d_ind, order=3)
grangertest(dd_credt, d_ind, order=3)
grangertest(d_jurl, d_ind, order=3)

grangertest(d_lm1,d_ind, order=6)
grangertest(d_spread,d_ind, order=6)
grangertest(d_selic,d_ind, order=6)
grangertest(dd_credt, d_ind, order=6)
grangertest(d_jurl, d_ind, order=6)

grangertest(d_lm1,d_ind, order=9)
grangertest(d_spread,d_ind, order=9)
grangertest(d_selic,d_ind, order=9)
grangertest(dd_credt, d_ind, order=9)
grangertest(d_jurl, d_ind, order=9)

#Causa produto
library(vars)
vinter <- cbind(d_selic, d_lm1, dd_credt, d_ind)
var.inter.d2 <- VAR(vinter, p=2, type=c("const"))
summary(var.inter.d2)
causality(var.inter.d2, cause = "d_ind")$Granger
AIC(var.inter.d2, k=2)

var.inter.d3 <- VAR(vinter, p=3, type=c("const"))
summary(var.inter.d3)
causality(var.inter.d3, cause = "d_ind")$Granger
AIC(var.inter.d3)

#Causa
var.inter.d6 <- VAR(vinter, p=6, type=c("const"))
summary(var.inter.d6)
causality(var.inter.d6, cause = "d_ind")$Granger
AIC(var.inter.d6)

#Causa
var.inter.d9 <- VAR(vinter, p=9, type=c("const"))
summary(var.inter.d9)
causality(var.inter.d9, cause = "d_ind")$Granger
AIC(var.inter.d9)


#FRIs
library(vars)
fri2 <- cbind(dd_credt,d_ind, d_lm1, d_ltitp, d_selic)
fri3 <- cbind(lcredt, lind, lm1, ltitp, lselic)

varfri1 <- VAR(fri1, p=3, type = c("const"))
summary(varfri1)


varfri2 <- VAR(fri2, p= 2, type= c("const"))
summary(varfri2)


irfind <- irf(varfri2, impulse = c("d_selic"), response = c("d_ind"), n.ahead = 30)
plot(irfind, ylab='Produto')

irfm1 <- irf(varfri2, impulse = c("d_selic"), response = c("d_lm1"), n.ahead = 30)
plot(irfm1, ylab='M1')

irftitp<- irf(varfri2, impulse = c("d_selic"), response = c("d_ltitp"), n.ahead = 30)
plot(irftitp, ylab='Títulos públicos')

irfcredt <- irf(varfri2, impulse = c("d_selic"), response = c("dd_credt"), n.ahead = 30)
plot(irfcredt, ylab='Crédito')

irfsp <- irf(varfri2, impulse = c("d_selic"), response=c("d_spread"), n.ahead = 30)
plot(irfsp, ylab='Spread')

##Em nível
irfncred <- irf(varfri3, impulse = c("lselic"), response = c("lcredt"), n.ahead = 20)
plot(irfnivel, ylab= 'Crédito')

irfnind <- irf(varfri3, impulse = c("lselic"), response= c("lind"), n.ahead = 20)
plot(irfind, ylab='Produto')

irfnm1 <- irf(varfri3, impulse = c("lselic"), response = c("lm1"), n.ahead = 20)
plot(irfnm1, ylab='M1')

irfntitulo <- irf(varfri3, impulse = c("lselic"), reponse = c("ltitp"), n.ahead=20)
plot(irfntitulo, ylab='Títulos públicos')

library(urca)
johansen <- ca.jo(fri2, type = c("eigen", "trace"), ecdet = c("const"), K = 2)
summary(johansen)