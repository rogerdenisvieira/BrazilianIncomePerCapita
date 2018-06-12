#--------------------------------------- Loading dataframe from a CSV -----------------------------------------

df_county <- read.csv("data/county_data_91-00-10.csv", header = TRUE, sep = ",")

#------------------ Applying a filter into dataframe by year and state Rio Grande do Sul ---------------------

#mun_rs_1991 <- subset.data.frame(df_county, ANO==1991 & UF==43)
mun_rs_2000 <- subset.data.frame(df_county, ANO==2000 & UF==43)
mun_rs_2010 <- subset.data.frame(df_county, ANO==2010 & UF==43)

poa_2010 <- subset.data.frame(mun_rs_2010, Municipio== "PORTO ALEGRE")

#---------------------------------------- Plotting scatter graphics -------------------------------------------

plot(mun_rs_2000$IDHM, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "IDHM")
plot(mun_rs_2000$T_SUPER25M, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "T_SUPER25M")
plot(mun_rs_2000$TRABPUB, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "TRABPUB")

# removing scientific notation from x-axis
format(mun_rs_2000$PEA18M,scientific=FALSE)
plot(mun_rs_2000$PEA18M, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "PEA18M")
plot(mun_rs_2000$T_ENV, mun_rs_2000$RDPC, ylab = "RDPC", xlab = "T_ENV")

#---------------------------------------- Plotting histograms -------------------------------------------------

hist(mun_rs_2000$RDPC, xlab = "RPDC", ylab="Frequência", ylim = c(0,150), xlim = c(0,1500), main = "")
hist(mun_rs_2000$IDHM, xlab = "IDHM", ylab="Frequência", ylim = c(0,200), xlim = c(0.4,0.8),main = "")
hist(mun_rs_2000$T_SUPER25M, xlab = "T_SUPER25M", ylab="Frequência", ylim = c(0,200), xlim = c(0,25), main = "")
hist(mun_rs_2000$TRABPUB, xlab = "TRABPUB", ylab="Frequência", xlim = c(0,25), ylim = c(0,140), main = "")
hist(mun_rs_2000$PEA18M, xlab = "PEA18M", ylab="Frequência", ylim = c(0,500), main = "")
hist(mun_rs_2000$T_ENV, xlab = "T_ENV", ylab="Frequência", ylim = c(0,150), xlim = c(0, 20), main = "")

#---------------------------------------- Variable's amplitude ------------------------------------------------

ampl_RDPC <- (max(mun_rs_2000$RDPC) - min(mun_rs_2000$RDPC))
ampl_IDHM <- (max(mun_rs_2000$IDHM) - min(mun_rs_2000$IDHM))
ampl_T_SUPER25M <- (max(mun_rs_2000$T_SUPER25M) - min(mun_rs_2000$T_SUPER25M))
ampl_TRABPUB <- (max(mun_rs_2000$TRABPUB) - min(mun_rs_2000$TRABPUB))
ampl_PEA18M <- (max(mun_rs_2000$PEA18M) - min(mun_rs_2000$PEA18M))
ampl_T_ENV <- (max(mun_rs_2000$T_ENV) - min(mun_rs_2000$T_ENV))

#------------------------------------------ Linear Correlation ------------------------------------------------

cor(mun_rs_2000$RDPC,mun_rs_2000$IDHM)
cor(mun_rs_2000$RDPC,mun_rs_2000$T_SUPER25M)
cor(mun_rs_2000$RDPC,mun_rs_2000$TRABPUB)
cor(mun_rs_2000$RDPC,mun_rs_2000$PEA18M)
cor(mun_rs_2000$RDPC,mun_rs_2000$T_ENV)

#--------------------------------------------- Normality Test -------------------------------------------------
format(mun_rs_2000$IDHM,scientific=FALSE)
shapiro.test(mun_rs_2000$IDHM)
format(mun_rs_2000$T_SUPER25M,scientific=FALSE)
shapiro.test(mun_rs_2000$T_SUPER25M)
shapiro.test(mun_rs_2000$TRABPUB)
shapiro.test(mun_rs_2000$PEA18M)
shapiro.test(mun_rs_2000$T_ENV)

#------------------------------------------ Linear Regression -------------------------------------------------

reg <- lm(
  mun_rs_2000$RDPC 
  ~ mun_rs_2000$IDHM 
  + mun_rs_2000$PEA18M 
  + mun_rs_2000$TRABPUB
  + mun_rs_2000$T_SUPER25M 
  + mun_rs_2000$T_ENV
)

reg <- lm(mun_rs_2000$RDPC~mun_rs_2000$IDHM)
summary(reg)


y = -898.66311 + 2.28148 * poa_2010$IDHM









