######################################## Loading dataframe from a CSV #########################################

br.counties <- read.csv("./data/county_data_91-00-10.csv", header = TRUE, sep = ";")

################### Applying a filter into dataframe by year and state Rio Grande do Sul #####################

#rs.counties.1991 <- subset.data.frame(br.counties, ANO==1991 & UF==43)
rs.counties.2000 <- subset.data.frame(br.counties, ANO==2000 & UF==43)
rs.counties.2010 <- subset.data.frame(br.counties, ANO==2010 & UF==43)



######################################### Plotting scatter graphics ###########################################

plot(rs.counties.2000$IDHM, rs.counties.2000$RDPC, ylab = "RDPC", xlab = "IDHM")
plot(rs.counties.2000$T_SUPER25M, rs.counties.2000$RDPC, ylab = "RDPC", xlab = "T_SUPER25M")
plot(rs.counties.2000$TRABPUB, rs.counties.2000$RDPC, ylab = "RDPC", xlab = "TRABPUB")

# removing scientific notation from x-axis
format(rs.counties.2000$PEA18M,scientific=FALSE)
plot(rs.counties.2000$PEA18M, rs.counties.2000$RDPC, ylab = "RDPC", xlab = "PEA18M")
plot(rs.counties.2000$T_ENV, rs.counties.2000$RDPC, ylab = "RDPC", xlab = "T_ENV")

######################################### Plotting histograms #################################################

hist(rs.counties.2000$RDPC, xlab = "RPDC", ylab="Frequência", ylim = c(0,150), xlim = c(0,1500), main = "")
hist(rs.counties.2000$IDHM, xlab = "IDHM", ylab="Frequência", ylim = c(0,200), xlim = c(0.4,0.8),main = "")
hist(rs.counties.2000$T_SUPER25M, xlab = "T_SUPER25M", ylab="Frequência", ylim = c(0,200), xlim = c(0,25), main = "")
hist(rs.counties.2000$TRABPUB, xlab = "TRABPUB", ylab="Frequência", xlim = c(0,25), ylim = c(0,140), main = "")
hist(rs.counties.2000$PEA18M, xlab = "PEA18M", ylab="Frequência", ylim = c(0,500), main = "")
hist(rs.counties.2000$T_ENV, xlab = "T_ENV", ylab="Frequência", ylim = c(0,150), xlim = c(0, 20), main = "")

######################################### Variable's amplitude ################################################

ampl_RDPC <- (max(rs.counties.2000$RDPC) - min(rs.counties.2000$RDPC))
ampl_IDHM <- (max(rs.counties.2000$IDHM) - min(rs.counties.2000$IDHM))
ampl_T_SUPER25M <- (max(rs.counties.2000$T_SUPER25M) - min(rs.counties.2000$T_SUPER25M))
ampl_TRABPUB <- (max(rs.counties.2000$TRABPUB) - min(rs.counties.2000$TRABPUB))
ampl_PEA18M <- (max(rs.counties.2000$PEA18M) - min(rs.counties.2000$PEA18M))
ampl_T_ENV <- (max(rs.counties.2000$T_ENV) - min(rs.counties.2000$T_ENV))

########################################### Linear Correlation ################################################

cor(rs.counties.2000$RDPC,rs.counties.2000$IDHM)
cor(rs.counties.2000$RDPC,rs.counties.2000$T_SUPER25M)
cor(rs.counties.2000$RDPC,rs.counties.2000$TRABPUB)
cor(rs.counties.2000$RDPC,rs.counties.2000$PEA18M)
cor(rs.counties.2000$RDPC,rs.counties.2000$T_ENV)

############################################## Normality Test #################################################

format(rs.counties.2000$IDHM,scientific=FALSE)
shapiro.test(rs.counties.2000$IDHM)
format(rs.counties.2000$T_SUPER25M,scientific=FALSE)
shapiro.test(rs.counties.2000$T_SUPER25M)
shapiro.test(rs.counties.2000$TRABPUB)
shapiro.test(rs.counties.2000$PEA18M)
shapiro.test(rs.counties.2000$T_ENV)

########################################### Linear Regression #################################################

coeficients.counties.2000 <- lm(
  rs.counties.2000$RDPC 
  ~ rs.counties.2000$IDHM 
  + rs.counties.2000$PEA18M 
  + rs.counties.2000$TRABPUB
  + rs.counties.2000$T_SUPER25M 
  + rs.counties.2000$T_ENV
)

########################## Fitting linear regression coeficients into RS counties #############################

predict.rs.counties.2010 <- data.frame(rs.counties.2010$Municipio, rs.counties.2010$Codmun7, rs.counties.2010$RDPC, fitted(coeficients.counties.2000))

###################### Taking counties that compound Porto Alegre's metropolitan region #######################

predict.poa.metro.2010 <- subset.data.frame(predict.rs.counties.2010, 
                                              rs.counties.2010$Codmun7 == "4300604" |
                                              rs.counties.2010$Codmun7 == "4303103" |
                                              rs.counties.2010$Codmun7 == "4303905" |
                                              rs.counties.2010$Codmun7 == "4304606" |
                                              rs.counties.2010$Codmun7 == "4307609" |
                                              rs.counties.2010$Codmun7 == "4307708" |
                                              rs.counties.2010$Codmun7 == "4309209" |
                                              rs.counties.2010$Codmun7 == "4309308" |
                                              rs.counties.2010$Codmun7 == "4313409" |
                                              rs.counties.2010$Codmun7 == "4314902" |
                                              rs.counties.2010$Codmun7 == "4318705" |
                                              rs.counties.2010$Codmun7 == "4319505" |
                                              rs.counties.2010$Codmun7 == "4319901" |
                                              rs.counties.2010$Codmun7 == "4320008" |
                                              rs.counties.2010$Codmun7 == "4323002" |
                                              rs.counties.2010$Codmun7 == "4306403" |
                                              rs.counties.2010$Codmun7 == "4306767" |
                                              rs.counties.2010$Codmun7 == "4309050" |
                                              rs.counties.2010$Codmun7 == "4310801" |
                                              rs.counties.2010$Codmun7 == "4313060" |
                                              rs.counties.2010$Codmun7 == "4314050" |
                                              rs.counties.2010$Codmun7 == "4314803" |
                                              rs.counties.2010$Codmun7 == "4322004" |
                                              rs.counties.2010$Codmun7 == "4305355" |
                                              rs.counties.2010$Codmun7 == "4300877" |
                                              rs.counties.2010$Codmun7 == "4313375" |
                                              rs.counties.2010$Codmun7 == "4312401" |
                                              rs.counties.2010$Codmun7 == "4321204" |
                                              rs.counties.2010$Codmun7 == "4318408" |
                                              rs.counties.2010$Codmun7 == "4301107" |
                                              rs.counties.2010$Codmun7 == "4317608" |
                                              rs.counties.2010$Codmun7 == "4304689" |
                                              rs.counties.2010$Codmun7 == "4316006" |
                                              rs.counties.2010$Codmun7 == "4310108"
                                            )






