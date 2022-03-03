############################## Replicação de Artigo ############################

# Value-Added Taxes, Chain Effects, and Informality - de Paula and Scheinkman
# Daniel H. A. Reis

################################################################################
library(tidyverse)
library(readr)
library(haven)
library(archive)
library(dplyr)
library(googledrive)
library(bigmemory)
library(biganalytics)
library(bigtabulate)
library(plm)
library(fastDummies)
library(recipes)
library(lmtest)
library(sandwich)
library(skimr)
library(psych)
library(foreign)
library(miceadds)
library(foreign)
library(AER)
library(stargazer)
library(ivprobit)

rm(list=ls())

#### Importando Dados

#Dados sobre fiscalização
fisc = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/fisc.dta")

#Dados sobre os coeficientes do SCN
techcoef2003 = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/techcoef2003.dta")

#Dados sobre a proporção de vendas por categorias das firmas de acordo com CNAE
sales2003 = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/sales2003.dta")

#Dados sobre SCN para cada CNAE
cnaescn = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/cnaescn.dta")

#Dados Econômicos da Pesquisa de Economia Informal Urbana ECINF 2003
ecinf2003 = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/ecinf2003.dta")
 
#Dados Domicílios - Variáveis das famílias
domicilios_modified01 = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/domicilios_modified01.dta")

#Dados IndProp - Variáveis dos empreendedores
indprop_modified01 = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/indprop_modified01.dta")

#Dados Pessoas Ocupadas
pesocup_modified01 = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/pesocup_modified01.dta")

#Dados Sebrae
sebrae_modified01 = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/sebrae_modified01.dta")

#Dados Clientes são formais
clform = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/clform.dta")

#Dados Fornecedores são formais
supform = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/supform.dta")

#Dados Educação
educurbsec = read_dta(file = "G:/Meu Drive/Teoria Econômica - Doutorado/8º Bimestre/Economia do Setor Público/Replicação/Dados/educurbsec.dta")


#### Tratamento dos Dados

#Excluindo Empreendedores que são Agricultura, Não declaradas ou ignoradas:
ecinf2003 <- ecinf2003 %>%  filter(v4301>9999 & v4301<99999)

#Juntando as Bases de dados
df <- ecinf2003 %>% full_join(indprop_modified01)

df <- df %>% full_join(pesocup_modified01)

df <- df %>% full_join(sebrae_modified01)

df <- df %>% merge(domicilios_modified01)

df <- df %>% full_join(cnaescn)

df <- df %>% full_join(sales2003)

df <- df %>% full_join(techcoef2003)

df <- df %>% full_join(fisc)


#Verificando se restaram apenas Empreendedores na amostra
df %>% count(v4349a, v4349g)

# Removendo indivíduos com menos de 15 anos da amostra

df <- df %>% filter(v4349d>=15)

#Verificando quem são os clientes dos empreendedores
df %>% count(v4320)

#Verificando 'Possui constituição juridica?'
df %>% count(v4338)

#Criando Dummies

df$constjur <- ifelse(df$v4338 == 1, 1, 0) #const jur legal

df %>% count(constjur)

df$taxreg <- ifelse(df$v4340 == 1, 1, 0) #tax reg
df <- df %>% replace_na(list(taxreg = 0))
df %>% count(taxreg)


df$outsidehouse <- ifelse(df$v4302 == 3, 1, 0) #Local que desenvolve as atividades
df %>% count(outsidehouse)

df$largecl <- ifelse(df$v4320 == 3, 1, 0) #clientes grandes
df %>% count(largecl)

df$smallcl <- ifelse(df$v4320 == 5, 1, 0) #clientes pequenos
df %>% count(smallcl)

# Exluindo firmas cujo o maior cliente é o governo
df <- df %>% filter(v4320 != 7)


#Verificando

df %>%
  group_by(v5902) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)),
         percent = round((n / sum(n)), n),
         cumpercent = round(cumsum(freq = n / sum(n)), n))

df %>%
  group_by(v01) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)),
         percent = round((n / sum(n)), n),
         cumpercent = round(cumsum(freq = n / sum(n)), n))


#Criando novas variáveis

df <- df %>% mutate(n_employee = ifelse(
  df$v4349 ==99 , NA, df$v4349))


df$logn_employee = log(df$n_employee)

df$contpropria <- ifelse(df$v4349 == 1, 1, 0)

df$salario = df$v4327v_3/1000/df$v4349

df <- df %>% mutate(salario = ifelse(
  df$v4349 ==99 | df$v4327v_3==9999999, NA, df$v4327v_3/1000/df$v4349))

df$logsalario = log(df$salario)

df <- df %>% mutate(revenue = ifelse(
  df$v5903==9999999 , NA, df$v5903/1000))

df$logrevenue = log(df$revenue)

df <- df %>% mutate(despesa = ifelse(
  df$v5904==9999999 , NA, df$v5904/1000))

df$profit = df$revenue - df$despesa

df$bankloan <- ifelse(df$v4331 == 3 | df$v4331 == 5, 1, 0)

df$otherjob_old <- ifelse(df$v4413==2, 1, 0)

df <- df %>% mutate(otherjob = ifelse(
  numquest > 1 , 1, otherjob_old))

df$education <- ifelse(df$v4349e==9, 1, df$v4349e)

df$gender <- ifelse(df$v4349c==1, 1, 0)

df <- df %>% mutate(age = ifelse(
  df$v4349d==99 , NA, v4349d))

df$age2 = df$age^2

df$inst <- ifelse(df$v5905 == 9999999, NA, df$v5905)

df$loginst = log(df$inst)


df <- df %>% mutate(loginv = ifelse(v5906 == 9999999, NA, log(v5906)))


df$loginstperemp = df$loginst-log(df$n_employee)

df <- df %>% mutate(loginvperemp = 
                      ifelse(v5906 == 9999999, NA, loginv - log(n_employee)))

df$homeown <- ifelse(df$v1203==1, 1, 0)

df <- df %>% mutate(numroom = 
                      ifelse(v1202==99, NA, v1202))

df$homeown_numroom = df$homeown*df$numroom

df <- df %>% mutate(nearestbank  = 
                      ifelse(v4909==5 | v4909==9, NA, 
                             v4919*24 + v4929 + v4939/60)*(3-v4909)/2)

df$tempoprop = df$v4403a + df$v4403m/12



## Adcionando novas bases

df <- df %>% full_join(clform)

df <- df %>% full_join(supform)

df <- df %>% full_join(educurbsec)


## Adcionando novas variáveis

#Dummy se o setor tem substituição tributária
df$taxsub <- ifelse(df$v4301==34001 | df$v4301==34002 | df$v4301==35010 |
                      df$v4301==35020 | df$v4301==35030 | df$v4301==35090 | 
                      df$v4301==25010 | 
                      df$v4301==15050 | df$v4301==53030 | df$v4301==16000 | 
                      df$v4301==50010 | 
                      df$v4301==50030 | df$v4301==50040 | df$v4301==50050 | 
                      df$v4301==53065 | 
                      df$v4301==23010 | df$v4301==23020 | df$v4301==50020, 1, 0)


df <- dummy_cols(df, select_columns = c("v01", "v5902"),
                        remove_first_dummy = TRUE)

df$taxsub_largecl = df$taxsub*df$largecl

df$taxsub_smallcl = df$taxsub*df$smallcl

df$taxsub_clcgc = df$taxsub*df$clcgc

df$taxsub_compcgc = df$taxsub*df$compcgc


#### Tabela 1 - Estatísticas descritivas

sum_stat <- df %>% select(taxreg, taxsub, largecl, smallcl, 
                       outsidehouse, n_employee, revenue, 
                       otherjob, bankloan, education, age, 
                       gender, homeown_numroom, sup_fisc_po,  
                       cl_fisc_po, logsalario, compcgc, clcgc) %>%
  psych::describe(quant = c(.25, .75)) %>%
  as_tibble(rownames = "rowname") %>%
  print()

sum_stat_2 <- sum_stat %>%
  select(var = rowname, n, mean, sd, min, max) %>%
  print()



#### Tabela 03 - Setor Econômico (ECINF)

df %>%
  group_by(v03) %>%
  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)),
         percent = round((n / sum(n)), n),
         cumpercent = round(cumsum(freq = n / sum(n)), n))



#### Tabela 04 - Probit e Efeitos Marginais


tab4 <- glm(taxreg ~ v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                   v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                   v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                   v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                   v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                   v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                   outsidehouse + n_employee + revenue + bankloan + 
                   otherjob + education + age + age2 + gender + 
                   homeown_numroom,
                 family = binomial(link = probit), data = df
                 )

summary(tab4)



sandwich1 <- function(object,...) sandwich(object) * nobs(object)/(nobs(object) - 1)

coeftest(tab4,vcov = sandwich1)


tab4_cluster <- coeftest(tab4, vcovCL, type = 'HC0', 
                              cluster = ~ v02)

summary(tab4_cluster)

stargazer(tab4, tab4_cluster, type = "text", keep.stat = "n") # Comparar

#Efeitos Marginais

xb.prob <- predict(tab4)

#APE Factors

factor.prob <- mean(dnorm(xb.prob))

cbind(factor.prob)

APE.prob4 <- coef(tab4)*factor.prob

cbind(APE.prob4)



#### Tabela 5 - Probit - Efeito de Cadeias

#Estimação 1

tab5_1 <- glm(taxreg ~  largecl + smallcl + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                   v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                   v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                   v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                   v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                   v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                   outsidehouse + n_employee + revenue + bankloan + 
                   otherjob + education + age + age2 + gender + 
                   homeown_numroom,
                 family = binomial(link = probit), data = df
)

summary(tab5_1)

stargazer(tab5_1, type = "text", keep.stat = "n") # Comparar

#Efeitos Marginais

xb.prob5_1 <- predict(tab5_1)

#APE Factors

factor.prob5_1 <- mean(dnorm(xb.prob5_1))

cbind(factor.prob5_1)

APE.prob5_1 <- coef(probitres5_1)*factor.prob5_1

cbind(APE.prob5_1)

#Estimação 2

tab5_2 <- glm(taxreg ~  compcgc + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom,
                    family = binomial(link = probit), data = df
)

summary(tab5_2)

stargazer(tab5_2, type = "text", keep.stat = "n") # Comparar


#Efeitos Marginais

xb.prob5_2 <- predict(tab5_2)

#APE Factors

factor.prob5_2 <- mean(dnorm(xb.prob5_2))

cbind(factor.prob5_2)

APE.prob5_2 <- coef(tab5_2)*factor.prob5_2

cbind(APE.prob5_2)


#Estimação 3

tab5_3 <- glm(taxreg ~  clcgc + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom,
                    family = binomial(link = probit), data = df
)

summary(tab5_3)

stargazer(tab5_3, type = "text", keep.stat = "n") # Comparar


#Efeitos Marginais

xb.prob5_3 <- predict(tab5_3)

#APE Factors

factor.prob5_3 <- mean(dnorm(xb.prob5_3))

cbind(factor.prob5_3)

APE.prob5_3 <- coef(tab5_3)*factor.prob5_3

cbind(APE.prob5_3)


#Estimação 4

tab5_4 <- glm(taxreg ~  largecl + smallcl + compcgc + clcgc + v01_12 + 
                      v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom,
                    family = binomial(link = probit), data = df
)

summary(tab5_4)

stargazer(tab5_4, type = "text", keep.stat = "n") # Comparar


#Efeitos Marginais

xb.prob5_4 <- predict(tab5_4)

#APE Factors

factor.prob5_4 <- mean(dnorm(xb.prob5_4))

cbind(factor.prob5_4)

APE.prob5_4 <- coef(tab5_4)*factor.prob5_4

cbind(APE.prob5_4)



#### Tabela 6 - IV Probit (Efeito de Cadeia)

# Estimação 1 - Sem IV

tab6_1 <- glm(taxreg ~  v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom + largecl,
                    family = binomial(link = probit), data = df
)

summary(tab6_1)

stargazer(tab6_1, type = "text", keep.stat = "n") # Comparar

# Estimação IV 

tab6_2 <- ivprobit(taxreg ~ v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                    v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                    v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                    v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                    v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                    v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                    outsidehouse + n_employee + revenue + bankloan + 
                    otherjob + education + age + age2 + gender + 
                    homeown_numroom |largecl|educurbsec + nearestbank + 
                    v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                    v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                    v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                    v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                    v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                    v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                    outsidehouse + n_employee + revenue + bankloan + 
                    otherjob + education + age + age2 + gender + 
                    homeown_numroom, df)

summary(tab6_2)




# Estimação de 1º estágio

tab6_3 <- glm(largecl ~ educurbsec + nearestbank + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom, family = binomial(link = probit), data = df
)

summary(tab6_3)

stargazer(tab6_3, type = "text", keep.stat = "n") # Comparar


#### Tabela 7 - Probit - Fiscalização (Enforcement)

#Estimação 1

tab7_1 <- glm(taxreg ~ sup_fisc_po + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom, 
                    family = binomial(link = probit), data = df)


summary(tab7_1)

stargazer(tab7_1, type = "text", keep.stat = "n") # Comparar

#Efeitos Marginais

xb.prob7_1 <- predict(tab7_1)

#APE Factors

factor.prob7_1 <- mean(dnorm(xb.prob7_1))

cbind(factor.prob7_1)

APE.prob7_1 <- coef(tab7_1)*factor.prob7_1

cbind(APE.prob7_1)


#Estimação 2

tab7_2 <- glm(taxreg ~ cl_fisc_po + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom, 
                    family = binomial(link = probit), data = df)


summary(tab7_2)

stargazer(tab7_2, type = "text", keep.stat = "n") # Comparar

#Efeitos Marginais

xb.prob7_2 <- predict(tab7_2)

#APE Factors

factor.prob7_2 <- mean(dnorm(xb.prob7_2))

cbind(factor.prob7_2)

APE.prob7_2 <- coef(tab7_2)*factor.prob7_2

cbind(APE.prob7_2)




#### Tabela 11 - Probit - Substituição Tributária


#Estimação 1

tab11_1 <- glm(taxreg ~ largecl + smallcl + taxsub_largecl + 
                      taxsub_smallcl + taxsub + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                      v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                      v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                      v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                      v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                      v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                      outsidehouse + n_employee + revenue + bankloan + 
                      otherjob + education + age + age2 + gender + 
                      homeown_numroom, 
                     family = binomial(link = probit), data = df)


summary(tab11_1)

stargazer(tab11_1, type = "text", keep.stat = "n") # Comparar

#Efeitos Marginais

xb.prob11_1 <- predict(tab11_1)

#APE Factors

factor.prob11_1 <- mean(dnorm(xb.prob11_1))

cbind(factor.prob11_1)

APE.prob11_1 <- coef(tab11_1)*factor.prob11_1

cbind(APE.prob11_1)


# Estimação 2


df2 <- df %>% filter(taxsub == 1)




tab11_2 <- glm(taxreg ~ largecl + smallcl + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                       v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                       v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                       v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                       v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                       v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                       outsidehouse + n_employee + revenue + bankloan + 
                       otherjob + education + age + age2 + gender + 
                       homeown_numroom, 
                     family = binomial(link = probit), data = df2)


summary(tab11_2)

stargazer(tab11_2, type = "text", keep.stat = "n") # Comparar

#Efeitos Marginais

xb.prob11_2 <- predict(tab11_2)

#APE Factors

factor.prob11_2 <- mean(dnorm(xb.prob11_2))

cbind(factor.prob11_2)

APE.prob11_2 <- coef(tab11_2)*factor.prob11_2

cbind(APE.prob11_2)





#### Teste de Robustez - Conta Prórpria



teste1 <- glm(taxreg ~ contpropria + largecl + smallcl + compcgc + clcgc + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                outsidehouse  + revenue + bankloan + 
                otherjob + education + age + age2 + gender + 
                homeown_numroom,
              family = binomial(link = probit), data = df
)

summary(teste)

stargazer(teste5, type = "text", keep.stat = "n") # Comparar

df3 <- df %>% filter(contpropria == 1)

teste2 <- glm(taxreg ~ largecl + smallcl + compcgc + clcgc  + v01_12 + v01_13 + v01_14 + v01_15 + v01_16 + 
                v01_17 + v01_21 + v01_22 + v01_23 + v01_24 + v01_25 +
                v01_26 + v01_27 + v01_28 + v01_29 + v01_31 + v01_32 +
                v01_33 + v01_35 + v01_41 + v01_42 + v01_43 + v01_50 +
                v01_51 + v01_52 + v01_53 + v5902_2 + v5902_3 + v5902_4 +
                v5902_5 + v5902_6 + v5902_7 + v5902_8 + v5902_9 + 
                outsidehouse  + revenue + bankloan + 
                otherjob + education + age + age2 + gender + 
                homeown_numroom,
              family = binomial(link = probit), data = df3
)

summary(teste2)

stargazer(teste1, teste2, type = "text", keep.stat = "n") # Comparar


