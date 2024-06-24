# Instalar e carregar pacotes necessários
install.packages("ggplot2")
library(ggplot2)


# Visualizar as primeiras linhas e estrutura dos dados
head(DNOPEN22)
str(DNOPEN22)



## 1. Estatística Descritiva

# Pergunta 1: Qual é a distribuição de peso dos recém-nascidos por sexo?

DNOPEN22 <- DNOPEN22[!is.na(DNOPEN22$PESO) & !is.na(DNOPEN22$SEXO), ]

DNOPEN22$PESO <- as.numeric(DNOPEN22$PESO)

summary(DNOPEN22$PESO)
summary(DNOPEN22[DNOPEN22$SEXO == 1, ]$PESO)# Masculino
summary(DNOPEN22[DNOPEN22$SEXO == 2, ]$PESO)# Feminino
summary(DNOPEN22[DNOPEN22$SEXO == 0, ]$PESO)# Ignorado

ggplot(DNOPEN22, aes(x=factor(SEXO), y=as.numeric(PESO))) +
  geom_boxplot() +
  xlab("Sexo (1=Masculino, 2=Feminino, 0=Ignorado)") +
  ylab("Peso (g)") +
  ggtitle("Boxplot do Peso dos Recém-nascidos por Sexo")



# Pergunta 2: Qual é a distribuição de idade das mães por estado civil?

DNOPEN22 <- DNOPEN22[!is.na(DNOPEN22$IDADEMAE) & !is.na(DNOPEN22$ESTCIVMAE), ]

summary(DNOPEN22$IDADEMAE)
summary(DNOPEN22[DNOPEN22$ESTCIVMAE == 1, ]$IDADEMAE)  # Solteira
summary(DNOPEN22[DNOPEN22$ESTCIVMAE == 2, ]$IDADEMAE)  # Casada
summary(DNOPEN22[DNOPEN22$ESTCIVMAE == 3, ]$IDADEMAE)  # Viúva
summary(DNOPEN22[DNOPEN22$ESTCIVMAE == 4, ]$IDADEMAE)  # Divorciada
summary(DNOPEN22[DNOPEN22$ESTCIVMAE == 5, ]$IDADEMAE)  # União estável
summary(DNOPEN22[DNOPEN22$ESTCIVMAE == 9, ]$IDADEMAE)  # Ignorada

ggplot(DNOPEN22, aes(x=factor(ESTCIVMAE), y=as.numeric(IDADEMAE))) +
  geom_boxplot() +
  xlab("Estado Civil (1=Solteira, 2=Casada, 3=Viúva, 4=Divorciada, 5=União estável, 9=Ignorada)") +
  ylab("Idade da Mãe") +
  ggtitle("Boxplot da Idade das Mães por Estado Civil")




## 2. Probabilidade

# Pergunta 3: Qual a probabilidade de um recém-nascido ter baixo peso ao nascer (< 2500g) considerando o tipo de parto (normal ou cesárea)?
baixo_peso_normal <- nrow(DNOPEN22[DNOPEN22$PESO < 2500 & DNOPEN22$PARTO == 1, ]) / nrow(DNOPEN22[DNOPEN22$PARTO == 1, ])
baixo_peso_cesarea <- nrow(DNOPEN22[DNOPEN22$PESO < 2500 & DNOPEN22$PARTO == 2, ]) / nrow(DNOPEN22[DNOPEN22$PARTO == 2, ])

baixo_peso_normal
baixo_peso_cesarea



# Pergunta 4: Qual a probabilidade de um recém-nascido nascer com anomalia considerando a idade da mãe?
DNOPEN22$GRUPO_ETARIO <- cut(as.numeric(DNOPEN22$IDADEMAE), breaks=c(0, 19, 29, 39, 49, 59), labels=c("0-19", "20-29", "30-39", "40-49", "50-59"))
anomalia_prob <- prop.table(table(DNOPEN22$GRUPO_ETARIO, DNOPEN22$IDANOMAL), 1)
anomalia_prob



## 3. Inferência

# Pergunta 5: Existe diferença significativa entre o número médio de consultas pré-natais realizadas por mães de diferentes grupos etários?
DNOPEN22$GRUPO_ETARIO <- cut(as.numeric(DNOPEN22$IDADEMAE), breaks=c(0, 19, 29, 39, 49), labels=c("0-19", "20-29", "30-39", "40-49"))

DNOPEN22 <- DNOPEN22[!is.na(DNOPEN22$IDADEMAE) & !is.na(DNOPEN22$CONSULTAS), ]

anova_result <- aov(as.numeric(CONSULTAS) ~ GRUPO_ETARIO, data=DNOPEN22)
summary(anova_result)

ggplot(DNOPEN22, aes(x=GRUPO_ETARIO, y=as.numeric(CONSULTAS))) +
  geom_boxplot() +
  xlab("Grupo Etário") +
  ylab("Número de Consultas Pré-natais") +
  ggtitle("Boxplot do Número de Consultas Pré-natais por Grupo Etário")



# Pergunta 6: Existe diferença significativa no peso dos recém-nascidos entre diferentes faixas de idade gestacional?
DNOPEN22$GRUPO_GESTACIONAL <- cut(as.numeric(DNOPEN22$SEMAGESTAC), breaks=c(0, 27, 37, 42), labels=c("Pré-termo", "Termo", "Pós-termo"))

DNOPEN22 <- DNOPEN22[!is.na(DNOPEN22$PESO) & !is.na(DNOPEN22$SEMAGESTAC), ]

anova_result <- aov(as.numeric(PESO) ~ GRUPO_GESTACIONAL, data=DNOPEN22)
summary(anova_result)

ggplot(DNOPEN22, aes(x=GRUPO_GESTACIONAL, y=as.numeric(PESO))) +
  geom_boxplot() +
  xlab("Grupo Gestacional (Pré-termo, Termo, Pós-termo)") +
  ylab("Peso (g)") +
  ggtitle("Boxplot do Peso dos Recém-nascidos por Grupo Gestacional")