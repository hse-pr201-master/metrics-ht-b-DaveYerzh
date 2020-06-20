# Ержанов Давид БЭК171 


library("tidyverse") # манипуляции
library("rio") # импорт данных
library("lmtest") # тесты для линейных моделей
library("skimr") # описательные статистики
library("mfx") # предельные эффекты
library("ivpack") # IV
library("texreg") # Таблички сравнения моделей
library("ggplot2") # графики
library("sandwich") # оценка Var для гетероскедастичности
library("bstats") # тест Уайта, тест Бройша-Пагана
library("lmtest") # тест Бройша-Пагана
library("dplyr")
library("tidyr")
library("readxl")
library("knitr")
library("glmnet")
library("MASS")
library("quantreg")
library("fable")
library("tsibble")
library("feasts")
library("car")
library("zoo")
library("xts")
library("forecast")
library("lubridate")

getwd()
setwd("/Users/david/desktop/homework")
data <- read.table(file = "country_profile_variables.csv", header = TRUE, sep = ",")
glimpse(data)
head(data)
summary(data)
view(data)



# 1.1
# Мне интересно, как показатели населения (плотность населения, соотношение числа мужчин и женщин, доля городского населения)
# влияют на их благосостояние (ВВП на душу населения)
# Поэтому зависимой переменной я возьму ВВП на душу населения, вернее его логарифм

data$GDP.per.capita..current.US..[data$GDP.per.capita..current.US.. == -99] <- NA
table(data$GDP.per.capita..current.US..)
GDP <- na.omit(data$GDP.per.capita..current.US..)
logGDP <- log(data$GDP.per.capita..current.US..)
hist(GDP)
hist(logGDP)
boxplot(GDP)


#_________________________________________________________________

# 1.2, 1.3
# a) Непрерывная переменная: доля городского населения
# Доля городского населения может влиять на ВВП, так как это говорит об урбанизации
PopUrban <- data$Urban.population....of.total.population.
hist(PopUrban)
boxplot(PopUrban)
table(PopUrban)
# Выбросы есть, однако они входят в выборку неслучайно, поэтому избавляться от них не стоит

# b) Бинарная переменная: если мужчин больше, чем женщин, то 1, наоборот - 0
# Интересна гипотеза, вдруг количество мужчин коррелирует с ВВП
data$Sex.ratio..m.per.100.f..2017.[data$Sex.ratio..m.per.100.f..2017. == -99] <- NA
Gender <- data$Sex.ratio..m.per.100.f..2017.
Gender
Gender[is.na(Gender)] <- mean(Gender, na.rm = T)
Gender
boxplot(Gender)
table(Gender)
# У нас есть выбросы, в особенности 4 сильных выброса, начиная с 168.3, однако поскольку я собираюсь сделать эту переменную бинарной, то не вижу смысле их удалять

MoreMan <- ifelse(Gender > 100, 1, 0)
table(MoreMan)
MoreMan.f <- factor(MoreMan)

# c) Нелинейная переменная: логарифм плотности населения
# плотность населения моэет положительно коррелировать с ВВП, так как это может привести к увеличению производства
logDensity <- log(data$Population.density..per.km2..2017.)
hist(data$Population.density..per.km2..2017.) # ситуация с выбросами такая же, как и в переменной по доле городского населения. По тем же причинам, избавляться от них не буду
hist(logDensity)


# d) 

#_________________________________________________________________

# 1.4

model <- lm(data = data, logGDP~PopUrban + MoreMan.f + logDensity) #  МНК
summary(model)

# a) Мультиколлинеарность - из-за неё отсутствуют теоретические МНК-оценки
vif(model) # мультиколлинеарность не наблюдается
mk <- model.matrix(data=data, logGDP~0 + PopUrban + MoreMan.f + logDensity)
head(mk)
cor(mk) # показатели корреляции очень малы

# b) Гетероскедастичность - из-за неё растут стандартные отклонения => неэффективные оценки, но остаются состоятельными

# Тест Бройша-Пагана
bptest(model)
# P-value очень низкий, поэтому мы отвергаем H0 => есть гетероскедастичность

# Голдфельд-Квант
gqtest(model,fraction=0.4)
# P-value большой, H0 не отвергается => нет гетероскедастичности

# Эндогенность - из-за неё оценки смещённые и несостоятельные




#_________________________________________________________________

# 1.5
# a) МНК

model <- lm(data = data, logGDP~PopUrban + MoreMan.f + logDensity)
summary(model)


#_________________________________________________________________


# 2.1

# 1
# AR(1)
y1 <- arima.sim(n=120, list(ar=0.8))
ggtsdisplay(y1)
# Данный процесс является стационарным, так как все AR(1) процессы с коэффициентом перед лаговой переменной по модулю меньши единицы являются стационарными.

# 2
# AR(3)
y2 <- arima.sim(n=120, list(ar = c(0.1, 0.2, 0.3)))
ggtsdisplay(y2)
# Есть стационарное решение, так как единственный корень характеристического многочлена по модулю меньше, чем 1.

# 3
# MA(2)
y3 <- arima.sim(n = 120, list(ma = c(1.2, 2)))
plot(y3)
ggtsdisplay(y3)
# MА процессы всегда являются стационарными

#_________________________________________________________________

# 2.2

#ARIMA(0, 1, 2)
mod_1 <- arima.sim(n = 120, list(c(0,1,2)))
ggtsdisplay(mod_1)

#ARIMA(0, 0, 0)
mod_2 <- arima.sim(n = 120, list(c(0,0,0)))
ggtsdisplay(mod_2)

#ARIMA(3, 0, 0)
mod_3 <- arima.sim(n = 120, list(c(3,0,0)))
ggtsdisplay(mod_3)

#_________________________________________________________________

# 2.3

# y_t = y_(t-1) + u_t
mod_4 <- arima.sim(n = 100, list(order=c(0,1,0)))
ggtsdisplay(mod_4) # Это уравнение не имеет стационарного решения, ему соответствуют постепенно убывающие автокорреляции

#_________________________________________________________________

# 2.4

ggtsdisplay(y1)
ggtsdisplay(mod_4)
# ACF различаются тем, что у y1 (стац) он убывает гораздо быстрее, чем у mod_4 (нестац)
# PACF почти не различаются, так как первый значим, остальные нет, если не считать частную автокорреляцию на нескольких шагах для y1, так как иногда они являются значимы и не на первом шаге

#_________________________________________________________________

# 2.5

# a)

n <- arima.sim(n=120, list(c(2,0,3)))
length(n)
# b)

train <- head(n, 100)
train
length(train)

test <- tail(n, 20)
test
length(test)

# c)

mod_5 <- Arima(train, order = c(2,0,3))
summary(mod_5)
length(mod_5)
# d)

Prognoz <- forecast(mod_5, h = 20, level = 95)
summary(Prognoz)
length(Prognoz)

# e)





