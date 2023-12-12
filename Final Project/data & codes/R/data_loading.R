# Load the data
data <- read.csv("df_final.csv")
head(data)

# wage
data$wghist = as.factor(data$wghist)
data$wghist = relevel(data$wghist, ref = 'never')

# employment stability
data$employ = as.factor(data$employ)
data$employ = relevel(data$employ, ref = 'stable')

# race
data$race = as.factor(data$race)
data$race = relevel(data$race, ref = 'nhwhite')

# gender
data$gender[data$gender == '1.male'] = 'male'
data$gender[data$gender == '2.female'] = 'female'
data$gender = as.factor(data$gender)
data$gender = relevel(data$gender, ref = 'female')

# education
data$edu = as.factor(data$edu)
data$edu = relevel(data$edu, ref = '<=12years')

# parental education
data$pedu = as.factor(data$pedu)
data$pedu = relevel(data$pedu, ref = '<=12years')

# religion
data$religion[data$religion == '1.protestant'] = 'protestant'
data$religion[data$religion == '2.catholic'] = 'catholic'
data$religion[data$religion == '3.jewish'] = 'jewish'
data$religion[data$religion == '4.none/no pref'] = 'none'
data$religion[data$religion == '5.other'] = 'other'
data$religion = as.factor(data$religion)
data$religion = relevel(data$religion, ref = 'none')

# living area
data$live[data$live == '1.northeast'] = 'northeast'
data$live[data$live == '2.midwest'] = 'midwest'
data$live[data$live == '3.south'] = 'south'
data$live[data$live == '4.west'] = 'west'
data$live[data$live == '5.move'] = 'move'
data$live = as.factor(data$live)
data$live = relevel(data$live, ref = 'northeast')

# marriage
data$marriage = as.factor(data$marriage)
data$marriage = relevel(data$marriage, ref = 'married')

# Insurance from government
data$insur_gov = as.factor(data$insur_gov)
data$insur_gov = relevel(data$insur_gov, ref = 'never')

# Insurance from employer
data$insur_com = as.factor(data$insur_com)
data$insur_com = relevel(data$insur_com, ref = 'never')

# self-reported health
data$self_health = as.factor(data$self_health)
data$self_health = relevel(data$self_health, ref = 'good/very good/excellent')

# smoke
data$smoke = as.factor(data$smoke)
data$smoke = relevel(data$smoke, ref = 'never')

# summary
data = data[data$age >= 60 & data$age <= 78, ]
summary(data)
