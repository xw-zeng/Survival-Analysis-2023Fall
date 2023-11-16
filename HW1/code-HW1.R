
# Problem 3

r = c(5, 8, 12, 24, 32, 17, 16, 17, 19, 30)
d = c(11, 12, 15, 33, 45, 28, 16, 17, 19, 30)
c_r = c(rep(0, 6), rep(1, 4))
c_d = c(0, 0, 0, 1, 0, rep(1, 5))


data = data.frame('patient' = seq(1, 10), 
                  'relapse' = r, 
                  'relapse_c' = c_r, 
                  'death' = d,
                  'death_c' = c_d)

m1 = sum(1 - data$relapse_c)
k1 = sum(data$relapse_c)
T1 = sum(data$relapse * (1 - data$relapse_c))
U1 = sum(data$relapse * data$relapse_c)

m2 = sum(1 - data$death_c)
k2 = sum(data$death_c)
T2 = sum(data$death * (1 - data$death_c))
U2 = sum(data$death * data$death_c)

data = data[-c(7,8,9,10),]

m3 = sum(1 - data$death_c)
k3 = sum(data$death_c)
T3 = sum(data$death * (1 - data$death_c))
U3 = sum(data$death * data$death_c)

# Problem 4

library(ggplot2)

find_n = function(n){qpois(p = 0.05, lambda = 0.25 * n)}

n_list = seq(135, 180)
y_list = find_n(n_list)

ggplot() +
  geom_line(aes(y = y_list, x = n_list)) +
  geom_point(aes(y = 30, x = n_list[y_list == 30][1]),
             color='red', size = 2) + 
  annotate('text', x = n_list[y_list == 30][1] - 1.2, 
           y = 30.7, label='(159, 30)') + 
  theme_light() +
  labs(x = 'n', y = 'smallest integer s.t. P >= 0.05')

find_n = function(n){pgamma(q = 1, shape = 30, rate = 0.25 * n)}

n_list = seq(135, 180)
p_list = find_n(n_list)

ggplot() +
  geom_line(aes(y = p_list, x = n_list)) +
  geom_point(aes(y = p_list[p_list >= 0.95][1], x = n_list[p_list >= 0.95][1]),
             color = 'red', size = 2) + 
  annotate('text', x = n_list[p_list >= 0.95][1] + 6, 
           y = p_list[p_list >= 0.95][1] - 0.012, label='(159, 0.953)') + 
  theme_light() +
  labs(x = 'n', y = 'smallest integer s.t. P >= 0.95')
