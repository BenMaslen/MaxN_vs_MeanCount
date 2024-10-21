
#Check long term stationary conditions for specification

library(expm)
t_m_slow <- matrix(c(1-0.00001,0.001,0.00001,1-0.001),nrow=2)
t_m_fast <- matrix(c(1-0.00001*10,0.001*10,0.00001*10,1-0.001*10),nrow=2)

sample_ratio = 0.00001/(0.001+0.00001)

overall_population_mean = 100
starting_vec <- matrix(c(overall_population_mean*(1-sample_ratio),overall_population_mean*sample_ratio),nrow=1)
vid_length = 60*60

starting_vec %*% (t_m_slow %^% vid_length)
starting_vec %*% (t_m_fast %^% vid_length)
#looks good!


#check the variance is consistent for schooling or non-schooling poisson schooling

n_sim = 100000
n_schooling = 5
overall_population_mean = 100
pop_no_school = vector()
pop_school = vector()
pop_disp = 1/1.2
school_disp = (overall_population_mean/n_schooling)/((overall_population_mean/n_schooling)/pop_disp - 1)


for (i in 1:n_sim){
  schools = rpois(rnegbin(1,overall_population_mean/n_schooling,school_disp),n_schooling)
  pop_school[i] = sum(schools)
}

sd(pop_school)
sd(rnegbin(n_sim,overall_population_mean,pop_disp))

mean(pop_school)
mean(rnegbin(n_sim,overall_population_mean,pop_disp))


#looks good!


#check the variance is consistent for schooling or non-schooling negative binomial schooling


n_sim = 100000
n_schooling = 5
school_disp = 1/1.5
overall_population_mean = 100
pop_no_school = vector()
pop_school = vector()
pop_disp = 1/1.2
school_disp = (overall_population_mean/n_schooling)/((overall_population_mean/n_schooling)/pop_disp - 1)
school_disp = (overall_population_mean/n_schooling)/((overall_population_mean/n_schooling)/pop_disp - 1)

school_disp = (school_disp*pop_disp*(overall_population_mean/n_schooling))/(((overall_population_mean/n_schooling)*school_disp) -(pop_disp + pop_disp*school_disp))


for (i in 1:n_sim){
  schools = rnegbin(rnegbin(1,overall_population_mean/n_schooling,school_disp),n_schooling,school_disp)
  pop_school[i] = sum(schools)
}

sd(pop_school)
sd(rnegbin(n_sim,overall_population_mean,pop_disp))

mean(pop_school)
mean(rnegbin(n_sim,overall_population_mean,pop_disp))


#check the variance is consistent for schooling or non-schooling negative binomial sm simple

n_sim = 100000
n_schooling = 3
overall_population_mean = 100
pop_no_school = vector()
pop_school = vector()
pop_disp = 1/1.2
school_disp = (n_schooling*pop_disp)/overall_population_mean


for (i in 1:n_sim){
  schools = rnegbin(overall_population_mean/n_schooling,n_schooling,school_disp)
  pop_school[i] = sum(schools)
}

sd(pop_school)
sd(rnegbin(n_sim,overall_population_mean,pop_disp))

mean(pop_school)
mean(rnegbin(n_sim,overall_population_mean,pop_disp))



