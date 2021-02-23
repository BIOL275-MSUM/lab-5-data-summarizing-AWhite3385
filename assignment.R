#load packages
library(tidyverse)    

#iris data set pakages
iris <- as_tibble(iris) # so it prints a little nicer

# Question 1

iris

i2 <-rename(iris, sepal_length = Sepal.Length,
       sepal_width = Sepal.Width,
       petal_length = Petal.Length,
       petal_width = Petal.Width,
       species = Species)

#question 2

ni2 <-mutate(i2, sepal_length = sepal_length * 10,
       sepal_width = sepal_width * 10,
       petal_length = petal_length * 10,
       petal_width = petal_width * 10)

#question 3

ia1 <-mutate(ni2, sepal_area = sepal_length * sepal_width,
       petal_area = petal_length * petal_width)

select(ia1, petal_area, sepal_area)

#question 4

ques4 <-summarize(ni2, sample_size = n(), 
          max = max(sepal_length),
          min = min(sepal_length),
          range = max - min ,
          median = median(sepal_length),
          q1 = quantile(sepal_length, probs = 0.25),
          q3 = quantile(sepal_length, probs = 0.75),
          iqr = q3-q1)

ques4

#question 5

ni2_grouped <-group_by(ni2, species)

ques5 <-summarize(ni2_grouped, sample_size = n(),
          petal_mean = mean(petal_width),
          st_dev = sd(petal_width),
          width_variance = var(petal_width),
          sem = sd(petal_width) / sqrt(sample_size),
          upper_ci = mean(petal_width) + 1.96 * sem,
          lower_ci = mean(petal_width) - 1.96 * sem)

ques5                           

#question 6

ggplot(data = ni2) +
        geom_jitter(mapping = aes(x = species, y = petal_length))

#question 7

width_summary <-
        summarise(
                ni2_grouped, 
                petal_mean = mean(petal_width),
                sem = sd(petal_width) / sqrt(n()),
                upper_limit =  petal_mean + 1.96 * sem,
                lower_limit =  petal_mean - 1.96 * sem
        )

width_summary
          
ggplot(data = ni2) +
        geom_jitter(mapping = aes(x = species, y = petal_width)) +
        geom_crossbar(
                data = width_summary,
                mapping = aes(
                        x = species,
                        y = petal_mean,
                        ymax = upper_limit,
                        ymin = lower_limit
                ),
                color = "red"
        )

#question 8

ggplot(data = ni2) +
        geom_point(mapping = aes(x = petal_length, y = petal_width, color = species), alpha = 0.5)



