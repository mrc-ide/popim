library("devtools")
library("roxygen2")

load_all()

pop <- setup_population(2000, 2010, 0, 10)

plot_population(pop)

pop <- apply_vacc(pop, 2002, 2, 5, 0.5, skew = 0)
pop <- apply_vacc(pop, 2000, 0, 0, 0.5, skew = 0)
plot_population(pop)


