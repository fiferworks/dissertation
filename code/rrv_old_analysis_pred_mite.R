#importing data
rose <-
  readxl::read_excel('pred_mites/pred_mite_olfactometer.xlsx', sheet = 'rose_experiments')

#converting choice to a factor for calculations
rose$choice <- forcats::as_factor(rose$choice)
rose$rose_label <- as.factor(rose$rose_label)
rose$RRV <- forcats::as_factor(rose$RRV)

rose$value <- rose$choice
rose$value <- sub('control', '-1', rose$value)
rose$value <- sub('experiment', '1', rose$value)
rose$value <- sub('no choice', '0', rose$value)
rose$value <- as.numeric(rose$value)

#table of results from choice tests
rosults <- table(rose$rose_label, rose$choice)

#number of observations per rose
rowSums(rosults)

#total choices for all experiments
colSums(rosults)

#porportion tables (percent)
prop.table(table(rose$rose_label, rose$choice)) * 100

#testing to see if the rose number and olfactometer choices are independent:
chisq.test(rose$rose_label, rose$choice)

#filtering out the non-choices for the graph
rgraph <- dplyr::filter(rose, choice != 'no choice')

#linear model to compare variables
#lme4::glmer(choice ~ rose_label + time_sec +(1 | date), data = rose, family = 'poisson', verbose = 1)

rosults

colSums(rosults)

ggplot2::ggplot(data = rgraph, ggplot2::aes(x = rose_label, y = value, fill = choice)) + ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::scale_fill_manual(values = wes_palette(
  name = 'Moonrise3',
  n = 2,
  type = c('discrete')
))