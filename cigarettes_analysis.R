
#   In 1970, a national insurance organisation wanted to study the consumption pattern of cigarettes in all
#   50 states. Variables in this data are:
#       - Age       Median age of a person living in a state.
#       - HS        Percentage of people over 25 years of age in a state who had completed high school.
#       - Income    Per capita personal income for a state (in USD).
#       - Black     Percentage of black people living in a state.
#       - Female    Percentage of females living in a state.
#       - Price     Weighted average price (in cents) of a pack of cigarettes in a state.
#       - Sales     Number of packs of cigarettes sold in a state on a per capita basis.
#
#   Source: http://www.ats.ucla.edu/stat/examples/chp/p081.txt
#
# Objective: Find best predictor for sales

library(ggplot2)

x <- read.csv('D:\\DataScience\\lesson2\\cigarettes.txt', sep='\t', header=T)

pairs(x)
