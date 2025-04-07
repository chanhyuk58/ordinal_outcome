library('tidyverse')

a <- 1 + 1
msg <- paste0('This is the test ', a)
print(msg)

write_excel_csv(msg, file='./msg.csv')
