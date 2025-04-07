library('tidyverse')
path <- getwd()

a <- 1 + 1
msg_df <- data.frame()
msg <- paste0('This is the test ', a)
print(msg)
msg_df <- rbind(msg_df, msg)

write_excel_csv(msg_df, file='msg.csv')
write_excel_csv(msg_df, file=paste0(path, '/data/msg.csv'))
