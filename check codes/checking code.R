
#import dummy dataset 
checks <- read_excel("check codes/checks.xlsx")
View(checks)

# Filter this to check if it works
check1 <- checks %>% filter(anthonia == c('manchester', 'trafford', 'bury'))
view(check1)
