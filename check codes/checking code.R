
#import dummy dataset 
checks <- read_excel("check codes/checks.xlsx")
View(checks)

filtervector <- c('manchester', 'trafford', 'bury')

# Filter this to check if it works
check1 <- checks %>% 
  filter(anthonia %in% filtervector)
view(check1)
