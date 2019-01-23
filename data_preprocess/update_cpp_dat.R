library(tidyverse)

load(file = "data/cpp.rda")

# removed z calculations as the package is demonstrating these.
cpp <- cpp %>%
        select(-waz, -haz, -baz, -whz)

usethis::use_data(cpp, overwrite = TRUE)        

## check whz values 
