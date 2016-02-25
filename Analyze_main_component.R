# Find the highest component score for each individul

# 1. Read input data   (PCA_results_main_component)
mydf <- read.csv(file="PCA_results_main_component_2013.csv", head=TRUE, sep=",")

# 2. Analyze data
# find the highest value and assciated column name
mydf <- mydf[,2:6]
main_comp <- colnames(mydf)[max.col(mydf,ties.method="first")]


write.csv(main_comp, file = "MyResult2.csv")


