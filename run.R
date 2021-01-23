##################################################################################################
# CALCULANDO AS MEDIDAS DE AVALIAÇÃO DE DESEMPENHO MULTIRRÓTULO NA MÃO                           #
# Elaine Cecilia Gatto                                                                           #
# Script 2 - Run                                                                                 #
##################################################################################################

##################################################################################################
# Configures the workspace according to the operating system                                     #
##################################################################################################
sistema = c(Sys.info())
FolderRoot = ""
if (sistema[1] == "Linux"){
  FolderRoot = paste("/home/", sistema[7], "/multilabel_performance_evaluation_measures", sep="")
  setwd(FolderRoot)
} else {
  FolderRoot = paste("C:/Users/", sistema[7], "/multilabel_performance_evaluation_measures", sep="")
  setwd(FolderRoot)
}

setwd(FolderRoot)
source("functions_measures_multilabel.R")

##################################################################################################
# Configures Scientific Notation                                                                #
##################################################################################################
cat("\nSet Global Scientific Notation\n")
options(scipen=30)

##################################################################################################
# Opens the file "datasets.csv"                                                                  #
##################################################################################################
cat("\nOpen Dataset Infomation File\n")
setwd(FolderRoot)
datasets = data.frame(read.csv("datasets.csv"))
n = nrow(datasets)
cat("\nTotal of Datasets: ", n, "\n")

# Open files with true and predicts labels
true_labels = read.csv("true_labels.csv")
pred_labels = read.csv("pred_labels.csv")
bipartition = data.frame(true_labels, pred_labels)

# consult the datasets file with contains values about the datasets
i = 9
ds = datasets[i,]
dataset_name = toString(ds$Name)
number_labels = ds$Labels
number_instances = ds$Instances
names_labels = colnames(true_labels)

# confusion matrix
mc = matrix_confusion(bipartition, number_labels, names_labels, number_instances)

# compute measures
evaluated = results(mc)



