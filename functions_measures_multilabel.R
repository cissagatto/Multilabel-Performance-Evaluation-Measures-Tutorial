##################################################################################################
# CALCULANDO AS MEDIDAS DE AVALIAÇÃO DE DESEMPENHO MULTIRRÓTULO NA MÃO                           #
# Elaine Cecilia Gatto                                                                           #
# Script 1 - Funções                                                                             #
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


matrix_confusion <- function(bipartition, number_labels, names_labels, number_instances){
  
  retorno = list()
  
  # true labels
  cat("\n True Labels")
  true_labels = bipartition[,1:number_labels]
  retorno$true_labels = true_labels
  
  cat("\n Predict Labels")
  # predict labels
  pred_labels = bipartition[,(number_labels+1):(number_labels*2)]
  retorno$pred_labels = pred_labels
  
  cat("\n Positive Instances")
  # positive instances
  positive_instances = apply(bipartition, 2, sum)
  retorno$number_positive_instances = positive_instances
  print(positive_instances)
  
  cat("\n Negative Instances")
  # negative instances
  negative_instances = m - positive_instances
  retorno$number_negative_instances = negative_instances
  print(negative_instances)
  
  cat("\n Number Labels")
  # number labels
  l = number_labels
  retorno$l = l
  
  cat("\n Number Instances")
  # number instances
  m = number_instances
  retorno$m = m
  
  cat("\n Y = 1")
  # calcular Y = 1
  true_1 = ifelse(true_labels==1,1,0)
  retorno$true_1 = true_1
  
  cat("\n Y = 0")
  # calcular Y = 0
  true_0 = ifelse(true_labels==0,1,0)
  retorno$true_0 = true_0
  
  cat("\n ^Y = 1")
  # calcular ^Y = 1
  pred_1 = ifelse(pred_labels==1,1,0)
  retorno$pred_1 = pred_1
  
  cat("\n ^Y = 1")
  # calcular ^Y = 0
  pred_0 = ifelse(pred_labels==0,1,0)
  retorno$pred_0 = pred_0
  
  cat("\n Total Y = 1")
  # total true 1
  total_true_1 = apply(true_1, 2, sum)
  retorno$total_true_1 = total_true_1
  
  cat("\n Total Y = 0")
  # total true 0
  total_true_0 = apply(true_0, 2, sum)
  retorno$total_true_0 = total_true_0
  
  cat("\n Total ^Y = 1")
  # total pred 1
  total_pred_1 = apply(pred_1, 2, sum)
  retorno$total_pred_1 = total_pred_1
  
  cat("\n Total ^Y = 0")
  # total pred 0
  total_pred_0 = apply(pred_0, 2, sum)
  retorno$total_pred_0 = total_pred_0
  
  matriz_totais = cbind(total_true_0,total_true_1, total_pred_0, total_pred_1)
  retorno$totals = matriz_totais
  
  cat("\n TPi")
  # Verdadeiro Positivo: O modelo previu A (1) e a resposta é A (1)
  # calcular TP: ^Y=1 & Y=1 
  TPi  = ifelse((true_1 & true_1),1,0)
  colnames(TPi) = names_labels
  retorno$TPi = TPi
  
  cat("\n TNi")
  # Verdadeiro Negativo: O modelo previu B (0) e a resposta é B (0)
  # calcular TN: ^Y=0 & Y=0
  TNi  = ifelse((true_0 & pred_0),1,0)
  colnames(TNi) = names_labels
  retorno$TNi = TNi
  
  cat("\n FNi")
  # Falso Positivo: O modelo previu A (1) e a resposta é B (0)
  # calcular FP: ^Y=1 & Y=0
  FPi  = ifelse((true_0 & pred_1),1,0)
  colnames(FNi) = names_labels
  retorno$FNi = FNi
  
  cat("\n FPi")
  # Falso Negativo: O modelo previu B (0) e a resposta é A (1)
  # calcular FN: ^Y=0 & Y=1
  FNi  = ifelse((true_1 & pred_0),1,0)
  colnames(FPi) = names_labels
  retorno$FPi = FPi
  
  cat("\n TPl")
  # CALCULANDO OS TOTAIS POR RÓTULOS
  TPl = apply(TPi, 2, sum)
  retorno$TPl = TPl
  
  cat("\n TNl")
  TNl = apply(TNi, 2, sum)
  retorno$TNl = TNl
  
  cat("\n FNl")
  FNl = apply(FNi, 2, sum)
  retorno$FNl = FNl
  
  cat("\n FPl")
  FPl = apply(FPi, 2, sum)
  retorno$FPl = FPl
  
  cat("\n Matriz de Confusão por rótulos")
  # montando a matriz de confusão absoluta
  matriz_confusao_por_rotulos = data.frame(TPl, FPl, TNl, FNl)
  colnames(matriz_confusao_por_rotulos) = c("TP","FP", "TN", "FN")
  retorno$matrix_confusion_absolute = matriz_confusao_por_rotulos
  
  cat("\n Matriz de Confusão Porcentagem")
  # calculando matriz de confusão proporcional
  matriz_confusao_por_rotulo_porcentagem = data.frame(matriz_confusao_por_rotulos/m)
  retorno$matrix_confusion_relative = matriz_confusao_por_rotulo_porcentagem
  
  cat("\n Wrong labels")
  # calculando o total de rótulos classificados errados
  errados = matriz_confusao_por_rotulos$FP + matriz_confusao_por_rotulos$FN
  retorno$wrong = errados
  
  cat("\n Percent Wrong Labels")
  # calculando a porcentagem de rótulos classificados errados
  percent_errados = errados/m
  retorno$percent_wrong = percent_errados
  
  cat("\n Correct Labels")
  # calculando o total de rótulos classificados corretamente
  certos = matriz_confusao_por_rotulos$TP + matriz_confusao_por_rotulos$TN
  retorno$correct = certos
  
  cat("\n Percent Correct Labels")
  # calculando a porcentagem de rótulos classificados corretamente
  percent_certos = certos/m
  retorno$percent_correct = percent_certos
  
  cat("\n Gather matrixes")
  # juntando tudo
  result1 = data.frame(matriz_confusao_por_rotulos, certos, percent_certos, errados, percent_errados)
  retorno$matrix_confusion_complete = result1
  
  cat("\n Total Matrix Absolute")
  # totais da matriz de confusão absoluta
  matriz_confusao_all = data.frame(apply(matriz_confusao_por_rotulos, 2, sum))
  colnames(matriz_confusao_all) = "absolute"
  retorno$matrix_confusion_all = matriz_confusao_all
  
  cat("\n Total Matrix Percent")
  # matriz de confusão proporcional
  matriz_confusao_all_porcentagem = data.frame(matriz_confusao/m)
  colnames(matriz_confusao_all_porcentagem) = "porcentage"
  retorno$matriz_confusao_all_porcentagem = matriz_confusao_all_porcentagem
  
  cat("\n Final Matrix")
  # matriz final
  result2 = data.frame(matriz_confusao, matriz_confusao_porcentagem)
  retorno$matrix_confusion = result2
  
  cat("\n Total Final Matrix - Collumn")
  # total por coluna
  total_per_column = apply(matriz_confusao_por_rotulos, 2, sum)
  retorno$matrix_confusion_totals_column = total_per_column
  
  cat("\n Total Final Matrix - Lines")
  # total por linha
  total_per_line = apply(matriz_confusao_por_rotulos, 1, sum)
  retorno$matrix_confusion_totals_lines = total_per_line
  
  return(retorno)
  
}

accuracy <- function(true_labels, pred_labels, names_labels){
  cat("\n Acurácia")
  # OR
  true_yi_ou_pred_yi = ifelse((true_labels | pred_labels),1,0)
  colnames(true_yi_ou_pred_yi) = names_labels
  
  # somando por instância
  total_1 = apply(true_yi_ou_pred_yi, 1, sum)
  
  # calculando o verdadeiro positivo
  TPi  = ifelse((true_labels & pred_labels),1,0)
  
  # somando por instância
  total_2 = apply(TPi, 1, sum)
  
  # dividir total 1 por total 2
  acuracia = mean(total_2/total_1)
  #acuracia = acuracia %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x), 0, x))
  
  return(acuracia)
  
}

subsetAccuracy <- function(true_labels, pred_labels){
  cat("\n Subset Accuracy")
  sa = mean(ifelse(true_labels==pred_labels,1,0))
  #sa = sa %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x), 0, x))
  return(sa)
}

zerOneLoss <- function(true_labels, pred_labels){
  cat("\n 0/1L")
  zol = mean(ifelse(true_labels!=pred_labels,1,0))
  #zol = zol %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x), 0, x))
  return(zol)
}

hammingLoss <- function(true_labels, pred_labels){
  cat("\n Hamming Loss")
  hl = mean(ifelse((true_labels - pred_labels)|(pred_labels - true_labels),1,0))
  #hl = hl %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x), 0, x))
  return(hl)
}

precision <- function(true_labels, pred_labels){
  cat("\n Precisão")
  res = data.frame(pred_labels / (ifelse(true_labels&pred_labels,1,0)))
  res[mapply(is.infinite, res)] <- 0
  res[mapply(is.nan, res)] <- 0
  precision = sum(apply(res,1,sum))/m
  return(precision)
}

recall <- function(true_labels, pred_labels){
  cat("\n Revocação")
  res = true_labels / (ifelse(true_labels&pred_labels,1,0))
  res = res %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x), 0, x))
  res = res %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
  recall = sum(apply(res,1,sum))/m
  return(recall)
}

f1 <- function(true_labels, pred_labels){
  cat("\n F1")
  res = (2*ifelse(true_labels&pred_labels,1,0)) / (true_labels + pred_labels)
  res <- res %>% mutate_if(is.numeric, function(x) ifelse(is.nan(x), 0, x))
  res <- res %>% mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))
  f1 = sum(res)/m
  return(f1)
}

macroPrecision <- function(TPl, FPl){
  cat("\n Macro-Precisão")
  return((1/l)* sum(TPl/(TPl+FPl)))
}

macroRecall <- function(TPl, FNl){
  cat("\n Macro-Revocação")
  return((1/l)* sum(TPl/(TPl+FNl)))
}

macroF1 <- function(map, mar){
  cat("\n Macro-F1")
  return((2*map*mar)/(map+mar))
}

microPrecision <- function(TPl, FPl){
  cat("\n Micro-Precisão")
  return(sum(TPl)/sum(sum(TPl) + sum(FPl)))
}

microRecall <- function(TPl, FNl){
  cat("\n Micro-Revocação")
  return(sum(TPl)/sum(sum(TPl) + sum(FNl)))
}

microF1 <- function(mip, mir){
  cat("\n Micro-F1")
  return((2*mip*mir)/(mip+mir))
}

clp <- function(TNl, FNl){
  cat("\n CLP")
  return(sum((TNl+FNl)==0)/l)
}

mlp <- function(TPl, FPl){
  cat("\n MLP")
  return(sum((TPl+FPl)==0)/l)
}

wlp <- function(TPl){
  cat("\n WLP")
  return(sum(TPl==0)/l)
}

results <- function(mc){
  
  a = accuracy(mc$true_labels, mc$pred_labels, names_labels)
  
  sa = subsetAccuracy(mc$true_labels, mc$pred_labels)
  
  zol = zerOneLoss(mc$true_labels, mc$pred_labels)
  
  hl = hammingLoss(mc$true_labels, mc$pred_labels)
  
  p = precision(mc$true_labels, mc$pred_labels)
  
  r = recall(mc$true_labels, mc$pred_labels)
  
  f = f1(mc$true_labels, mc$pred_labels)
  
  map = macroPrecision(mc$TPl, mc$FPl)
  
  mar = macroRecall(mc$TPl, mc$FNl)
  
  maf1 = macroF1(map, mar)
  
  mip = microPrecision(mc$TPl, mc$FPl)
  
  mir = microRecall(mc$TPl, mc$FNl)
  
  mif1 = microF1(mip, mir)
  
  clp = clp(mc$TPl, mc$FNl)
  
  mlp = mlp(mc$TPl, mc$FPl)
  
  wlp = wlp(mc$TPl)
  
  results_measures_bipartition_instances = data.frame(a, sa, zol, hl, p, r, f, map, mar, maf1,
                                                      mip, mir, mif1, clp, mlp, wlp)
  
  colnames(results_measures_bipartition_instances) = c("accuraccy", "subsetAccuraccy", "zeroOneLoss",
                                                       "hammingLoss", "precision", "recall","f1",
                                                       "macro-precision","macro-recall", "macro-f1",
                                                       "micro-precision","micro-recall","micro-f1",
                                                       "clp","mlp","wlp")
  
  return(results_measures_bipartition_instances)
}