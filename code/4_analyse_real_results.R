remotes::install_github("mlr-org/mlr3benchmark")
library(rstatix)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(data.table)
library(magrittr)
library(mlr3benchmark)
library(magick)
# ---------------
# load and prep
# ---------------
set.seed(011120)
# load data
df = read.csv("c7_bench/real_jobs/results/results.csv")
df$learner_id = rep(c("KM", "Nel", "AE", "CPH", "GLM", "Pen", "Par", "Flex",
                 "RFB", "RFL", "RFC", "RFCIF", "RRT", "GBC",
                 "GBU", "GBG", "COXB", "SVM", "CoxT",
                 "DH", "DS", "LH", "PCH", "DNN"), 30)
# cleanup columns
df$task_id = factor(df$task_id, levels = unique(df$task_id))
df$learner_id = factor(df$learner_id, levels = unique(df$learner_id))

# set high values to to 1e5
df$VanA[df$VanA > 1e5] = 1e5
# df$C_Gonen[is.na(df$C_Gonen)] = 0
# df$RMSE[is.na(df$RMSE)] = 1e5
df$RMSE[df$RMSE > 1e5] = 1e5
# df$MAE[is.na(df$MAE)] = 1e5
df$MAE[df$MAE > 1e5] = 1e5

# ---------------------------
# compute and plot post-hocs function
# --------------------------
# colnames(df)[1:2] = c("task_id", "learner_id")
# df %<>% select(-ends_with('_se'))
ba = BenchmarkAggr$new(df)

# all signif
ba$friedman_test()

autoplot(ba, type = "fn", meas = "C_Uno")
ggsave("../images/C7_bench/fn_uno.png",
       width = 14, height = 11, units = "cm", dpi = 300)
autoplot(ba, type = "fn", meas = "IGS")
ggsave("../images/C7_bench/fn_igs.png",
       width = 15, height = 11, units = "cm", dpi = 300)
autoplot(ba, type = "fn", meas = "MAE")
ggsave("../images/C7_bench/fn_mae.png",
       width = 15, height = 11, units = "cm", dpi = 300)
autoplot(ba, type = "fn", meas = "VanA")
ggsave("../images/C7_bench/fn_van.png",
       width = 15, height = 11, units = "cm", dpi = 300)

file = "../images/C7_bench/cd_uno.png"
autoplot(ba, type = "cd", test = "nem", meas = "C_Uno", minimize = F, ratio = 1/1.5)
ggsave(file, width = 9, height = 6)
image_write(image_trim(image_read(file)), file)

file = "../images/C7_bench/cd_h.png"
autoplot(ba, type = "cd", test = "nem", meas = "C_Harrell", minimize = F, ratio = 1/1.5)
ggsave(file, width = 9, height = 6)
image_write(image_trim(image_read(file)), file)

file = "../images/C7_bench/cd_gh.png"
autoplot(ba, type = "cd", test = "nem", meas = "C_Gonen", minimize = F, ratio = 1/1.5)
ggsave(file, width = 9, height = 6)
image_write(image_trim(image_read(file)), file)

file = "../images/C7_bench/cd_igs.png"
autoplot(ba, type = "cd", test = "nem", meas = "IGS", minimize = T, ratio = 1/1.5)
ggsave(file, width = 9, height = 6)
image_write(image_trim(image_read(file)), file)

file = "../images/C7_bench/cd_ill.png"
autoplot(ba, type = "cd", test = "nem", meas = "ILL", minimize = T, ratio = 1/1.5)
ggsave(file, width = 9, height = 6)
image_write(image_trim(image_read(file)), file)

file = "../images/C7_bench/cd_mae.png"
autoplot(ba, type = "cd", test = "nem", meas = "MAE", minimize = T, ratio = 1/1.5)
ggsave(file, width = 9, height = 6)
image_write(image_trim(image_read(file)), file)

autoplot(ba, meas = "C_Uno")
  # labs(title = "Mean Model Performance for Uno's C", x = "Model", y = "Uno's C")
ggsave("../images/C7_bench/mean_uno.png",
       width = 9, height = 6)
autoplot(ba, meas = "IGS")
  # labs(title = "Mean Model Performance for IGS", x = "Model", y = "IGS")
ggsave("../images/C7_bench/mean_igs.png",
       width = 9, height = 6)
autoplot(ba, meas = "MAE")
  # labs(title = "Mean Model Performance for MAE", x = "Model", y = "MAE")
ggsave("../images/C7_bench/mean_mae.png",
       width = 9, height = 6)
autoplot(ba, meas = "VanA") + ylim(0, 3)
  # labs(title = sprintf("Mean Model Performance for Van's %s", "\u03B1"), x = "Model",
       # y = paste0("Van's ", "\u03B1"))
ggsave("../images/C7_bench/mean_van.png",
       width = 9, height = 6)

autoplot(ba, type = "box", meas = "C_Uno") +
  theme_minimal() + theme(legend.position = "none")
  # labs(title = "Boxplots over 30 Datasets for Uno's C", x = "Model", y = "Uno's C")
ggsave("../images/C7_bench/box_uno.png",
       width = 9, height = 6)
autoplot(ba, type = "box", meas = "IGS") +
  theme_minimal() + theme(legend.position = "none")
  # labs(title = "Boxplots over 30 Datasets for IGS", x = "Model", y = "IGS")
ggsave("../images/C7_bench/box_igs.png",
       width = 9, height = 6)
autoplot(ba, type = "box", meas = "MAE") +
  theme_minimal() + theme(legend.position = "none") +
  ylim(0, 100)
  # labs(title = "Boxplots over 30 Datasets for MAE", x = "Model", y = "MAE")
ggsave("../images/C7_bench/box_mae.png",
       width = 9, height = 6)
autoplot(ba, type = "box", meas = "VanA") +
  theme_minimal() + theme(legend.position = "none") + ylim(0, 10)
  # labs(title = sprintf("Boxplots over 30 Datasets for Van's %s", "\u03B1"), x = "Model",
       # y = paste0("Van's ", "\u03B1"))
ggsave("../images/C7_bench/box_van.png",
       width = 9, height = 6)
# # ---------------
# # datasets
# # ---------------
# df$IGS_l = df$IGS - 1.96 * df$IGS_se
# df$IGS_u = df$IGS + 1.96 * df$IGS_se
# df$MAE_l = df$MAE - 1.96 * df$MAE_se
# df$MAE_u = df$MAE + 1.96 * df$MAE_se
#
# plot_dataset = function(id, meas) {
#
#   dfid = subset(df, Data == id)
#
#   if (meas == "C_Uno") {
#
#     dfid$Model = factor(dfid$Model, levels = dfid$Model[order(dfid[, meas], decreasing = TRUE)])
#
#     p = ggplot(dfid,
#                aes_string(x = "Model", y =  meas, fill = "Model")) +
#       geom_bar(stat = "identity") +
#       theme(axis.text.y = element_text(angle = 45),
#             axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.7),
#             panel.grid = element_blank(),
#             panel.background = element_rect(fill = "white"),
#             legend.position = "none") +
#       labs(title = id)
#   } else {
#     dfid$Model = factor(dfid$Model, levels = dfid$Model[order(dfid[, meas], decreasing = FALSE)])
#
#     p = ggplot(dfid,
#                aes_string(x = "Model", y =  meas, colour = "Model")) +
#       geom_errorbar(aes_string(ymin = paste0(meas, "_l"),
#                                ymax = paste0(meas, "_u")),
#                     width=.5) +
#       geom_point() +
#       theme_minimal() +
#       theme(axis.text.y = element_text(angle = 45),
#             axis.text.x = element_text(angle = 45, vjust = 0.8, hjust = 0.7),
#             panel.grid = element_blank(),
#             panel.background = element_rect(fill = "white"),
#             legend.position = "none") +
#       labs(title = id)
#   }
#
#   return(p)
# }
# c_real = c("aids2", "ALL", "bmt", "channing", "diabetic", "flchain", "gbsg", "grace",
#            "hepatoCellular",
#            "kidtran", "lung", "melanoma", "metabric", "mgus", "nafld1", "nki", "nwtco", "ova",
#            "patient", "pbc", "pharmacoSmoking", "prostateSurvival", "rats", "support",
#            "transplant", "tumor", "udca1", "veteran", "wbc1", "whas")
# plot_list = vector("list", 30)
# names(plot_list) = c_real
# for (i in seq(30)) {
#   # plot_list[[i]]$C_Uno = plot_dataset(c_real[i], "C_Uno")
#   plot_list[[i]] = plot_dataset(c_real[i], "IGS") +
#     theme(axis.line.y = element_blank(), axis.text.y = element_blank(),
#           axis.ticks.y = element_blank())
#   # plot_list[[i]]$MAE = plot_dataset(c_real[i], "MAE")
# }
# grid.arrange(grobs = plot_list[1:15], layout_matrix = matrix(1:15, nrow = 5, ncol = 3, byrow = TRUE))
# grid.arrange(grobs = plot_list[16:30], layout_matrix = matrix(1:15, nrow = 5, ncol = 3, byrow = TRUE))
