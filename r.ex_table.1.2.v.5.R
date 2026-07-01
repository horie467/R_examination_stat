#学内Proxy用
#Sys.setenv("http_proxy" = "http://axis.nagoya-aoi.ac.jp:8080")
#Sys.setenv("https_proxy" = "http://axis.nagoya-aoi.ac.jp:8080")

# 必要パッケージの読み込み
if (!require("Cairo")) install.packages("Cairo")
if (!require("e1071")) install.packages("e1071")
if (!require("gridExtra")) install.packages("gridExtra")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("reshape2")) install.packages("reshape2")

#library(conflicted)
library(Cairo)
library(ggplot2)
library(reshape2)
library(tidyverse)
library(fmsb)
library(gridExtra)
library(grid)
library(png)
library(e1071)
library(magick)

# --- 設定 ---
#Windowsでのフォント
font_family <- "MS Gothic" 
os_name <- Sys.info()["sysname"]
if(os_name=="Windows") {
  windowsFonts(JP = windowsFont(font_family))
}
#試験名など
test_name <- "管理栄養士国家試験テストデータ"
test_date <- "2026/4/2"
#stampの点数
good_score<-120
verygood_score<-100
#実行の設定
print_process_flag <- TRUE
stamp_flag <- TRUE

if(print_process_flag) {
  print("Reading data...")
}

#data file
data_file_answers <- "./data/2026.sample.answer.data.4.csv"
data_file_scores <- "./data/2026.sample.data.4.csv"
data_suffix <- gsub("/",".",test_date)

# --- 保存用ディレクトリの作成 ---
output_dir1 <- paste0(data_suffix)
output_dir2 <- paste0(data_suffix,"/","individual_reports")
if (!dir.exists(output_dir1)) dir.create(output_dir1)
if (!dir.exists(output_dir2)) dir.create(output_dir2)

# --- データの読み込みと採点 ---
ans_data <- read.csv(data_file_answers, fileEncoding = "CP932", stringsAsFactors = FALSE, check.names = FALSE)
stu_data <- read.csv(data_file_scores, fileEncoding = "CP932", stringsAsFactors = FALSE, check.names = FALSE)

if(print_process_flag) {
  print("Calculating stats...")
}

# 問題名の抽出
q_names <- colnames(ans_data)[-1]
num_q <- length(q_names)

field_info <- data.frame(
  name = c("社会環境と健康", "人体の構造", "食べ物と健康", "基礎栄養学", "応用栄養学", 
           "栄養教育論", "臨床栄養学", "公衆栄養学", "給食経営管理論", "応用力"),
  code = LETTERS[1:10],
  start = c(1, 17, 43, 68, 82, 98, 111, 137, 153, 171),
  end = c(16, 42, 67, 81, 97, 110, 136, 152, 170, 200)
)

scoring <- function(student_ans, correct_ans) {
  if (as.character(correct_ans) == "*") return(1)
  c_ans_list <- sort(unlist(strsplit(as.character(correct_ans), ",")))
  s_ans_list <- sort(unlist(strsplit(as.character(student_ans), ",")))
  if (identical(c_ans_list, s_ans_list)) return(1) else return(0)
}

score_matrix <- matrix(0, nrow = nrow(stu_data), ncol = 200)
for (i in 1:nrow(stu_data)) {
  for (j in 1:200) {
    score_matrix[i, j] <- scoring(stu_data[i, j+2], ans_data[1, j+1])
  }
}
q_accuracy <- colMeans(score_matrix) * 100
total_scores <- rowSums(score_matrix)

if(print_process_flag) {
  print("Crating report data 1...")
}

# 集計表の作成 (ex_summary2.csv)
summary2 <- cbind(stu_data[, 1:2], score_matrix, Total = total_scores)
colnames(summary2) <- c("学籍番号", "氏名", q_names, "合計点")
file_path <- file.path(output_dir1, "ex_summary2.csv")
write.csv(summary2, file_path, row.names = FALSE, fileEncoding = "CP932")

if(print_process_flag) {
  print("Crating report data 2...")
}


# 試験の総合成績表 (ex_summary3.csv)
summary3 <- data.frame(
  項目 = c("試験名", "試験日", "平均点", "標準偏差", "受験者数", "最高点", "最低点", "歪率", "尖度", "中央値"),
  値 = c(test_name, test_date, 
        round(mean(total_scores), 2), 
        round(sd(total_scores), 2), 
        as.integer(nrow(stu_data)), 
        as.integer(max(total_scores)), 
        as.integer(min(total_scores)), 
        round(skewness(total_scores), 2), 
        round(kurtosis(total_scores), 2), 
        as.integer(median(total_scores)))
)
file_path <- file.path(output_dir1, "ex_summary3.csv")
write.csv(summary3,file_path, row.names = FALSE, fileEncoding = "CP932")

if(print_process_flag) {
  print("Creating score table ...")
}

# 個人別集計表の作成

sections <- data.frame(
  name = c("社会環境と健康", "人体の構造と機能", "食べ物と健康", "基礎栄養", "応用栄養", 
           "栄養教育", "臨床栄養", "公衆栄養", "給食経営管理", "応用力"),
  max_q = c(16, 26, 25, 14, 16, 13, 26, 16, 18, 30)
)
subject_cols <- sections$name
max_question_num <- sections$max_q
question_num_first <- rep(0,10)
question_num_first[1] <- 1
question_num_last <- rep(0,10)
question_num_last[1] <- max_question_num[1]
for(i in 2:length(max_question_num)) {
  question_num_first[i] <- question_num_last[i-1] + 1
  question_num_last[i] <- question_num_first[i] + max_question_num[i]-1
}

score_table_df <- data.frame(学籍番号=stu_data[,1],氏名=stu_data[,2])

for(i in 1:10) {
  df <- data.frame(rowSums(score_matrix[,(question_num_first[i]:question_num_last[i])]))
  score_table_df <- cbind(score_table_df,df)
}

colnames(score_table_df)[3:12] <- subject_cols
file_path <- file.path(output_dir1, "score_table_1.csv")
write.csv(score_table_df, file_path, row.names = FALSE, fileEncoding = "CP932")

if(print_process_flag) {
  print("Creating exam. report ...")
}

# --- 統計計算 ---
score_table_df$Total <- rowSums(score_table_df[, subject_cols])
score_table_df$Rank  <- rank(-score_table_df$Total, ties.method = "min")
score_table_df$Dev   <- round((score_table_df$Total - mean(score_table_df$Total)) / sd(score_table_df$Total) * 10 + 50, 1)

# 全体統計
sec_means <- colMeans(score_table_df[, subject_cols])
sec_sds   <- sapply(score_table_df[, subject_cols], sd)

# 分野別の順位を計算
for(col in subject_cols) {
  score_table_df[[paste0(col, "_Rank")]] <- rank(-score_table_df[[col]], ties.method = "min")
}

#Totalの順位を計算
score_table_df[[paste0("Total", "_Rank")]] <- rank(-score_table_df[["Total"]], ties.method = "min")

# --- 補助関数：レーダーチャートをGrobに変換 ---総合成績表
get_radar_grob1 <- function(data_row, max_vals, title, targets, f_label=FALSE) {
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 800, height = 700, res = 120)
  par(mar = c(2, 2, 4, 2), family = font_family)
  
  r_data <- as.data.frame(rbind(
    max_vals, 
    rep(0, 10), 
    as.numeric(targets), 
    as.numeric(data_row)
  ))
  colnames(r_data) <- subject_cols
  if(f_label) {
    radarchart(r_data, pcol = c("red", "darkorange"), plwd = 3, plty = 1,
               pty = 16, title = title, vlcex = 0.8, cglcol = "grey25"
               ,axistype = 1)
  } else {
    radarchart(r_data, pcol = c("red", "darkorange"), plwd = 3, plty = 1,
               pty = 16, title = title, vlcex = 0.8, cglcol = "grey25"
    )
  }
  legend("topright", legend = c("合格水準(%)", "平均正解率(%)"), col = c("red", "darkorange"), lty = 1, lwd = 2, cex = 0.7)
  dev.off()
  rasterGrob(readPNG(tmp), interpolate = TRUE)
}

# --- 補助関数：レーダーチャートをGrobに変換 ---個人票 -- 60%の合格水準を表示
get_radar_grob2 <- function(data_row, max_vals, title, targets, pass_grade, axistype=4) {
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 800, height = 700, res = 120)
  par(mar = c(2, 2, 4, 2), family = font_family)
  
  r_data <- as.data.frame(rbind(
    max_vals, 
    rep(0, 10), 
    as.numeric(targets), 
    as.numeric(data_row),
    as.numeric(pass_grade)
  ))
  colnames(r_data) <- subject_cols
  
  if(axistype==4) {
    step_val <- max_vals[1]/4
    axis_label <- c(0,step_val,step_val*2,step_val*3,step_val*4)
    r_data2 <- as.data.frame(rbind(r_data,as.numeric(sections$max_q)))
    radarchart(r_data2, pcol = c("deepskyblue", "darkorange","red","magenta"), plwd = 2, plty = 1,
               pty = 16, title = title, vlcex = 0.8, cglcol = "grey25"
               , axistype = axistype
               , caxislabels = axis_label)
    legend("topright", legend = c("平均", "あなたの点数","合格水準","満点"), col = c("deepskyblue", "darkorange","red","magenta"), lty = 1, lwd = 2, cex = 0.7)
    
  } else {
    radarchart(r_data, pcol = c("deepskyblue", "darkorange","red"), plwd = 2, plty = 1,
               pty = 16, title = title, vlcex = 0.8, cglcol = "grey25"
               , axistype = axistype)  
    legend("topright", legend = c("平均", "あなたの点数","合格水準"), col = c("deepskyblue", "darkorange","red"), lty = 1, lwd = 2, cex = 0.7)
    
  }
  
  dev.off()
  rasterGrob(readPNG(tmp), interpolate = TRUE)
}

# --- 2. 総合成績表の作成 ---
file_path <- file.path(output_dir1, "summary_report.pdf")
cairo_pdf(file_path, width = 8.27, height = 11.69, family = font_family)

# タイトルと試験情報
title_grob <- textGrob("総合成績報告書", gp=gpar(fontsize=20, fontface="bold"))
info_grob <- textGrob(paste0("試験名：", test_name, "    実施日：", test_date), gp=gpar(fontsize=12))

# 表2-1
summary_stats <- data.frame(
  項目 = c("受験者数", "最高点", "最低点", "平均点", "標準偏差", "歪率", "尖度", "中央値"),
  値 = c(nrow(score_table_df), max(score_table_df$Total), min(score_table_df$Total), round(mean(score_table_df$Total), 2),
        round(sd(score_table_df$Total), 2), round(skewness(score_table_df$Total), 2), round(kurtosis(score_table_df$Total), 2), median(score_table_df$Total))
)
t21_long <- tableGrob(t(summary_stats[,-1]), rows = NULL, cols = summary_stats$項目, 
                      theme = ttheme_default(base_family = font_family, base_size = 11))

# グラフ2-1 (ヒストグラム)
g21 <- ggplot(score_table_df, aes(x = Total)) +
  geom_histogram(breaks = seq(40, 200, by = 10), fill = "skyblue", color = "white") +
  scale_x_continuous(breaks = seq(40, 200, by = 10)) +
  labs(title = "全体の得点分布", x = "合計点", y = "人数") + theme_bw(base_family = font_family)

# グラフ2-2 (分野別平均)
g22_grob <- get_radar_grob1(sec_means, sections$max_q, "分野別平均点 (オレンジ:平均正解率、赤色:合格水準60%)", sections$max_q * 0.6,TRUE)

#頻度表
h <- hist(score_table_df$Total, breaks = seq(40, 200, by = 10), plot = FALSE)
class_names <- paste(h$breaks[-length(h$breaks)], "～", h$breaks[-1])
freq_table <- data.frame(
階級 = class_names,
度数 = h$counts
)

freq_table_2 <- rbind(freq_table,data.frame(階級="合計",度数=sum(freq_table$度数)))

row_heights <- unit(rep(6, nrow(freq_table_2) + 1), "mm") # 全行2 mm,最小？

t22_distribution <- tableGrob(
  freq_table_2, rows = NULL, 
  theme=ttheme_default(base_family=font_family, 
  base_size = 8,rowhead = list(padding = row_heights), 
  core = list(padding = unit(c(1, 2), "mm")),
  colhead = list(padding = unit(c(1, 2), "mm"))))

# 縦並び配置
grid.arrange(
  title_grob, info_grob, t21_long, 
  #g21, 
  arrangeGrob(g21, t22_distribution, ncol = 2, widths = c(2, 1)),
  g22_grob,
  heights = c(0.05, 0.05, 0.1, 0.35, 0.45),
  vp = viewport(width = unit(17, "cm"), height = unit(27, "cm"))
)
dev.off()

if(print_process_flag) {
  print("Creating individual report 1 ...")
}

# --- 個人別成績表1の作成 ---

for (i in 1:nrow(score_table_df)) {
  s <- score_table_df[i, ]
  file_path <- file.path(output_dir2, paste0(s$学籍番号, ".成績表1.pdf"))
  cairo_pdf(file_path, width = 8.27, height = 11.69, family = font_family)
  
  # --- 表3-1 (基本情報) ---
  t31_data <- data.frame(
    学籍番号 = s$学籍番号, 
    氏名 = s$氏名, 
    合計点 = as.integer(s$Total), # 整数表示
    順位 = paste0(as.integer(s$Rank), " / ", nrow(df)), # 整数表示
    偏差値 = s$Dev,
    受験者数 = nrow(df), 
    試験名 = test_name, 
    実施日 = test_date
  )
  t31 <- tableGrob(t31_data, rows=NULL, theme=ttheme_minimal(base_family=font_family, base_size = 9))
  
  # --- 表3-2 (得点・順位をすべて整数として整形) ---
  cols_with_total <- c(subject_cols, "Total")
  
  # 得点行 (整数化)
  row_score <- as.integer(round(as.numeric(s[cols_with_total])))
  # 偏差値行 (小数第1位)
  row_dev <- c(round((as.numeric(s[subject_cols]) - sec_means)/sec_sds * 10 + 50, 1), s$Dev)
  # 平均点行 (小数第1位)
  row_mean <- round(c(sec_means, mean(score_table_df$Total)), 1)
  # 標準偏差行 (小数第2位)
  row_sd <- round(c(sec_sds, sd(score_table_df$Total)), 2)
  # 順位行 (整数化)
  row_rank <- as.integer(as.numeric(s[paste0(cols_with_total, if_else(cols_with_total=="Total", "_Rank", "_Rank"))])) #Total_Rankのデータを作った
  
  t32_data <- data.frame(
    項目 = c("得点", "偏差値", "平均点", "標準偏差", "順位"),
    rbind(format(row_score, scientific = FALSE, trim = TRUE), 
          format(row_dev,nsmall=1),
          format(row_mean,nsmall=1),
          format(row_sd,nsmall=2),
          format(row_rank, scientific = FALSE, trim = TRUE))
  )
  colnames(t32_data) <- c("項目", subject_cols, "合計")
  t32 <- tableGrob(t32_data, rows=NULL, theme=ttheme_default(base_family=font_family, base_size = 6.5))
  
  find_cell <- function(table, row, col, name = "core-fg"){
    l <- table$layout
    which(l$t == row & l$l == col & l$name == name)
  }
  
  #偏差値のセルに色を付ける
  ii <- 2
  for(jj in 2:12) {
    if(as.numeric(t32_data[ii,jj]) >= 60 ) {
      cell_idx <- find_cell(t32, ii + 1, jj, "core-bg")
      t32$grobs[[cell_idx]]$gp <- gpar(fill = "lightblue", col = "grey")
    }
    if(as.numeric(t32_data[ii,jj]) < 40 ) {
      cell_idx <- find_cell(t32, ii + 1, jj, "core-bg")
      t32$grobs[[cell_idx]]$gp <- gpar(fill = "pink", col = "grey")
    }
  }
  
  #順位のセルに色を付ける
  ii <- 5
  for(jj in 2:12) {
    if(as.numeric(t32_data[ii,jj]) > unlist(t31_data["受験者数"])/2 ) {
      cell_idx <- find_cell(t32, ii + 1, jj, "core-bg")
      t32$grobs[[cell_idx]]$gp <- gpar(fill = "pink", col = "grey")
    }
  }
  ii <- 5
  for(jj in 2:12) {
    if(as.numeric(t32_data[ii,jj]) <= unlist(t31_data["受験者数"])/2 ) {
      cell_idx <- find_cell(t32, ii + 1, jj, "core-bg")
      t32$grobs[[cell_idx]]$gp <- gpar(fill = "lightblue", col = "grey")
    }
  }
  
  #マージン　左にスペースを空ける
  caption1_grob <- textGrob("　　＊偏差値は60以上は水色、40未満はピンクで示してあります。\n　　＊順位が前半の場合は水色、後半の場合はピンクで示してあります。",x = 0,hjust = 0,gp=gpar(fontsize=9, fontface="plain")) #x = 0,hjust = 0,
  
  # --- 表3-3 (正解率) ---
  t33_data <- data.frame(
    項目 = c("本人正解率", "全体正解率","標準偏差"),
    rbind(paste0(format(round(as.numeric(s[subject_cols])/sections$max_q*100, 1),nsmall=1), "%"),
          paste0(format(round(sec_means/sections$max_q*100, 1),nsmall=1), "%"),
          paste0(format(round(sec_sds/sections$max_q*100,1),nsmall=1), "%"))
  )
  colnames(t33_data) <- c("項目", subject_cols)
  t33 <- tableGrob(t33_data, rows=NULL, theme=ttheme_default(base_family=font_family, base_size = 7))
  
  # --- グラフ描画 ---
  r1_grob <- get_radar_grob2(s[subject_cols], rep(30,10), "分野ごとの得点比較", sec_means, sections$max_q * 0.6, 4)
  r2_grob <- get_radar_grob2(s[subject_cols]/sections$max_q*100, rep(100, 10), "正解率(%)比較", sec_means/sections$max_q*100, rep(60,10), 1)
  
  g33 <- ggplot(data.frame(x=c("本人", "全体平均"), y=c(s$Total, mean(score_table_df$Total)), sd=c(0, sd(score_table_df$Total))), aes(x, y, fill=x)) +
    geom_bar(stat="identity", width=0.5, color="black") + geom_errorbar(aes(ymin=y, ymax=y+sd), width=0.2) +
    scale_y_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 40)) + 
    scale_fill_manual(values=c("本人"="orange", "全体平均"="skyblue")) +
    theme_bw(base_family=font_family) + theme(legend.position="none",plot.margin = unit(c(0, 1, 0, 1), "cm")) + labs(title="合計点比較", x="", y="点数") 
  
  # --- グラフ3-4: ヒストグラム強調 ---
  bin_width <- 10
  bin_left  <- (ceiling(s$Total / bin_width) - 1) * bin_width
  bin_right <- bin_left + bin_width
  
  g34 <- ggplot(score_table_df, aes(x = Total)) +
    geom_histogram(breaks = seq(40, 200, by = bin_width), fill = "skyblue", color = "white", closed = "right") +
    # ご提案の通り > と <= を用いたフィルタリング
    geom_histogram(data = score_table_df %>% filter(Total > bin_left & Total <= bin_right), 
                   breaks = seq(40, 200, by = bin_width), fill = "orange", color = "white", closed = "right") +
    scale_x_continuous(breaks = seq(40, 200, by = bin_width)) +
    theme_bw(base_family=font_family) + labs(title="得点分布（オレンジ：あなたの位置）", x="合計点", y="人数") 
  
  # --- レイアウト配置 ---
  grid.arrange(
    textGrob("総合試験成績表", gp=gpar(fontsize=16, fontface="bold")),
    t31, t32,t33,
    caption1_grob,
    arrangeGrob(r1_grob, g33, ncol = 2),
    arrangeGrob(r2_grob, g34, ncol = 2),
    textGrob("名古屋葵大学　健康栄養学科　　\n　　",gp=gpar(fontsize=8,fontface="plain"),x = 1.0, just = "right", hjust = 1),
    
    heights = c(0.04, 0.08, 0.15,0.12,0.05,0.28, 0.28,0.02),
    vp = viewport(width = unit(17, "cm"), height = unit(27, "cm"))
  )
  
  # --- 判子（画像）の貼り付け処理 ---
  if(stamp_flag) {
    # 合計点による判定
    stamp_file <- if (s$Total >= verygood_score) {
      "./img/verygood.gif"
    } else if (s$Total >= good_score) {
      "./img/good.gif"
    } else {
    "./img/nogood.gif"
    }
  
    # 画像の読み込みとGrob化
    img <- image_read(stamp_file)
    img_grob <- rasterGrob(as.raster(img))
  
    # 右上 (3cm x 3cm) の位置に配置
    # A4 = 21.0cm x 29.7cm
    # 右端から3.5cm、上端から3.5cmの位置を中心に3cm四方で描画
    pushViewport(viewport(x = unit(1, "npc") - unit(2.5, "cm"), 
                        y = unit(1, "npc") - unit(2.5, "cm"), 
                        width = unit(3, "cm"), 
                        height = unit(3, "cm")))
    grid.draw(img_grob)
    popViewport()
  }
  
  dev.off()
}

if(print_process_flag) {
  print("Creating individual report 2 ...")
}

#個人別成績表2の作製

# 基本情報の共通テーマ（罫線あり）
base_theme <- ttheme_default(
  base_family = font_family, base_size = 6,
  padding = unit(c(1.2, 1.2), "mm"),
  core = list(bg_params = list(fill = "white", col = "gray70", lwd = 0.4)),
  colhead = list(bg_params = list(fill = "gray95", col = "gray70", lwd = 0.4))
)
base_theme_flarge <- ttheme_default(
  base_family = font_family, base_size = 8,
  padding = unit(c(1.2, 1.2), "mm"),
  core = list(bg_params = list(fill = "white", col = "gray70", lwd = 0.4)),
  colhead = list(bg_params = list(fill = "gray95", col = "gray70", lwd = 0.4))
)
base_theme_flarge2 <- ttheme_default(
  base_family = font_family, base_size = 10,
  padding = unit(c(1.2, 1.2), "mm"),
  core = list(bg_params = list(fill = "white", col = "gray70", lwd = 0.4)),
  colhead = list(bg_params = list(fill = "gray95", col = "gray70", lwd = 0.4))
)

for (i in 1:nrow(stu_data)) {
  file_path <- file.path(output_dir2, paste0(stu_data[i, "学籍番号"], ".成績表2.pdf"))
  CairoPDF(file_path, width = 8.27, height = 11.69, family = font_family)
  
  # 1. 表題
  title_grob <- textGrob("総合試験成績表２", gp = gpar(fontsize = 16, fontface = "bold", fontfamily = font_family))
  
  #成績表1と合わせる
  t1_data <- data.frame(
    学籍番号 = stu_data[i, 1], #s$学籍番号, 
    氏名 = stu_data[i, 2], #s$氏名, 
    合計点 = total_scores[i], #as.integer(s$Total), # 整数表示
    順位 = paste0(rank(-total_scores, ties.method = "min")[i],"/",nrow(stu_data)),
    偏差値 = round(50 + 10 * (total_scores[i] - mean(total_scores)) / sd(total_scores), 1), #s$Dev,
    受験者数 = nrow(stu_data), #nrow(df), 
    試験名 = test_name, 
    実施日 = test_date
  )
  t1 <- tableGrob(t1_data, rows=NULL, theme=ttheme_minimal(base_family=font_family, base_size = 9))
  
  # 問題別テーブル作成関数
  create_q_table <- function(start_q, end_q) {
    indices <- start_q:end_q
    num_cols <- length(indices) + 1
    codes <- sapply(indices, function(q) field_info$code[which(field_info$start <= q & field_info$end >= q)])
    st_ans <- as.character(stu_data[i, indices+2])
    cr_ans <- as.character(ans_data[1, indices+1])
    marks <- sapply(indices, function(q) {
      acc = q_accuracy[q]; res = score_matrix[i, q]
      if(res == 1) { if(acc < 45) "◎" else "○" } else { if(acc < 45) "X" else "X*" }
    })
    accs_text <- sprintf("%.1f%%", q_accuracy[indices])
    
    df_t <- data.frame(T = c("分野", "問題", "解答", "正答", "正誤", "正解率"), rbind(codes, indices, st_ans, cr_ans, marks, accs_text))
    colnames(df_t) <- NULL
    
    fill_matrix <- matrix("white", nrow = 6, ncol = num_cols)
    col_matrix <- matrix("black", nrow = 6, ncol = num_cols)
    fontface_matrix <- matrix("plain", nrow = 6, ncol = num_cols)
    fill_matrix[, 1] <- "gray95"
    
    for(c in 2:num_cols) {
      m <- marks[c-1]
      if (m %in% c("○", "◎")) { fill_matrix[5, c] <- "lightblue" } else { fill_matrix[5, c] <- "pink" }
      if (m == "X*") { 
        col_matrix[5, c] <- "red"; fontface_matrix[5, c] <- "bold" 
        fill_matrix[2,c] <- "pink"}
      else if (m == "◎") { 
        fontface_matrix[5, c] <- "bold" 
        fill_matrix[2,c] <- "lightblue"
      }
      if (q_accuracy[indices[c-1]] >= 45) { fill_matrix[6, c] <- "lightblue" }
    }
    
    table_theme <- ttheme_default(
      base_family = font_family, base_size = 5.5,
      # 縦のpaddingを1.2mmに増やしてセルを高く設定
      padding = unit(c(0.3, 1.5), "mm"), # 1.2 -> 1.5
      core = list(
        fg_params = list(col = col_matrix, fontface = fontface_matrix, fontsize = 5.5, fontfamily = font_family, parse = FALSE),
        bg_params = list(fill = fill_matrix, col = "gray70", lwd = 0.4)
      )
    )
    tg <- tableGrob(df_t, rows = NULL, theme = table_theme)
    tg$widths <- unit(rep(0.95 / num_cols, num_cols), "npc")
    return(tg)
  }
  
  tables_list <- lapply(seq(1, 200, by=25), function(x) create_q_table(x, x+24))
  
  # 凡例
  legend_text <- paste(apply(field_info[, c("code", "name")], 1, function(x) paste0(x[1], ":", x[2])), collapse = " ") # collapse = "    "
  t_legend <- textGrob(paste("【分野コード説明】", legend_text), gp = gpar(fontsize = 5.5, fontfamily = font_family))
  
  #マージン　左にスペースを空ける
  caption_grob <- textGrob("　　＊正誤については正解を青、不正解を赤で示してあります。正解率45%以上の場合は水色で示してあります。\n　　＊正解率が低い問題の正解は◎(問題番号が水色)、正解率が高い問題の不正解はX*(問題番号がピンク)で示してあります。",x = 0,hjust = 0,gp=gpar(fontsize=5, fontface="plain",fontfamily = font_family)) #x = 0,hjust = 0,
  
  # グラフ
  graph_data <- data.frame(分野 = factor(field_info$name, levels=field_info$name))
  graph_data$low_acc_correct <- sapply(1:10, function(f) sum(score_matrix[i, field_info$start[f]:field_info$end[f]] == 1 & q_accuracy[field_info$start[f]:field_info$end[f]] < 45))
  graph_data$high_acc_incorrect <- sapply(1:10, function(f) sum(score_matrix[i, field_info$start[f]:field_info$end[f]] == 0 & q_accuracy[field_info$start[f]:field_info$end[f]] >= 45))
  plot_df <- melt(graph_data, id.vars="分野")
  
  p1 <- ggplot(plot_df, aes(x=分野, y=value, fill=variable)) +
    geom_bar(stat="identity", position="dodge", width=0.7,colour = "grey40", size = 0.5) +
    scale_fill_manual(values=c("low_acc_correct"="lightblue", "high_acc_incorrect"="orange"), 
                      labels=c("正解率の低い問題での正解(%)", "正解率の高い問題での不正解(%)")) +
    labs(title="正解率の高い問題と低い問題", y="問題数", x="", fill = NULL) +
    
    theme_linedraw(base_family = font_family) + 
    theme(axis.text.x = element_text(angle = 15, hjust = 1, size = 6), 
          legend.text = element_text(size = 6), legend.position = "bottom",
          legend.title = element_blank(), 
          plot.title = element_text(size = 10, hjust = 0.5),
          panel.grid.major = element_line(color = "grey40"),  
          panel.grid.minor = element_line(color = "grey40"))
          #plot.title = element_text(size = 10, hjust = 0.5, fontface = "bold")) # <- error?
  
  # 全体配置
  grid.arrange(
    title_grob,
    t1,
    tables_list[[1]], tables_list[[2]], tables_list[[3]], tables_list[[4]],
    tables_list[[5]], tables_list[[6]], tables_list[[7]], tables_list[[8]],
    t_legend,
    caption_grob,
    p1,
    textGrob("名古屋葵大学　健康栄養学科　　\n　　",gp=gpar(fontsize=8,fontface="plain",fontfamily = font_family),x = 1.0, just = "right", hjust = 1),
    ncol=1,
    heights = c(0.39, 0.6, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.1,0.3, 3.4,0.01),
    vp = viewport(width = unit(17, "cm"), height = unit(27, "cm"))
  )
  
  dev.off()
}

