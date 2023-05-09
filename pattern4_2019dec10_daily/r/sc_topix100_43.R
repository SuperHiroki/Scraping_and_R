#https://www.mutanoblog.org/2022/04/03/synthetic_control_and_effectiveness_verification/

#株価が載っていなかったりしたらスクレイピングの時点で弾く。
#海外比率などの予測変数がなかったらnaでのこしておき、rで除外する。
#treated unitがtopix100の銘柄に入っている場合、重複でデータを持ってきているので、rで除外する。
#ソフトバンク(9434)みたいにスクレイピングで弾かれないけど、株価のデータ数が足りない場合は、rで除外する。


#予測変数を決定して、synthの中身、na除外のところを変更する。
#BFGS, nelder-meadでいいかな。
#コントロール群の数は80でいいかな。
#placebo testの回数は200回でいいかな。
#equity_ratioは「--」も除外している。
#2022-01-01がcsvの中で勝手に2022/1/1に変換されてしまう時がある。
#↓
#週ごとのデータの場合、前後のイベントに関わっている銘柄を除外する。
#treated unitsを変更
#データ読み込み場所の確認。実際にファイルがあるかを確認。
#日付を変更。(発表日の前日が処置日だから注意。)
#↓
#日次データと週次データで、グラフの縦幅を変えていいかも。
#写真保存(最初のscの表だけwindowsの方で)
#ワークスペースを保存


#####
# synthのweightの桁数が小数第三位までしかないから、反実仮想の正規化した時点のところが100じゃない時がある。
#個別のSCは細かく計算してpath.plotとかやってるんだろうけど、synth.tables$tab.wでウェイトを出すと、小数第三位までしかない。
#wres_df_for_not_placebo[,2] %>% sum()   0.994
#wres_df_for_not_placebo[,3] %>% sum()   0.987
#####

########################
#2019/3/5, daily
########################
#写真保存(
#stock_price_all_company, 
#conterfactual_vs_real, 
#conterfactual_vs_real_gap, 
#characteristics_of_counterfactual_units, #スクショ
#weights_of_control_units, #スクショ
#placebo_test_gap, 
#p_value_regression #スクショ)
########################

#過去のオブジェクトのキャッシュを削除。
rm(list =ls())
ls()## character(0)が出力されるはず。
##########################################################
##########################################################
########################################################## 準備
#packageインストール
#install.packages("Synth")
#install.packages("tidyverse")
library(Synth)
library(tidyverse) 
library(dplyr)
library(ggplot2)
#basqueのデータ読み込み。
topix100<-read.csv("../python_scraping/nikkei225/1/nikkei225_1.csv",  header=T, fileEncoding = "cp932")
View(topix100)###
#rで削除されるcompany一覧。
rm_company<-c()
#softbank(9434)みたいにデータが途中から(まで)しかないものがある。
dddd<-topix100$company_code %>% unique()
xxxx<-c()
for (y in 1:length(dddd)){
  xxxx<-c(xxxx, sum(topix100$company_code==dddd[y]))
}
kkkk<-cbind(data.frame(code=dddd), hindo=xxxx)
zzzz<-names(which.max(table(kkkk$hindo)))
uuuu<-kkkk %>% filter(as.numeric(hindo)>=as.numeric(zzzz))
topix100<-topix100[which(topix100$company_code %in% uuuu$code), ]
rm_company<-c(rm_company, kkkk %>% filter(as.numeric(hindo)<as.numeric(zzzz)) %>% pull(code))#removeされたcompany。データ数が最頻値よりも小さい場合が削除となる。
#本当のtopix100の銘柄とtreated unitsでかぶりがあったら、片方を削除する。
treated_units<-c(6754, 7762, 8001)#c(5108, 4631, 6473, 4528)#ここを変更する。
View(topix100 %>% filter(company_code %in% treated_units))#かぶりがあるのか視認しておく。   ###
kkk<-nrow(topix100)/length(topix100$index_k %>% unique())
span<-length(topix100$index_k %>% unique())
eee<-c()
for (h in 1:kkk){
  eee<-c(eee, topix100[span*(h-1)+1, which(colnames(topix100)==c('company_code'))])
}
www<-which(duplicated(eee))
ppp<-c()
for (q in 1:length(www)){
  ppp<-c(ppp, ((span*(www[q]-1)+1):(span*www[q])))
}
topix100<-topix100[-ppp, ]
rm_company<-c(rm_company, eee[which(duplicated(eee))])#removeされたcompany。
#foreign_demand, equity_ratio, pbrがnaとなっている企業を除外。
rm_company<-c(rm_company, topix100 %>% filter(topix100$foreign_demand=="na" | topix100$equity_ratio=="na" | topix100$pbr=="na"| topix100$equity_ratio=="--")  %>% pull(company_code) %>% unique())#removeされたcompany。
topix100<-                         topix100[!(topix100$foreign_demand=="na" | topix100$equity_ratio=="na" | topix100$pbr=="na"| topix100$equity_ratio=="--"), ]
#週ごとのデータを使う場合、前後のイベントに関わりのある銘柄を除外する。
code_units_related_to_bf_af_event<-c(0)
if(length(which(topix100$company_code %in% code_units_related_to_bf_af_event))!=0){
  rm_company<-c(rm_company, topix100 %>% filter( company_code %in% code_units_related_to_bf_af_event) %>% pull(company_code) %>% unique())#removeされたcompany。
  topix100<-topix100                 %>% filter(!company_code %in% code_units_related_to_bf_af_event)
}
if(length(which(treated_units %in% code_units_related_to_bf_af_event))!=0){
  treated_units<-treated_units[-which(treated_units %in% code_units_related_to_bf_af_event)]
}
#2022-01-01がcsvの中で勝手に2022/1/1に変換されてしまう時があるので、修正する。
topix100<-topix100 %>% 
  dplyr::mutate(date=topix100$date %>% as.Date() %>% as.character())
#treated units とcontrol units のコード番号。
all_units<- topix100$company_code %>% unique() 
treated_units<-treated_units
control_units<-all_units[-which(all_units %in% c(treated_units))]
#コントロール群が多すぎると行列計算に不具合が起きるので、コントロール群を減らす。
number_of_control_units<-80
control_units<-           control_units[ which(control_units %in% (topix100 %>% filter(company_code %in% control_units) %>% filter(trading_value >= (topix100 %>% filter(company_code %in% control_units) %>% pull(trading_value) %>% unique())[order(topix100 %>% filter(company_code %in% control_units) %>% pull(trading_value) %>% unique(), decreasing=T)[number_of_control_units]]) %>% pull(company_code) %>% unique()))]#サンプル数減らします
rm_company<-c(rm_company, control_units[-which(control_units %in% (topix100 %>% filter(company_code %in% control_units) %>% filter(trading_value >= (topix100 %>% filter(company_code %in% control_units) %>% pull(trading_value) %>% unique())[order(topix100 %>% filter(company_code %in% control_units) %>% pull(trading_value) %>% unique(), decreasing=T)[number_of_control_units]]) %>% pull(company_code) %>% unique()))])#removeされたcompany
topix100<-topix100 %>% filter(company_code %in% c(control_units, treated_units))
#index_c
index_c<-c()
company_kosuu<-nrow(topix100)/length(topix100$index_k %>% unique())
for ( p in 1:company_kosuu){
  index_c<-c(index_c, rep(p,times=span))
}
topix100<-add_column(topix100, index_c, .after = "index_k")
#日付を格納
start_day<-as.Date('2019-10-25')
end_day<-as.Date('2020-01-24')
start_estimation_window<-as.Date('2019-11-20')
end_estimation_window<-as.Date('2019-12-06')
event_day<-as.Date('2019-12-09')#本当は20221211
start_post_event_window<-as.Date('2019-12-09')
end_post_event_window<-as.Date('2020-01-24')
#date_numericを列に追加。
date_numeric<-topix100$index_k-1
topix100<-add_column(topix100, date_numeric, .after = "index_k")
date_numeric_vector<-topix100$date_numeric %>% unique()
#date_numeric2を列に追加。
#weekのデータは、同じ曜日に取得されてなかったり、取得されてない時がある(例えば、2019-04-22~2019-05-07)。
date_numeric2<-((topix100$date %>% as.Date())-start_day) %>% as.numeric()
topix100<-add_column(topix100, date_numeric2, .after = "index_k")
date_numeric2_vector<-topix100$date_numeric2 %>% unique()
#numeric型の日付を格納
start_day_numeric<-topix100 %>% filter(date==start_day %>% as.character) %>% select(date_numeric) %>% unique() %>% as.numeric()
end_day_numeric<-topix100 %>% filter(date==end_day %>% as.character) %>% select(date_numeric) %>% unique() %>% as.numeric()
start_estimation_window_numeric<-topix100 %>% filter(date==start_estimation_window %>% as.character) %>% select(date_numeric) %>% unique() %>% as.numeric()
end_estimation_window_numeric<-topix100 %>% filter(date==end_estimation_window %>% as.character) %>% select(date_numeric) %>% unique() %>% as.numeric()
event_day_numeric<-topix100 %>% filter(date==event_day %>% as.character) %>% select(date_numeric) %>% unique() %>% as.numeric()
start_post_event_window_numeric<-topix100 %>% filter(date==start_post_event_window %>% as.character) %>% select(date_numeric) %>% unique() %>% as.numeric()
end_post_event_window_numeric<-topix100 %>% filter(date==end_post_event_window %>% as.character) %>% select(date_numeric) %>% unique() %>% as.numeric()
#numeric2型の日付を格納
start_day_numeric2<-topix100 %>% filter(date==start_day %>% as.character) %>% select(date_numeric2) %>% unique() %>% as.numeric()
end_day_numeric2<-topix100 %>% filter(date==end_day %>% as.character) %>% select(date_numeric2) %>% unique() %>% as.numeric()
start_estimation_window_numeric2<-topix100 %>% filter(date==start_estimation_window %>% as.character) %>% select(date_numeric2) %>% unique() %>% as.numeric()
end_estimation_window_numeric2<-topix100 %>% filter(date==end_estimation_window %>% as.character) %>% select(date_numeric2) %>% unique() %>% as.numeric()
event_day_numeric2<-topix100 %>% filter(date==event_day %>% as.character) %>% select(date_numeric2) %>% unique() %>% as.numeric()
start_post_event_window_numeric2<-topix100 %>% filter(date==start_post_event_window %>% as.character) %>% select(date_numeric2) %>% unique() %>% as.numeric()
end_post_event_window_numeric2<-topix100 %>% filter(date==end_post_event_window %>% as.character) %>% select(date_numeric2) %>% unique() %>% as.numeric()
#stock_priceを標準化(基準はstart_estimation_window_numeric)
stock_price_normalized<-c()
for (s in 1:company_kosuu){
  for (t in 1: span){
    stock_price_normalized<-c(stock_price_normalized, topix100[(span*(s-1))+t, which(colnames(topix100)=='stock_price')]/topix100[1+(span*(s-1))+(start_estimation_window_numeric-1), which(colnames(topix100)=='stock_price')]*100)
  }
}
names(topix100)[ which( names(topix100)=="stock_price" ) ] <- "stock_price_original"
topix100<-add_column(topix100, stock_price=stock_price_normalized, .after = 'date')
#業種のダミー変数
topix100$rubber<-ifelse(topix100$type_of_industry=='ゴム製品', 1, 0)
topix100$chemical<-ifelse(topix100$type_of_industry=='化学', 1, 0)
topix100$machine<-ifelse(topix100$type_of_industry=='機械', 1, 0)
topix100$pharmaceuticals<-ifelse(topix100$type_of_industry=='医薬品', 1, 0)
#accounting_periodのダミー変数
topix100$accounting_period_mar<-ifelse(topix100$accounting_period=='3月末日', 1, 0)
topix100$accounting_period_jun<-ifelse(topix100$accounting_period=='6月末日', 1, 0)
topix100$accounting_period_sep<-ifelse(topix100$accounting_period=='9月末日', 1, 0)
topix100$accounting_period_dec<-ifelse(topix100$accounting_period=='12月末日', 1, 0)
#treated_unitsとそれ以外のダミー変数。
topix100<-topix100 %>% mutate(target = if_else(company_code %in% treated_units, "treated_units", "control_units")) 
#foreign_demand, equity_ratio, pbrをnumeric化(synthでchar判定されてしまうから)
topix100<-topix100 %>% 
  dplyr::mutate(foreign_demand=as.numeric(topix100$foreign_demand)) %>% 
  dplyr::mutate(equity_ratio=as.numeric(topix100$equity_ratio)) %>%
  dplyr::mutate(pbr=as.numeric(topix100$pbr)) %>%
  dplyr::mutate(stock_price=as.numeric(topix100$stock_price)) %>%
  dplyr::mutate(date_numeric=as.numeric(topix100$date_numeric)) %>%
  dplyr::mutate(company_code=as.numeric(topix100$company_code))
#view
rm_company %>% print()###
nrow(topix100) ###データ数の確認。63(時系列の長さ)*82(コントロール群と処置群)=5166
View(topix100) ###
##########################################################
##########################################################
##########################################################全ての会社の株価のグラフを書く。
ggplot() +
  geom_line(data.frame(company_code=factor(topix100$company_code, levels=c(topix100$company_code[-which(topix100$company_code %in% treated_units)] ,treated_units) %>% unique())) %>% cbind(topix100 %>% select(-company_code)), mapping = aes(x = date_numeric, y = stock_price, group = company_code, color = target, size = target))+
  geom_vline(xintercept =event_day_numeric , linetype = "dashed", alpha = 0.6, size=1.5) +
  geom_vline(xintercept =start_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  geom_vline(xintercept =end_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  scale_y_continuous(
    limits = c(60, 140),
  ) +
  scale_x_continuous(
    limits = c(start_estimation_window_numeric-1, start_post_event_window_numeric+25),
  ) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title=element_blank()) +
  scale_colour_manual(values = c("gray", "black")) +
  scale_size_manual(values = c(0.4, 0.8)) +
  ggtitle("all company stock price") + 
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("date")
##########################################################
##########################################################
########################################################## sc of multiple treated units
#treated unit の繰り返し。
treated_units_mspe<-data.frame()#treated unitそれぞれのmspe。
control_res_df_each_treated_unit<-data.frame(date_numeric=unique(topix100$date_numeric)) #treated unitそれぞれの合成コントロール(反実仮想)を格納。この行列にウェイトをかけることで、treated unitsの合成コントロール(反実仮想)を作る。
tab_pred_treated<-data.frame()#treated unitそれぞれのの特徴(予測変数)
tab_pred_synthetic<-data.frame()#treated unitそれぞれの合成コントロール(反実仮想)の特徴(予測変数)。
tab_pred_synthetic_mean<-data.frame()#treated unitそれぞれの合成コントロールを構成する企業(コントロール群)の平均の特徴(予測変数)。
tab_pred_for_each_treated_unit<-data.frame()#それぞれのtreated_unitに関して、treated_unitの予測変数、合成treated_unitの予測変数、合成treated_unitを構成する企業の平均の予測変数これらを論文のために縦につなげただけ。
wres_df_for_not_placebo<-data.frame(company_code=control_units)#それぞれのtreated unitを作る時の各企業のウェイト。
for (i in 1:length(treated_units)) {
  i %>% print()
  control_units %>% print()
  treated_units %>% print()
  treated_units[i] %>% print()
  dataprep.out <- dataprep(
    foo = topix100,
    predictors = c(
      "foreign_demand",
      "equity_ratio",
      #"per",
      "pbr"
      #"accounting_period_mar",
      #"accounting_period_jun",
      #"rubber",
      #"chemical",
      #"machine",
      #"pharmaceuticals",
      #"Construction",
      #"electrical_equipment",
      #foodstuff",
      #"transportation_equipment",
      #"retail_trade",
      #"textile_products",
      #"precision_equipment",
      #"wholesale_business",
      #"service",
      #"bank",
      #"petroleum_and_coal_products",
      #"steel",
      #"other_financial_services",
      #"others",
    ),
    predictors.op = "mean",　
    time.predictors.prior = start_estimation_window_numeric:end_estimation_window_numeric,     # 介入前期間の指定           # Z_iの調整
    special.predictors = list(             # Z_iの調整
      list("stock_price", seq(start_estimation_window_numeric+3, start_estimation_window_numeric+3, 1), "mean"),
      list("stock_price", seq(start_estimation_window_numeric+6, start_estimation_window_numeric+6, 1), "mean"),
      list("stock_price", seq(start_estimation_window_numeric+9, start_estimation_window_numeric+9, 1), "mean"),
      list("stock_price", seq(start_estimation_window_numeric+12, start_estimation_window_numeric+12, 1), "mean")
    ),
    dependent = "stock_price",                  # 目的変数の指定 
    unit.variable = "company_code",            # 各地域を識別する変数の指定
    unit.names.variable = "company_name",
    time.variable = "date_numeric",                # 時刻を表す変数の指定#numericのみ
    treatment.identifier = treated_units[i],             # 介入群の指定
    controls.identifier = control_units,     # コントロール群の指定
    time.optimize.ssr = start_estimation_window_numeric:end_estimation_window_numeric,        # 最適化する期間の指定
    time.plot = (start_estimation_window_numeric-1):(start_post_event_window_numeric+25)               # グラフ化する際に使用する期間の指定
  )
  #論文だと、予測変数がZiで、結果変数の時系列データがXiだったと思う。逆になっている。
  #dataprep.out$X1 #treated unitの予測変数(平均値)。
  #dataprep.out$Z1 #treated unitの結果変数の時系列データ。
  #dataprep.out$X0 #control unitsの予測変数(平均値)。
  #dataprep.out$Z0 #control unitsの結果変数の時系列データ
  # 合成コントロールの実行
  synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod = c("Nelder-Mead",'BFGS'))
  #グラフ化
  path.plot(
    synth.res = synth.out, 
    dataprep.res = dataprep.out,
    Ylab = "stock price",
    Xlab = "date",
    Ylim = c(80, 120),
    Legend = c( paste0("treated_unit", treated_units[i]), paste("synthetic", paste0("treated_unit", treated_units[i]))), 
    Legend.position = "bottomright"
  )
  #今回のtreated unitの、実際のデータとト反実仮想のデータ(合成コントロール)の差。
  gaps.plot(
    synth.res = synth.out, 
    dataprep.res = dataprep.out,
    Ylab = "stock price", 
    Xlab = "date",
    Ylim = c(-20, 20), 
    Main = NA
  )
  #今回のtreated unitの反実仮想(合成コントロール)を作る上での、コントロール群のそれぞれの企業のウェイトかな。
  synth.tables <- synth.tab(synth.res = synth.out, dataprep.res = dataprep.out)
  w_res <- synth.tables$tab.w$w.weights
  wres_df_for_not_placebo <- cbind(#それぞれのtreated unitを作る時の各企業のウェイトを格納。
    wres_df_for_not_placebo,
    data.frame(
      wres = w_res
    )
  )
  #それぞれのtreated_unitに関して、treated_unitの予測変数、合成treated_unitの予測変数、合成treated_unitを構成する企業の平均の予測変数これらを論文のために縦につなげただけ。
  tab_pred_for_each_treated_unit<-tab_pred_for_each_treated_unit %>% rbind(data.frame(predictors= rownames(synth.tables$tab.pred)) %>% cbind(treated_unit_number=i) %>% cbind(synth.tables$tab.pred %>% as.data.frame()))
  #treated unitの特徴とtreated unitの合成コントロール(反実仮想)の特徴(予測変数の数値)。
  if (i==1){
    tab_pred_treated<-data.frame(predictors= rownames(synth.tables$tab.pred))
    tab_pred_synthetic<-data.frame(predictors= rownames(synth.tables$tab.pred))
    tab_pred_synthetic_mean<-data.frame(predictors= rownames(synth.tables$tab.pred))
  }
  tab_pred_treated<-       cbind(tab_pred_treated       , no_name=synth.tables$tab.pred[,1] %>% as.vector())
  tab_pred_synthetic<-     cbind(tab_pred_synthetic     , no_name=synth.tables$tab.pred[,2] %>% as.vector())
  tab_pred_synthetic_mean<-cbind(tab_pred_synthetic_mean, no_name=synth.tables$tab.pred[,3] %>% as.vector())
  # コントロール群のデータのみ抽出
  control_units_data <- topix100 %>% 
    filter(company_code %in% control_units) %>% 
    select(company_code, date_numeric, stock_price) %>% 
    spread(date_numeric, stock_price) %>% 
    arrange(match(company_code, topix100 %>% filter(company_code %in% control_units) %>% pull(company_code) %>% unique())) %>% 
    select(-company_code) %>%
    as.matrix()
  # 処理ユニットのデータのみ抽出
  treated_unit_data <- topix100 %>% 
    filter(company_code == treated_units[i]) %>% 
    select(company_code, date_numeric, stock_price) 
  treated_unit_data <- data.frame(
    date_numeric = treated_unit_data$date_numeric,
    stock_price = treated_unit_data$stock_price
  )
  # 行列計算を用いて加重平均を算出。今回のtreated unitの合成コントロール(反実仮想)を作る。
  control_res <- control_units_data %>% t() %*% w_res %>% t() 
  control_res <- data.frame(
    date_numeric = control_res %>% as.data.frame() %>% colnames() %>% as.integer(),
    stock_price = control_res %>% as.vector()
  )
  control_res_df_each_treated_unit<-cbind(control_res_df_each_treated_unit, yyy=control_res$stock_price)#treated unitそれぞれの合成コントロール(反実仮想)を格納。この行列にウェイトをかけることで、treated unitsの合成コントロール(反実仮想)を作る。
  # 今回のtreated unitの平均二乗誤差率(MSPE)の算出。mspe取得の期間はestimation window。
  span <- control_res %>% 
    filter(start_estimation_window_numeric <= date_numeric & date_numeric <=  end_estimation_window_numeric) %>% 
    nrow()
  treated_unit_data_bf_event <- treated_unit_data %>% 
    filter(start_estimation_window_numeric <= date_numeric & date_numeric <=  end_estimation_window_numeric) %>% 
    pull(stock_price)
  control_res_bf_event <- control_res %>% 
    filter(start_estimation_window_numeric <= date_numeric & date_numeric <=  end_estimation_window_numeric) %>% 
    pull(stock_price)
  mspe <- (  (((treated_unit_data_bf_event - control_res_bf_event)^2) %>% sum()) / (end_estimation_window_numeric-start_estimation_window_numeric+1)  ) %>% sqrt()
  # 今回のtreated unitのmspeを格納
  treated_units_mspe <- rbind(
    treated_units_mspe,
    data.frame(
      company_code = treated_units[i],
      mspe = mspe #実際はRMSPEとなっている
    )
  )
}
################ここのふたつは、placebo_testでも同じ変数名を使っているから、消える。
treated_units_mspe　%>% print()###
control_res_df_each_treated_unit %>% print()###グラフ化してるからそっちを論文に載せる。
################
wres_df_for_not_placebo %>% print()###論文に載せる。
tab_pred_for_each_treated_unit %>% print()###論文に載せる。
#上で求めたそれぞれのtreated unitのmspeを用い、てtreated unitsの中でウェイトを割り当てる。
w_in_treated_units<-c()
for (i in 1:nrow(treated_units_mspe)){
  w_in_treated_units<-append(w_in_treated_units ,(1/treated_units_mspe[i,2])/((1/treated_units_mspe[,2]) %>% sum()))
}
treated_units_mspe_weight<-cbind(treated_units_mspe, w_in_treated_units)
treated_units_mspe_weight %>% print()###論文に載せる。#placebo_testでも同じ変数名を使っているから、消える。
treated_units_mspe_weight_for_ronbun <- treated_units_mspe_weight#論文のために保存。
#treated unitsのウェイトを使って、treated unitsの合成データとtreated unitsの合成コントロール(反実仮想)を構成するの各企業のウェイトを作る。
wres_treated_units<-(wres_df_for_not_placebo[, -1] %>% as.matrix()) %*% treated_units_mspe_weight[, 'w_in_treated_units']
wres_treated_units<- data.frame(company_code=control_units, company_name=topix100 %>% filter(company_code %in% control_units) %>% pull(company_name) %>% unique(), weight=wres_treated_units %>% as.vector())
View(wres_treated_units)###
#treated unitsのウェイトを使って、treated unitsの合成データとtreated unitsの合成コントロール(反実仮想)の特徴(予測変数)とコントロール群の予測変数の平均を作る。
tab_pred_treated_units<-                    (tab_pred_treated[, -1]        %>% as.matrix()) %*% treated_units_mspe_weight[, 'w_in_treated_units']
tab_pred_counterfactual_treated_units<-     (tab_pred_synthetic[, -1]      %>% as.matrix()) %*% treated_units_mspe_weight[, 'w_in_treated_units']
tab_pred_counterfactual_treated_units_mean<-(tab_pred_synthetic_mean[, -1] %>% as.matrix()) %*% treated_units_mspe_weight[, 'w_in_treated_units']
tab_pred_treated_units_and_counterfactual_treated_units_and_mean<-data.frame(predictors=tab_pred_treated[, 1], treated_units=tab_pred_treated_units, counterfactual_treated_units=tab_pred_counterfactual_treated_units, counterfactual_treated_units_mean=tab_pred_counterfactual_treated_units_mean)
tab_pred_treated_units_and_counterfactual_treated_units_and_mean %>% print()###
#treated unitsのウェイトを使って、treated unitsの合成データを作る。
treated_units_data <- topix100 %>% 
  filter(company_code %in% treated_units) %>% 
  select(company_code, date_numeric, stock_price) %>% 
  spread(date_numeric, stock_price) %>% 
  arrange(match(company_code, topix100 %>% filter(company_code %in% treated_units) %>% pull(company_code) %>% unique())) %>% 
  select(-company_code) %>%
  as.matrix()
synthetic_treated_units_data<-treated_units_data %>% t() %*% treated_units_mspe_weight[, 'w_in_treated_units'] %>% t()
synthetic_treated_units_data_df <- data.frame(
  date_numeric = synthetic_treated_units_data %>% as.data.frame() %>% colnames() %>% as.integer(),
  stock_price = synthetic_treated_units_data %>% as.vector()
)
synthetic_treated_units_data_df %>% print()###
synthetic_treated_units_data_df_for_henkaritu <- synthetic_treated_units_data_df#後で消えちゃうので、残しておく。
#treated unitsのウェイトを使って、treated unitsの合成コントロール(反実仮想)を作る。
control_res_df_each_treated_unit_minusdate<-control_res_df_each_treated_unit[, -which (colnames(control_res_df_each_treated_unit) %in% c("date_numeric"))] %>% as.matrix()
counterfactual_synthetic_treated_units_data<-control_res_df_each_treated_unit_minusdate %*% treated_units_mspe_weight[, 'w_in_treated_units'] 
counterfactual_synthetic_treated_units_data_df<-data.frame(
  date_numeric=control_res_df_each_treated_unit[, 'date_numeric'],
  stock_price=counterfactual_synthetic_treated_units_data 
)
counterfactual_synthetic_treated_units_data_df %>% print()###
counterfactual_synthetic_treated_units_data_df_for_henkaritu <- counterfactual_synthetic_treated_units_data_df#後で消えちゃうので、残しておく。
########################################################## 
########################################################## 
#not_placebo testにおける、時系列データを格納するデータフレーム
real_and_counterfactual_df_not_placebo<-data.frame(
    group_num=paste0('group_', '1'),
    date_numeric = synthetic_treated_units_data_df$date_numeric,
    real = synthetic_treated_units_data_df$stock_price,
    counterfactual = counterfactual_synthetic_treated_units_data_df$stock_price,
    not_placebo_or_placebo = 'not_placebo_test'
  )
henkaritu_waru_henkaritu_not_placebo<-
  data.frame(
    group_num=paste0('group_', 1),
    real_event_day_numeric_minus1= synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    real_event_day_numeric_plus0 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price),
    real_event_day_numeric_plus1 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price),
    real_event_day_numeric_plus2 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price),
    henkaritu_real_plus0 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    henkaritu_real_plus1 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    henkaritu_real_plus2 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    counterfactual_event_day_numeric_minus1=counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    counterfactual_event_day_numeric_plus0 =counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price),
    counterfactual_event_day_numeric_plus1 =counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price),
    counterfactual_event_day_numeric_plus2 =counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price),
    henkaritu_counterfactual_plus0 = counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    henkaritu_counterfactual_plus1 = counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    henkaritu_counterfactual_plus2 = counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
    compare_real_and_counterfactual_day_plus0=  (  synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price) )   /   (   counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price)  )  -1,
    compare_real_and_counterfactual_day_plus1=  (  synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price) )   /   (   counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price)  )  -1,
    compare_real_and_counterfactual_day_plus2=  (  synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price) )   /   (   counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price)  )  -1,
    not_placebo_or_placebo = 'not_placebo_test'
  )
#「treated unitsの合成データ」と「treated unitsの合成コントロール(反実仮想)」の時系列の差と累積の差を求める。
gap<-(synthetic_treated_units_data_df %>% pull(stock_price)) - (counterfactual_synthetic_treated_units_data_df %>% pull(stock_price))
gap_bf_event <-  (synthetic_treated_units_data_df %>% filter(start_estimation_window_numeric<=date_numeric & date_numeric<=end_estimation_window_numeric) %>% pull(stock_price)) - (counterfactual_synthetic_treated_units_data_df %>% filter(start_estimation_window_numeric<=date_numeric & date_numeric<=end_estimation_window_numeric) %>% pull(stock_price)) 
gap_af_event <-  (synthetic_treated_units_data_df %>% filter(start_post_event_window_numeric<=date_numeric & date_numeric<=end_post_event_window_numeric) %>% pull(stock_price)) - (counterfactual_synthetic_treated_units_data_df %>% filter(start_post_event_window_numeric<=date_numeric & date_numeric<=end_post_event_window_numeric) %>% pull(stock_price)) 
gap %>% print()###
gap_bf_event %>% print()###
gap_af_event %>% print()###
cumulative_gap<-(gap^2) %>% sum()
cumulative_gap_bf_event <- (gap_bf_event^2) %>% sum()
cumulative_gap_af_event <- (gap_af_event^2) %>% sum()
cumulative_gap %>% print()###
cumulative_gap_bf_event %>% print()###
cumulative_gap_af_event %>% print()###
##################################
#後で消えちゃうので、残しておく。
gap_not_placebo<-gap
gap_bf_event_not_placebo<-gap_bf_event
gap_af_event_not_placebo<-gap_af_event
cumulative_gap_not_placebo<-cumulative_gap
cumulative_gap_bf_event_not_placebo<-cumulative_gap_bf_event
cumulative_gap_af_event_not_placebo<-cumulative_gap_af_event
##################################
#「treated unitsの合成データ」と「treated unitsの合成コントロール(反実仮想)」をグラフにしておく。
real_and_counterfactual_for_graph<-rbind(cbind(synthetic_treated_units_data_df, real_and_counterfactual_dummy_name='real_treated_units'), cbind(counterfactual_synthetic_treated_units_data_df, real_and_counterfactual_dummy_name='counterfactual_treated_units'))
ggplot() +
  geom_line(real_and_counterfactual_for_graph, mapping = aes(x = date_numeric, y = stock_price, group = real_and_counterfactual_dummy_name, color = real_and_counterfactual_dummy_name, size = real_and_counterfactual_dummy_name))+
  geom_vline(xintercept =event_day_numeric , linetype = "dashed", alpha = 0.6, size=1.5) +
  geom_vline(xintercept =start_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  geom_vline(xintercept =end_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  scale_y_continuous(
    limits = c(80, 120),
  ) +
  scale_x_continuous(
    limits = c(start_estimation_window_numeric-1, start_post_event_window_numeric+25),
  ) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title=element_blank()) +
  scale_colour_manual(values = c("gray", "black")) +
  scale_size_manual(values = c(0.8, 0.8)) +
  ggtitle("real vs counterfactual") + 
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("date")
##################################
#「treated unitsの合成データ」と「treated unitsの合成コントロール(反実仮想)」のgapをグラフにしておく。
gap_not_placebo_df<-data.frame(date_numeric=date_numeric, stock_price=gap_not_placebo)
ggplot() +
  geom_line(gap_not_placebo_df, mapping = aes(x = date_numeric, y = stock_price), size=0.8)+
  geom_vline(xintercept =event_day_numeric , linetype = "dashed", alpha = 0.6, size=1.5) +
  geom_vline(xintercept =start_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  geom_vline(xintercept =end_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  geom_hline(yintercept =0 , linetype = "longdash", alpha = 1) +
  scale_y_continuous(
    limits = c(-20, 20),
  ) +
  scale_x_continuous(
    limits = c(start_estimation_window_numeric-1, start_post_event_window_numeric+25),
  ) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title=element_blank()) +
  ggtitle("real vs counterfactual") + 
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("date")
##########################################################
##########################################################
########################################################## placebo test
number_of_attempts_placebo_test=200# placebo testを何回行うか。
too_large_mspe_exclude=5#イベント前のmspeが大きすぎるなら除外する。not placebo testのx倍以上のイベント前mspeを持つやつは除外する。
#時系列データ格納。real and counterfactual
real_and_counterfactual_df_placebo<-data.frame()
henkaritu_waru_henkaritu_placebo<-data.frame()
#placebo testにおける、gapの時系列データを格納するデータフレーム。最後一番重要なグラフを書くために使う。
gap_df_placebo<-data.frame()
#placebo testにおける、cumulative gapを格納する。
cumulative_gap_of_each_placebo_test<-data.frame()
# placebo testの繰り返し。
error_counter<-0
success_counter<-0
for (k in 1:number_of_attempts_placebo_test){
  #エラーが起きたら実行中のプラセボテストをスキップするためにtrycatch関数を使う。エラーが起きた回数だけプラセボテストの回数が減る。
  tryCatch({
    #treated units とcontrol units のコード番号。ランダムサンプリングにおいて、treated unitsの組み合わせによっては、synthのところで無限値や欠測値が出てきてしまう。
    treated_units_placebo<-sample( control_units, length(treated_units) , replace=F) %>% sort()
    control_units_placebo<-control_units[-which(control_units %in% treated_units_placebo)] 
    ########################################################## 
    ########################################################## sc of multiple treated units
    #treated unit の繰り返し。
    treated_units_mspe<-data.frame()#treated unitそれぞれのmspe。
    control_res_df_each_treated_unit<-data.frame(date_numeric=unique(topix100$date_numeric)) #treated unitそれぞれの合成コントロール(反実仮想)を格納。この行列にウェイトをかけることで、treated unitsの合成コントロール(反実仮想)を作る。
    for (i in 1:length(treated_units)) {
      k %>% print()
      i %>% print()
      control_units_placebo %>% print()
      treated_units_placebo %>% print()
      treated_units_placebo[i] %>% print()
      dataprep.out <- dataprep(
        foo = topix100,
        predictors = c(　
          "foreign_demand",
          "equity_ratio",
          #"per",
          "pbr"
          #"accounting_period_mar",
          #"accounting_period_jun",
          #"rubber",
          #"chemical",
          #"machine",
          #"pharmaceuticals",
          #"Construction",
          #"electrical_equipment",
          #foodstuff",
          #"transportation_equipment",
          #"retail_trade",
          #"textile_products",
          #"precision_equipment",
          #"wholesale_business",
          #"service",
          #"bank",
          #"petroleum_and_coal_products",
          #"rubber",
          #"steel",
          #"other_financial_services",
          #"others",
        ),
        predictors.op = "mean",　
        time.predictors.prior = start_estimation_window_numeric:end_estimation_window_numeric,     # 介入前期間の指定
        special.predictors = list(             # Z_iの調整
          list("stock_price", seq(start_estimation_window_numeric+3, start_estimation_window_numeric+3, 1), "mean"),
          list("stock_price", seq(start_estimation_window_numeric+6, start_estimation_window_numeric+6, 1), "mean"),
          list("stock_price", seq(start_estimation_window_numeric+9, start_estimation_window_numeric+9, 1), "mean"),
          list("stock_price", seq(start_estimation_window_numeric+12, start_estimation_window_numeric+12, 1), "mean")
        ),
        dependent = "stock_price",                  # 目的変数の指定 
        unit.variable = "company_code",            # 各地域を識別する変数の指定
        unit.names.variable = "company_name",
        time.variable = "date_numeric",                # 時刻を表す変数の指定#numericのみ
        treatment.identifier = treated_units_placebo[i],             # 介入群の指定
        controls.identifier = control_units_placebo,     # コントロール群の指定
        time.optimize.ssr = start_estimation_window_numeric:end_estimation_window_numeric,        # 最適化する期間の指定
        time.plot = (start_estimation_window_numeric-1):(start_post_event_window_numeric+25)              # グラフ化する際に使用する期間の指定
      )
      #論文だと、予測変数がZiで、結果変数の時系列データがXiだったと思う。逆になっている。
      #dataprep.out$X1 #treated unitの予測変数(平均値)。
      #dataprep.out$Z1 #treated unitの結果変数の時系列データ。
      #dataprep.out$X0 #control unitsの予測変数(平均値)。
      #dataprep.out$Z0 #control unitsの結果変数の時系列データ
      # 合成コントロールの実行
      synth.out <- synth(data.prep.obj = dataprep.out, optimxmethod = c("BFGS","Nelder-Mead"))
      #グラフ化
      path.plot(
        synth.res = synth.out, 
        dataprep.res = dataprep.out,
        Ylab = "stock price",
        Xlab = "date",
        Ylim = c(80, 120),
        Legend = c( paste0("treated_unit", treated_units_placebo[i]), paste("synthetic", paste0("treated_unit", treated_units_placebo[i]))), 
        Legend.position = "bottomright"
      )
      #今回のtreated unitの、実際のデータとト反実仮想のデータ(合成コントロール)の差。
      gaps.plot(
        synth.res = synth.out, 
        dataprep.res = dataprep.out,
        Ylab = "stock price", 
        Xlab = "date",
        Ylim = c(-20, 20), 
        Main = NA
      )
      #今回のtreated unitの反実仮想(合成コントロール)を作る上での、コントロール群のそれぞれの企業のウェイトかな。
      synth.tables <- synth.tab(synth.res = synth.out, dataprep.res = dataprep.out)
      w_res <- synth.tables$tab.w$w.weights
      # コントロール群のデータのみ抽出
      control_units_data <- topix100 %>% 
        filter(company_code %in% control_units_placebo) %>% 
        select(company_code, date_numeric, stock_price) %>% 
        spread(date_numeric, stock_price) %>% 
        arrange(match(company_code, topix100 %>% filter(company_code %in% control_units_placebo) %>% pull(company_code) %>% unique())) %>% 
        select(-company_code) %>%
        as.matrix()
      # 処理ユニットのデータのみ抽出
      treated_unit_data <- topix100 %>% 
        filter(company_code == treated_units_placebo[i]) %>% 
        select(company_code, date_numeric, stock_price) 
      treated_unit_data <- data.frame(
        date_numeric = treated_unit_data$date_numeric,
        stock_price = treated_unit_data$stock_price
      )
      # 行列計算を用いて加重平均を算出。今回のtreated unitの合成コントロール(反実仮想)を作る。
      control_res <- control_units_data %>% t() %*% w_res %>% t() 
      control_res <- data.frame(
        date_numeric = control_res %>% as.data.frame() %>% colnames() %>% as.integer(),
        stock_price = control_res %>% as.vector()
      )
      control_res_df_each_treated_unit<-cbind(control_res_df_each_treated_unit, yyy=control_res$stock_price)#treated unitそれぞれの合成コントロール(反実仮想)を格納。この行列にウェイトをかけることで、treated unitsの合成コントロール(反実仮想)を作る。
      # 今回のtreated unitの平均二乗誤差率(MSPE)の算出。mspe取得の期間はestimation window。
      span <- control_res %>% 
        filter(start_estimation_window_numeric <= date_numeric & date_numeric <=  end_estimation_window_numeric) %>% 
        nrow()
      treated_unit_data_bf_event <- treated_unit_data %>% 
        filter(start_estimation_window_numeric <= date_numeric & date_numeric <=  end_estimation_window_numeric) %>% 
        pull(stock_price)
      control_res_bf_event <- control_res %>% 
        filter(start_estimation_window_numeric <= date_numeric & date_numeric <=  end_estimation_window_numeric) %>% 
        pull(stock_price)
      mspe <- (  (((treated_unit_data_bf_event - control_res_bf_event)^2) %>% sum()) / (end_estimation_window_numeric-start_estimation_window_numeric+1)  ) %>% sqrt()
      # 今回のtreated unitのmspeを格納
      treated_units_mspe <- rbind(
        treated_units_mspe,
        data.frame(
          company_code = treated_units_placebo[i],
          mspe = mspe
        )
      )
      print('eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee')
    }
    treated_units_mspe　%>% print()###
    control_res_df_each_treated_unit %>% print()###
    print('dddddddddddddddddddddddddddddddddddddddddddddddddddddd')
    #上で求めたそれぞれのtreated unitのmspeを用い、てtreated unitsの中でウェイトを割り当てる。
    w_in_treated_units<-c()
    for (i in 1:nrow(treated_units_mspe)){
      w_in_treated_units<-append(w_in_treated_units ,(1/treated_units_mspe[i,2])/((1/treated_units_mspe[,2]) %>% sum()))
    }
    treated_units_mspe_weight<-cbind(treated_units_mspe, w_in_treated_units)
    treated_units_mspe_weight %>% print()###
    print('kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk')
    #treated unitsのウェイトを使って、treated unitsの合成データを作る。
    treated_units_data <- topix100 %>% 
      filter(company_code %in% treated_units_placebo) %>% 
      select(company_code, date_numeric, stock_price) %>% 
      spread(date_numeric, stock_price) %>%
      arrange(match(company_code, topix100 %>% filter(company_code %in% treated_units_placebo) %>% pull(company_code) %>% unique())) %>% 
      select(-company_code) %>%
      as.matrix()
    synthetic_treated_units_data<-treated_units_data %>% t() %*% treated_units_mspe_weight[, 'w_in_treated_units'] %>% t()
    synthetic_treated_units_data_df <- data.frame(
      date_numeric = synthetic_treated_units_data %>% as.data.frame() %>% colnames() %>% as.integer(),
      stock_price = synthetic_treated_units_data %>% as.vector()
    )
    synthetic_treated_units_data_df %>% print()###
    print('yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy')
    #treated unitsのウェイトを使って、treated unitsの合成コントロール(反実仮想)を作る。
    control_res_df_each_treated_unit_minusdate<-control_res_df_each_treated_unit[, -which (colnames(control_res_df_each_treated_unit) %in% c("date_numeric"))] %>% as.matrix()
    counterfactual_synthetic_treated_units_data<-control_res_df_each_treated_unit_minusdate %*% treated_units_mspe_weight[, 'w_in_treated_units'] 
    counterfactual_synthetic_treated_units_data_df<-data.frame(
      date_numeric=control_res_df_each_treated_unit[, 'date_numeric'],
      stock_price=counterfactual_synthetic_treated_units_data 
    )
    counterfactual_synthetic_treated_units_data_df %>% print()###
    print('uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu')
    ########################################################## 
    ########################################################## 
    #placebo testにおける、時系列データを格納するデータフレーム
    real_and_counterfactual_df_placebo<- rbind(
      real_and_counterfactual_df_placebo,
      data.frame(
        group_num=paste0('group_', (k+1)),
        date_numeric = control_res$date_numeric,
        real = synthetic_treated_units_data_df$stock_price,
        counterfactual = counterfactual_synthetic_treated_units_data_df$stock_price,
        not_placebo_or_placebo = 'placebo_test'
      )
    )
    henkaritu_waru_henkaritu_placebo<- rbind(
      henkaritu_waru_henkaritu_placebo,
      data.frame(
        group_num=paste0('group_', (k+1)),
        real_event_day_numeric_minus1= synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        real_event_day_numeric_plus0 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price),
        real_event_day_numeric_plus1 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price),
        real_event_day_numeric_plus2 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price),
        henkaritu_real_plus0 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        henkaritu_real_plus1 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        henkaritu_real_plus2 = synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        counterfactual_event_day_numeric_minus1=counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        counterfactual_event_day_numeric_plus0 =counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price),
        counterfactual_event_day_numeric_plus1 =counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price),
        counterfactual_event_day_numeric_plus2 =counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price),
        henkaritu_counterfactual_plus0 = counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        henkaritu_counterfactual_plus1 = counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        henkaritu_counterfactual_plus2 = counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price),
        compare_real_and_counterfactual_day_plus0=  ( synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price)  )  /  (  counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric  ) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price) ) -1,
        compare_real_and_counterfactual_day_plus1=  ( synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price)  )  /  (  counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+1) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price) ) -1,
        compare_real_and_counterfactual_day_plus2=  ( synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price)  )  /  (  counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric+2) %>% pull(stock_price) / counterfactual_synthetic_treated_units_data_df %>% filter(date_numeric==event_day_numeric-1) %>% pull(stock_price) ) -1,
        not_placebo_or_placebo = 'placebo_test'
      )
    )
    #「treated unitsの合成データ」と「treated unitsの合成コントロール(反実仮想)」の時系列の差と累積の差を求める。
    gap<-(synthetic_treated_units_data_df %>% pull(stock_price)) - (counterfactual_synthetic_treated_units_data_df %>% pull(stock_price))
    gap_bf_event <-  (synthetic_treated_units_data_df %>% filter(start_estimation_window_numeric<=date_numeric & date_numeric<=end_estimation_window_numeric) %>% pull(stock_price)) - (counterfactual_synthetic_treated_units_data_df %>% filter(start_estimation_window_numeric<=date_numeric & date_numeric<=end_estimation_window_numeric) %>% pull(stock_price)) 
    gap_af_event <-  (synthetic_treated_units_data_df %>% filter(start_post_event_window_numeric<=date_numeric & date_numeric<=end_post_event_window_numeric) %>% pull(stock_price)) - (counterfactual_synthetic_treated_units_data_df %>% filter(start_post_event_window_numeric<=date_numeric & date_numeric<=end_post_event_window_numeric) %>% pull(stock_price)) 
    gap %>% print()###
    gap_bf_event %>% print()###
    gap_af_event %>% print()###
    cumulative_gap<-(gap^2) %>% sum()
    cumulative_gap_bf_event <- (gap_bf_event^2) %>% sum()
    cumulative_gap_af_event <- (gap_af_event^2) %>% sum()
    cumulative_gap %>% print()###
    cumulative_gap_bf_event %>% print()###
    cumulative_gap_af_event %>% print()###
    print('aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa')
    ########################################################## 
    ########################################################## 
    #placebo testにおける、gapの時系列データを格納するデータフレーム。最後一番重要なグラフを書くために使う。
    gap_df_placebo <- rbind(
      gap_df_placebo,
      data.frame(
        group_num=paste0('group_', (k+1)),
        date_numeric = control_res$date_numeric,
        gap = gap,
        not_placebo_or_placebo = 'placebo_test',
        cumulative_gap_bf_event_col = cumulative_gap_bf_event
      )
    )
    print('gggggggggggggggggggggggggggggg')
    #placebo testにおける、cumulative gapを格納する。
    cumulative_gap_of_each_placebo_test<-dplyr::bind_rows(cumulative_gap_of_each_placebo_test, data.frame(cumulative_gap_col=c(cumulative_gap), cumulative_gap_bf_event_col=c(cumulative_gap_bf_event), cumulative_gap_af_event_col=c(cumulative_gap_af_event)))
    print('qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq')
    success_counter<-success_counter+1
    print(paste0('success_counter=', success_counter))
  }, 
  error = function(e) {
    error_counter<-error_counter+1
    print('error')
    print(paste0('error_counter=', error_counter))
  },
  warning = function(e) {
    print('warning')
  },
  finnaly = {
    #ここに記載したコードは必ず実行される
  },
  silent = FALSE
  )
}
gap_df_placebo %>% print()###
cumulative_gap_of_each_placebo_test %>% print()###
#not placebo test とplacebo testの累積のgap(mspe)のデータを合体させてデータフレームを作る。
cumulative_gap_of_not_placebo_test_and_each_placebo_test<-data.frame()
cumulative_gap_of_not_placebo_test_and_each_placebo_test<-dplyr::bind_rows(data.frame(cumulative_gap_col=c(cumulative_gap_not_placebo), cumulative_gap_bf_event_col=c(cumulative_gap_bf_event_not_placebo), cumulative_gap_af_event_col=c(cumulative_gap_af_event_not_placebo)), cumulative_gap_of_each_placebo_test)
not_placebo_or_placebo_dummy<-c()
for (l in 1:nrow(cumulative_gap_of_not_placebo_test_and_each_placebo_test)){
  if (l == 1) {
    not_placebo_or_placebo_dummy<-append(not_placebo_or_placebo_dummy, 1)
  } else {
    not_placebo_or_placebo_dummy<-append(not_placebo_or_placebo_dummy, 0)
  }
}
cumulative_gap_of_not_placebo_test_and_each_placebo_test<-cbind(cumulative_gap_of_not_placebo_test_and_each_placebo_test, not_placebo_or_placebo_dummy) 
cumulative_gap_of_not_placebo_test_and_each_placebo_test %>% print()###
#not placebo test とplacebo testにおける、gapの時系列データを格納するデータフレーム。最後一番重要なグラフを書くために使う。
gap_df<-dplyr::bind_rows(
  data.frame(
    group_num=c(paste0('group_', 1)),
    date_numeric = control_res$date_numeric,
    gap = gap_not_placebo,
    not_placebo_or_placebo = 'not_placebo_test',
    cumulative_gap_bf_event_col = cumulative_gap_bf_event_not_placebo
    ),
  gap_df_placebo
)
#placebo testのtreated unitsのイベント前(before event) の累積のgap(mspe)が真のtreated unitsのgap(mspe)の5倍よりも大きい場合は、placebo testとして認めない。つまり除外する。
#「(下の数)+(上の数)-1」でsuccess_counterと同じになる。「(下の数)-1」がplacebo_testの回数になる。「(上の数)」がmspeでかすぎてはじかれた個数。
                 gap_df %>% filter(cumulative_gap_bf_event_col >= cumulative_gap_bf_event_not_placebo*too_large_mspe_exclude) %>% pull(group_num) %>% unique() %>% length()
                 gap_df %>% filter(cumulative_gap_bf_event_col <  cumulative_gap_bf_event_not_placebo*too_large_mspe_exclude) %>% pull(group_num) %>% unique() %>% length()
gap_df_low_mspe<-gap_df %>% filter(cumulative_gap_bf_event_col <  cumulative_gap_bf_event_not_placebo*too_large_mspe_exclude)
View(gap_df_low_mspe) ###
##########################################################
##########################################################
########################################################## p値を求める。#順番検定にする。両側検定なので絶対値を使用。
#event_day_plus_0
gap_real_test_plus0 <-            gap_df_low_mspe %>% filter(date_numeric == event_day_numeric) %>% pull(gap) %>% head(1) 
gap_real_test_abs_plus0 <-        gap_df_low_mspe %>% filter(date_numeric == event_day_numeric) %>% pull(gap) %>% head(1)  %>% abs()
count_placebo_for_pvalue_plus0 <- gap_df_low_mspe %>% filter(date_numeric == event_day_numeric) %>% pull(gap) %>% tail(-1) %>% length()
count_bigger_for_pvalue_plus0<- ((gap_df_low_mspe %>% filter(date_numeric == event_day_numeric) %>% pull(gap) %>% tail(-1) %>% abs()) > gap_real_test_abs_plus0) %>% sum()#個数が出るんだよ。
p_value_plus0<-count_bigger_for_pvalue_plus0 / count_placebo_for_pvalue_plus0 
#event_day_plus_1
gap_real_test_plus1 <-            gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+1) %>% pull(gap) %>% head(1)  
gap_real_test_abs_plus1 <-        gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+1) %>% pull(gap) %>% head(1)  %>% abs()
count_placebo_for_pvalue_plus1 <- gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+1) %>% pull(gap) %>% tail(-1) %>% length()
count_bigger_for_pvalue_plus1<- ((gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+1) %>% pull(gap) %>% tail(-1) %>% abs()) > gap_real_test_abs_plus1) %>% sum()#個数が出るんだよ。
p_value_plus1<-count_bigger_for_pvalue_plus1 / count_placebo_for_pvalue_plus1
#event_day_plus_2
gap_real_test_plus2 <-            gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+2) %>% pull(gap) %>% head(1) 
gap_real_test_abs_plus2 <-        gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+2) %>% pull(gap) %>% head(1)  %>% abs()
count_placebo_for_pvalue_plus2 <- gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+2) %>% pull(gap) %>% tail(-1) %>% length()
count_bigger_for_pvalue_plus2<- ((gap_df_low_mspe %>% filter(date_numeric == event_day_numeric+2) %>% pull(gap) %>% tail(-1) %>% abs()) > gap_real_test_abs_plus2) %>% sum()#個数が出るんだよ。
p_value_plus2<-count_bigger_for_pvalue_plus2 / count_placebo_for_pvalue_plus2
##########################################################
##########################################################
########################################################## p値を求める。
henkaritu_waru_henkaritu_placebo_and_not_placebo<-rbind(henkaritu_waru_henkaritu_not_placebo, henkaritu_waru_henkaritu_placebo)
henkaritu_waru_henkaritu_placebo_and_not_placebo<-cbind(henkaritu_waru_henkaritu_placebo_and_not_placebo, cumulative_gap_bf_event_col=gap_df %>% filter(date_numeric==0) %>% pull(cumulative_gap_bf_event_col))
#placebo testのtreated unitsのイベント前(before event) の累積のgap(mspe)が真のtreated unitsのgap(mspe)の5倍よりも大きい場合は、placebo testとして認めない。つまり除外する。
#「(下の数)+(上の数)-1」でsuccess_counterと同じになる。「(下の数)-1」がplacebo_testの回数になる。「(上の数)」がmspeでかすぎてはじかれた個数。
henkaritu_waru_henkaritu_placebo_and_not_placebo %>% filter(cumulative_gap_bf_event_col >= cumulative_gap_bf_event_not_placebo*too_large_mspe_exclude) %>% pull(group_num) %>% unique() %>% length()
henkaritu_waru_henkaritu_placebo_and_not_placebo %>% filter(cumulative_gap_bf_event_col <  cumulative_gap_bf_event_not_placebo*too_large_mspe_exclude) %>% pull(group_num) %>% unique() %>% length()
henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe<-henkaritu_waru_henkaritu_placebo_and_not_placebo %>% filter(cumulative_gap_bf_event_col <  cumulative_gap_bf_event_not_placebo*too_large_mspe_exclude)
View(henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe) ###
#event_day_plus_0
henkaritu_waru_henkaritu_not_placebo_for_pvalue_0<-henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>% filter(group_num=='group_1') %>% pull(compare_real_and_counterfactual_day_plus0) 
count_placebo_0 <- henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>%  pull(compare_real_and_counterfactual_day_plus0) %>% tail(-1) %>% length()
count_bigger_0<- ((henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>%  pull(compare_real_and_counterfactual_day_plus0) %>% tail(-1) %>% abs()) > henkaritu_waru_henkaritu_not_placebo_for_pvalue_0 %>% abs()) %>% sum()#個数が出るんだよ。
pvalue_0<-count_bigger_0/count_placebo_0
#event_day_plus_1
henkaritu_waru_henkaritu_not_placebo_for_pvalue_1<-henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>% filter(group_num=='group_1') %>% pull(compare_real_and_counterfactual_day_plus1) 
count_placebo_1 <- henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>%  pull(compare_real_and_counterfactual_day_plus1) %>% tail(-1) %>% length()
count_bigger_1<- ((henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>%  pull(compare_real_and_counterfactual_day_plus1) %>% tail(-1) %>% abs()) > henkaritu_waru_henkaritu_not_placebo_for_pvalue_1 %>% abs()) %>% sum()#個数が出るんだよ。
pvalue_1<-count_bigger_1/count_placebo_1
#event_day_plus_2
henkaritu_waru_henkaritu_not_placebo_for_pvalue_2<-henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>% filter(group_num=='group_1') %>% pull(compare_real_and_counterfactual_day_plus2) 
count_placebo_2 <- henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>%  pull(compare_real_and_counterfactual_day_plus2) %>% tail(-1) %>% length()
count_bigger_2<- ((henkaritu_waru_henkaritu_placebo_and_not_placebo_low_mspe %>%  pull(compare_real_and_counterfactual_day_plus2) %>% tail(-1) %>% abs()) > henkaritu_waru_henkaritu_not_placebo_for_pvalue_2 %>% abs()) %>% sum()#個数が出るんだよ。
pvalue_2<-count_bigger_2/count_placebo_2
##########################################################
##########################################################
########################################################## not placebo とplaceboのgap(mspe)を一つのグラフにまとめる。
ggplot() +
  geom_line(data.frame(group_num=factor(gap_df_low_mspe$group_num, levels=c(gap_df_low_mspe$group_num[-which(gap_df_low_mspe$group_num %in% 'group_1')] ,c('group_1')) %>% unique())) %>% cbind(gap_df_low_mspe %>% select(-group_num)), mapping = aes(x = date_numeric, y = gap, group = group_num, color = not_placebo_or_placebo, size = not_placebo_or_placebo, alpha = not_placebo_or_placebo)) +
  geom_vline(xintercept = event_day_numeric, linetype = "dashed", alpha = 0.6, size=1.5) +
  geom_vline(xintercept =start_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  geom_vline(xintercept =end_estimation_window_numeric , linetype = "longdash", alpha = 0.6) +
  scale_y_continuous(
    limits = c(-20, 20),
  ) +
  scale_x_continuous(
    limits = c(start_estimation_window_numeric-1, start_post_event_window_numeric+25),
  ) +
  theme_bw() + 
  theme(legend.position = "bottom", legend.title=element_blank()) +
  scale_colour_manual(values = c("black", "grey")) +
  scale_size_manual(values = c(0.8, 0.3)) +
  scale_alpha_manual(values = c(1, 0.5)) +
  ggtitle("gap of each test") + 
  theme(plot.title = element_text(hjust=0.5)) +
  xlab("date")






















