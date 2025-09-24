library(testthat)
library(office)

test_check("office")

決算日 <- "2018-8-31"
FY <- "/Users/nt1969m/OneDrive/office/確定申告/21期-2018.08.31"
dir.create( paste0( FY ,"/tests" ) )
setwd( paste0( FY ,"/tests" ) )
getwd()

# 【⑥有価証券の内訳書】
  # 約定履歴
d <- file.path(	FY  ,"SBI" )
f <- "約定履歴_2018-08-31"   # 拡張子(.csv)不要
sbi_idou <- Read_MS( d ,f ,n=8 )

#  # ve <- lapply( sbi_idou ,mode )
#  sapply( sbi_idou ,mode )
#  print( paste("    :",mode(sbi_idou) ) )
#  for( i in 1:ncol(sbi_idou)){
#    print( paste("     #col",i,colnames(sbi_idou[i]) ,mode(sbi_idou[,i]) ) )
##    print( paste("     #col",i,mode(sbi_idou[,i]) ) )
#  }

# i06_idou <- i06_sbi_約定( sbi_idou )
i06_idou <- i06_sbi_idou( sbi_idou )

  # 株式分割
# i06_As <- i06_init() # ←割当株式が無い場合は、こちらを実行してください。

# PDF（SBI証券、割当株式等のお知らせ）からCSV（テキスト）を作成する
library(sbitools) # 参照 https://qiita.com/nt1969m/items/890846d529d08c8edbb2
d <- file.path(	FY  ,"SBI" ,"割当株式" )
sbi_As  <- As( d )

# i06_As <- i06_sbi_割当( sbi_As )
i06_As <- i06_sbi_As( sbi_As )

  # (4)銘柄別、(9:17)「期中増（減）の明細」　※(4)種類 に銘柄コードを設定済み
i06_期中 <- i06_merge( i06_idou ,i06_As )
#i06_期中[ c(4,5,13:15,17)] # (17)住所は計算用紙（売却時の入金額）

# 前期の⑥有価証券を取り込み
i06_前期 <- i06_init() # ←前期が無い場合は、こちらを実行してください。

# colnames( i06_前期 ) <- colnames( i06_init() )

#帳簿価額の繰越
# (6:8)期末現在高 のみ抽出
# i06_前期残高 <- i06_残高繰越( i06_前期 )

# 当期の帳簿価額
# # (17)摘要=は計算用紙（売却損益）
# i06_0 <- i06_帳簿価額( i06_merge( i06_前期残高 ,i06_期中 ) )
# i06_0 <- i06_帳簿価額( i06_前期 ,i06_期中 )
i06_0 <- i06_Book_value( i06_前期 ,i06_期中 ) #
#i06_0[ ,c(2,4,5,6,8,13:15) ]

# (4)銘柄別、(6:8)「期末現在高」　
#i06_1 <- i06_合計行( i06_0 )
i06_1 <- i06_lines_sum( i06_0 )
#i06_1[ ,c(2,4,5,6,8) ]

# i <- i06_0$"行区分" == 0 # 明細行 （前期の合計行）削除
# i06_当期 <-i06_merge( i06_0[ i, ]   # 明細行のみ残す
#                      ,i06_1         # 合計行
#                      )
i06_当期 <-i06_ct( i06_0           # 明細行のみ残す（前期・合計行を削除）
                  ,i06_1 )         # 合計行

Write_eTax( i06_当期 ,i06.sheet( 決算日 ) )

i06_PL  <-i06_ct( i06_0 ,n = 2 )
write.csv( i06_PL ,"i06_PL.csv")

# 【別表八(一) 受取配当等の益金不算入に関する明細書】

# PDF（SBI証券　株式等配当金のお知らせ）からCSV（テキスト）を作成する
library(sbitools) # 参照 https://qiita.com/nt1969m/items/8e9d6f3454c2087bf6ec

# フォーマット変換、ノーマライズ、正規化
# case of SBI証券
d <- file.path(	FY  ,"SBI" ,"配当" )
sbi_Div  <- Div( d )
    # f <- file.path(	FY  ,"SBI" ,"配当" ,"株式等配当金")
    # sbi_Div <-  read.csv( paste0( f ,".csv" )  )
b08_4 <- b08_4.sbi( sbi_Div ) # 非支配 case of SBI証券
# 株数 of 基準日
# C <- b08_4[ ,"保有割合=" ]

# i06_10 <- i06_merge( i06_前期[ i06_前期$"行区分" == 0 , ]
#                     ,i06_期中 )　# 明細行（前期と当期）
# i06_当期[ i06_当期[,2] == 0　
#i06_idou <- i06_idou( rbind( i06_前期 ,i06_当期 ) ) # KEY項目を抽出
i06_p <- i06_p( i06_前期 ,i06_当期 )                # KEY項目を抽出

# B <- b08_4.基準日以前１月以内に取得( b08_4 ,i06_10 ) # Error test
# B <- b08_4.B1m( b08_4 ,i06_10 ) # b08_4.基準日以前１月以内に取得( b08_4 ,i06_10 )
# B <- b08_4.B1m( b08_4 ,i06_idou ) #
B <- b08_4.B1m( b08_4 ,i06_p ) #
# E <- b08_4.基準日後２月以内に譲渡( b08_4 ,i06_10 )
# E <- b08_4.A2m( b08_4 ,i06_idou )
#  P <- E[ ,"E"]　*
#    ( B[,"B"] / ( B[,"B"] + B[,"A"] ) ) *
#    ( E[,"C"] / ( E[,"C"] + E[,"D"] ) )
# b08_temp <- b08_4.益金不算入( b08_4 ,B ,E )
# b08 <- b08_4.ex( b08_4 ,B ,E )
# b08 <- b08_4.ex( b08_4 ,B )
b08 <- b08_4.ex( b08_4 ,i06_前期 ,i06_当期 )

#sum( b08$`受取配当等の額=` )
#sum( b08$`益金算入=` ,na.rm=TRUE )
#sum( b08$`益金不算入=` )
# 小数点の取扱い（自己責任でお願いします）
#  i <- !is.na( b08$"益金算入=" ) # 短期保有のみ再計算
#  b08_temp[ i ,"益金算入=" ] <-
#    ceiling( b08_temp[ i ,"益金算入=" ] ) # 切り上げ
#  b08_temp[ i ,"益金算入=" ] <-
#      floor( b08_temp[ i ,"益金算入=" ] ) # 切り捨て（納税者有利）
#  b08_temp[ i ,"益金不算入=" ] <-
#    b08[ i ,"受取配当等の額="] -
#    b08[ i ,"益金算入=" ]
#
Write_eTax( b08 ,b08.sheet( 決算日 ) )

# 【別表六(一) 所得税額の控除に関する明細書】

# フォーマット変換、ノーマライズ、正規化
# case of SBI証券
#  f <- file.path(	FY  ,"B_0988_SBI証券" ,"配当" ,"株式等配当金")
#  sbi_Div <-  read.csv( paste0( f ,e )  )
# case of SBI証券
b06_1 <- b06_1.sbi( sbi_Div ) # 1:個別法
# 計算期間を確認 ※省略値６ヶ月
b06_1[ ,c(2,5) ] # b06_1[ ,c("銘柄=","配当等の計算期間=") ]

b06_2 <- b06_2.sbi( sbi_Div ) # 2:銘柄別簡便法


# b06_1[ ,c(2,5) ]
# 計算期間を確認 ※データ補正するなら
# （例えば）# 計算期間が１２ヶ月の場合
# b06_1[ b06_1$`銘柄=` == "9793 2021-02-28"
#        ,"配当等の計算期間=" ] <- 12
# idou <- i06_異動明細( i06_10 ) # test
# idou <- i06_idou( i06_10 ) # test
# idou[,2]
# i06_10 <- i06_merge( i06_前期[ i06_前期$"行区分" == 0 , ]
#                      ,i06_0 )　# 明細行（前期と当期）
#月別元本 <- b06_月別元本( b06_1 ,b06_2 ,i06_10)
#月別元本 <- b06_月別元本( b06_1 ,b06_2 ,i06_idou )
# MP <- b06_MP( b06_1 ,b06_2 ,i06_idou )
MP <- b06_MP( b06_1 ,b06_2 ,i06_前期 ,i06_当期 )
# stop("the script ends")

#月別元本[ ,-5 ]  # 除く（月別月初日）
#月別元本[ ,-4 ]  # 除く
#月別元本[ ,5 ][1]
#月別元本[ ,5 ][1:5]
#月別元本[ ,5 ][[1]]
# lapply( 月別元本[ ,5 ] ,as.Date ) # 検証（月別を日付形式で）
# ve <- sapply( 月別元本[ ,6 ] ,as.Date )  # 検証（月別を日付形式で）
# as.Date( ve ,origin="1970-1-1")

# b06_2 <- b06_2_控除所得税額( b06_2 ,月別元本 )
b06_2 <- b06_2_De( b06_2 ,MP )
b06_2[ ,-c(1,8:9) ]  # 比較（期末、期首）
b06_2[ ,-c(1,7,9) ]  # 検証（所有元本割合）
b06_2[ ,c(3,4,8,9) ] # 比較（所得税額）
# sum( b06_2[ ,3 ] )   # 項番3(13)収入金額=
# sum( b06_2[ ,4 ] )   # 項番4(14)所得税額=
# sum( b06_2[ ,9 ] )   # 項番9(19)合計（所得税額）

# b06_1 <- b06_1_控除所得税額( b06_1 ,月別元本 )
# b06_1 <- b06_1_De( b06_1 ,MP )
b0601 <- b0601_De( b06_1 ,b06_2 ,i06_前期 ,i06_当期 )

# sum( b06_1[ ,4 ] )   # 項番4(8)所得税額=
# sum( b06_1[ ,8 ] )   # 項番8(12)控除を受ける所得税額=

Write_eTax( b06_2 ,b06.sheet( 決算日 ) )

# showNonASCII(x)
#sf <- system.file( "i06_sheet.csv", package = "office" )
# system.file( "i06.R", package = "office" )
#i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"
 i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"
# sf <- file.path( i.path ,"office" ,"R" )
#tools::showNonASCII( i06_init  )
sf <- file.path( i.path ,"office" ,"R" ,"b06.R")
tools::showNonASCIIfile( sf  )
#
#FY <- "/Users/nt1969m/OneDrive/office/確定申告/22期-2019.08.31"
#FY1 <- "/Users/nt1969m/OneDrive/office/確定申告/23期-2020.08.31"
#FY <- "/Users/nt1969m/OneDrive/office/確定申告/24期-2021-08-31"


