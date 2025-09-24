library(testthat)
library(office)

test_check("office")

決算日 <- "2020-8-31"
FY <- "/Users/nt1969m/OneDrive/office/確定申告/23期-2020.08.31"
FY1  <- "/Users/nt1969m/OneDrive/office/確定申告/22期-2019.08.31"
dir.create( paste0( FY ,"/tests" ) )
setwd( paste0( FY ,"/tests" ) )
getwd()

# 【⑥有価証券の内訳書】
  # 約定履歴
d <- file.path(	FY ,"B_0988_SBI証券" )
f <- "約定_すべての商品_2020-08-31"     # 拡張子(.csv)不要
sbi_idou <- Read_MS( d ,f ,n=8 )

i06_sbi <- i06_sbi_idou( sbi_idou )

  # 株式分割
# i06_As <- i06_init() # ←割当株式が無い場合は、こちらを実行してください。

# PDF（SBI証券、割当株式等のお知らせ）からCSV（テキスト）を作成する
library(sbitools) # 参照 https://qiita.com/nt1969m/items/890846d529d08c8edbb2
d <- file.path(	FY
                ,"B_0988_SBI証券"
                ,"割当株式" )
sbi_As  <- As( d )

i06_As <- i06_sbi_As( sbi_As )

  # (4)銘柄別、(9:17)「期中増（減）の明細」　※(4)種類 に銘柄コードを設定済み
i06_期中 <- i06_merge( i06_sbi ,i06_As )
# i06_期中[ c(4,5,13:15,17)] # (17)住所は計算用紙（売却時の入金額）

# 前期の⑥有価証券を取り込み
# i06_前期 <- i06_init() # ←前期が無い場合は、こちらを実行してください。
d1 <- file.path(	FY1 ,"tests" )
f1 <- "HOI060_4.0_⑥有価証券" # 拡張子(.csv)不要
  # f1 <- "HOB800_14.0_受取配当等の益金不算入" # test
  # f1 <- "HOB016_6.0_所得税額" # test
i06_前期 <- Read_eTax( d1 ,f1 )
#stop("the script ends") # 要確認

  # 株式交換 2019-3-31 昭和シェル石油(5002)300 to 出光興産(5019)123
  #Xs <- data.frame( matrix(
  #  c( 5002 ,300 ,"2019-3-31" ,"交換" ,5019 ,123 ,"出光興産" )
  #  ,1 ,7 ) )
  #i06_Xs <- i06_Xs( Xs ,i06_前期
  #                     ,i06_期中 )
# # stop("the script ends") # 要確認

  #i06_期中 <- i06_merge( i06_期中 ,i06_Xs )
  # stop("the script ends") # 要確認

# 当期の帳簿価額
i06_0 <- i06_Book_value( i06_前期
                        ,i06_期中 )
# # i06_0[ ,c(2,4,5,6,8,13:15) ]
# i <- grep( 5002 ,i06_0[,4] )                   #列4 種類=
# i <- c(i,grep( 5019 ,i06_0[,4] ) )                   #列4 種類=
# # i06_0[ i,c(4,5,9:12,13:15,17)]

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
d <- file.path(	FY  ,"B_0988_SBI証券" ,"配当" )
sbi_Div  <- Div( d )
#
b08_4 <- b08_4.sbi( sbi_Div ) # 非支配 case of SBI証券
#
b08 <- b08_4.ex( b08_4 ,i06_前期 ,i06_当期 )
#
Write_eTax( b08 ,b08.sheet( 決算日 ) )
# 検証
i06_p <- i06_p( i06_前期 ,i06_当期 )                # KEY項目を抽出
B <- b08_4.B1m( b08_4 ,i06_p ) #

# 【別表六(一) 所得税額の控除に関する明細書】

# フォーマット変換、ノーマライズ、正規化
# case of SBI証券
#  f <- file.path(	FY  ,"B_0988_SBI証券" ,"配当" ,"株式等配当金")
#  sbi_Div <-  read.csv( paste0( f ,e )  )
# case of SBI証券
b06_1 <- b06_1.sbi( sbi_Div ) # 1:個別法 # 計算期間を確認 ※省略値６ヶ月
              # b06_1[ ,c(2,5) ] # b06_1[ ,c("銘柄=","配当等の計算期間=") ]
b06_2 <- b06_2.sbi( sbi_Div ) # 2:銘柄別簡便法
              # 別表6(1)所得税額の控除に関する明細書
b0601 <- b0601_De( b06_1 ,b06_2 ,i06_前期 ,i06_当期 )

Write_eTax( b0601 ,b06.sheet( "決算日" ) )
# 検証
MP <- b06_MP( b06_1 ,b06_2 ,i06_前期 ,i06_当期 )
b06_1 <- b06_1_De( b06_1 ,MP )
b06_2 <- b06_2_De( b06_2 ,MP )

# showNonASCII(x)
#sf <- system.file( "i06_sheet.csv", package = "office" )
# system.file( "i06.R", package = "office" )
#i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"
 i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"
# sf <- file.path( i.path ,"office" ,"R" )
#tools::showNonASCII( i06_init  )
sf <- file.path( i.path ,"office" ,"R" ,"i06.R")
# tools::showNonASCIIfile( sf  )
#
#FY <- "/Users/nt1969m/OneDrive/office/確定申告/24期-2021-08-31"
