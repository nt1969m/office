library(devtools)
library(office)

FY1 <- "/Users/nt1969m/OneDrive/office/確定申告/23期-2020.08.31"
FY <- "/Users/nt1969m/OneDrive/office/確定申告/24期-2021-08-31"

# dir.create( paste0( FY ,"/eTax" ) )
setwd( paste0( FY ,"/eTax" ) )
  print( c( "getwd()" ,getwd() ) )  # 目検（目視にて検証する)

# o_FD.R
  print( c( "Write_eTax()" ,Write_eTax )  )
  print( c("Read_eTax()" ,Read_eTax ) )
#  print( c( "Read_Bank()" ,Read_Bank )  )
  print( c( "Read_MS()" ,Read_MS )  )
#  print( c( "Write_会計()" ,Write_会計 )  )
  print( c( "Write_FA()" ,Write_FA )  )

# i06.R
  print( c( "i06_init()" ,i06_init )  )
  print( c( "i06_sbi_約定()" ,i06_sbi_約定 )  )
  print( c( "i06_sbi_割当()" ,i06_sbi_割当 )  )
  print( c( "i06_残高繰越()" ,i06_残高繰越 )  )
  #  print( c( "i06_期末残高()" ,i06_期末残高 )  )
  print( c( "i06_帳簿価額()" ,i06_帳簿価額 )  )
#  print( c( "i06_提出用()" ,i06_提出用 )  )
  # 前期を取り込み
  #  f_p_i06 <- file.path(	"/Users/nt1969m/OneDrive/office/確定申告/22期-2019.08.31"
#                        ,"法人税"
#                        ,"HOI060_4.0_有価証券"
#  )
  z1 <- file.path(	FY1  ,"eTax"
                        ,"HOI060_4.0_有価証券"
  )
  #  print( c( "f_p_i06" ,f_p_i06 ) )
  print( c( "z1" ,z1 ) )
  i06_1 <- Read_eTax(z1)
  colnames( i06_1 ) <- colnames( i06_init() )

# 当期を取り込み
  e <- ".csv"
  # 約定
  f <- file.path(	FY  ,"B_0988_SBI証券" ,"SaveFile_000001_000005")
#  sbi_idou <- Read_Bank(f,n=8)
  sbi_idou <- Read_MS(f,n=8)
  i06_idou <- i06_sbi_約定(sbi_idou)

    # 株式分割
  f <- file.path(	FY  ,"B_0988_SBI証券" ,"割当株式" ,"割当株式等")
  print( paste0( f ,e ) )
    # sbi_As <- Read_Bank(f,n=1)
      #  ,skip=n
      #  ,fileEncoding="CP932"
      #  ,stringsAsFactors=F
#  sbi_As  <- Read_MS( f ,n=1 )
  sbi_As  <- read.csv( paste0( f ,e ) )
  i06_As <- i06_sbi_割当( sbi_As )

  i06_Tran <- i06_merge( i06_As ,i06_idou )

# 帳簿価額の繰越
#  i06_1[i06_1$`時価及び帳簿価額=` != NA, ]
  i06_1_bal_all <-  i06_1[ ! is.na( i06_1[,8] ), ]

  i06_1_bal   <-  i06_1_bal_all[ i06_1_bal_all[,8] != 0 , ]
  sum(i06_1_bal[,"時価及び帳簿価額="])
  i06_1_bal <- i06_残高繰越( i06_1_bal )
  sum(i06_1_bal[,"金額="])

  # 2021-08-31 データ補正
  i06_1_bal[ i06_1_bal[,4] == 8035 , 5 ] <- "東京エレクトロン"

  i06 <- i06_merge( i06_1_bal ,i06_Tran)

# 当期の帳簿価額
  i06_temp <- i06_帳簿価額( i06 )

#
  nrow( i06_1 )     ;nrow( i06_Tran )
  i <- grep( "前期末" ,i06_1$`異動事由=`  )　# 前期の異動明細を補正
    nrow( i06_1[ -i,] )     ;nrow( i06_temp )
  j <- grep( "前期末" ,i06_temp$`異動事由=`  )　# 当期の異動明細を補正
    nrow( i06_1[ -i,] )     ;nrow( i06_temp[ -j,] )
  i06_10 <- i06_merge( i06_1[ -i,] ,i06_temp[ -j,] )　# 前期と当期の異動明細
    nrow( i06_10 )

  # i06_10$`月=` <- gsub( "^0" ,"" ,i06_10$`月=` )
  #i06_10$`日=` <- gsub( "^0" ,"" ,i06_10$`日=` )
  y4md_gymd( i06_10[ ,9:12 ] )  # `期中増(減).異動年月日.元号=`
  # base <- y4md_gymd( b08_4[ ,4:7 ] )
  # A1d <- base + 1
  # A2m <- seq( A1d[1]  ,by = "2 month" ,len=2 )
  # NAがないことを確認
  i06_10$`数量=`
  i06_10$`金額=`

# 【別表八(一) 受取配当等の益金不算入に関する明細書】
  # フォーマット変換、ノーマライズ、正規化
    # case of SBI証券
  f <- file.path(	FY  ,"B_0988_SBI証券" ,"配当" ,"株式等配当金")
  sbi_Div <-  read.csv( paste0( f ,e )  )
  b08_4 <- b08_4.sbi( sbi_Div ) # case of SBI証券
                              # 株数 of 基準日
  C <- b08_4[ ,"保有割合=" ]
                              # 取得株数 of 基準日以前１月以内
  # B <- b08_4.基準日以前１月以内に取得( b08_4 ,i06_10 ) # Error test
  B <- b08_4.基準日以前１月以内に取得( b08_4 ,i06_10 )
  E <- b08_4.基準日後２月以内に譲渡( b08_4 ,i06_10 )
#  P <- E[ ,"E"]　*
#    ( B[,"B"] / ( B[,"B"] + B[,"A"] ) ) *
#    ( E[,"C"] / ( E[,"C"] + E[,"D"] ) )
  b08_temp <- b08_4.益金不算入( b08_4 ,B ,E )

  sum(b08_temp$`受取配当等の額=`)
  sum(b08_temp$`益金算入=` ,na.rm=TRUE)
  sum(b08_temp$`益金不算入=`)
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
    # Write_eTax( b08_temp ,b08.sheet("2021-08-31") )

# 【別表六(一) 所得税額の控除に関する明細書】
  # フォーマット変換、ノーマライズ、正規化
  # case of SBI証券
#  f <- file.path(	FY  ,"B_0988_SBI証券" ,"配当" ,"株式等配当金")
#  sbi_Div <-  read.csv( paste0( f ,e )  )
  # case of SBI証券
  b06_1 <- b06_1.sbi( sbi_Div ) # 1:個別法
  b06_2 <- b06_2.sbi( sbi_Div ) # 2:銘柄別簡便法

  # 計算期間を確認 ※デフォルト６ヶ月
  b06_1[ ,c("銘柄=","配当等の計算期間=") ]
    # b06_1[ ,c(2,5) ]
  # 計算期間を確認 ※データ補正するなら
    # （例えば）# 計算期間が１２ヶ月の場合
    # b06_1[ b06_1$`銘柄=` == "9793 2021-02-28"
    #        ,"配当等の計算期間=" ] <- 12
  idou <- i06_異動明細( i06_10 ) # test
  idou[,2]
  月別元本 <- b06_月別元本( b06_1 ,b06_2 ,i06_10)
  月別元本
  月別元本[ ,-6 ]  # 除く（月別月初日）
  #月別元本[ ,6 ][[1]]
  lapply( 月別元本[ ,6 ] ,as.Date ) # 検証（月別を日付形式で）
    # ve <- sapply( 月別元本[ ,6 ] ,as.Date )  # 検証（月別を日付形式で）
    # as.Date( ve ,origin="1970-1-1")

  b06_2 <- b06_2_控除所得税額( b06_2 ,月別元本 )
  b06_2[ ,-c(1,8:9) ]  # 比較（期末、期首）
  b06_2[ ,-c(1,7,9) ]  # 検証（所有元本割合）
  b06_2[ ,c(3,4,8,9) ] # 比較（所得税額）
  sum( b06_2[ ,9 ] )   # 合計（所得税額）

  b06_1 <- b06_1_控除所得税額( b06_1 ,月別元本 )
  sum( b06_1[ ,8 ] )   # 合計（所得税額）

  # 月別元本[ 18,"元本"][[1]][6]
  #
  # b08_temp[ b08_temp$"保有割合=" != "" , ] # 短期保有かも？
  # b08_temp[ (b08_temp$"保有割合=" == "") , ] # 短期保有かも？
  # b08_temp[ !is.na( b08_temp$`保有割合=` ) , ] # 短期保有かも？
#  B[!is.na( B )]   # 短期保有かも？

  # y4md <- b08_temp[ ,4 ]
  # length( y4md )
  # b08_base <-  y4md_gymd( b08_temp[ ,4:7 ] ) # 基準日
  # as.Date(
  #b08_base - as.integer( format( b08_base ,"%d" ) ) + 1
  #)
# cbind( b08_temp[,c(2,5:7)] ,b08_base ,b08_temp[,c(3)] )
  # b08_temp <- b08_4.基準日後２月以内に譲渡( b08_temp ,i06_10 )
  #b08_temp[ !is.na( b08_temp$`保有割合=` ) , ] # 短期保有かも？

  #b08_temp <- b08_4.短期保有( b08_temp ,i06_10 )


  # 当期を取り込み
  #f_c_i06	<-	file.path( FY  ,"eTax" ,"HOI060_4.0_有価証券_控え" )
  #print( c( "f_c_i06" ,f_c_i06 ) )

 # f3 <- file.path( FY ,"B_0988_SBI証券" ,"約定_すべての商品_2020-08-31" )
#  # sbi3 <-Read_Bank( f3 ,n=7 )
#  print( c("f3" ,f3 ) )

  # f3
  # 預け金　/ 有価証券
  #	write.csv(
  #		 sbi3[ sell , ]
  #		,paste0( f3 ,"_売り.txt" )
  #		,row.names=F
  #		)
  # 有価証券　/　預け金
  #,paste0( f3 ,"_買い.txt" )
  # 売買損益　/　有価証券
  #,paste0( f3 ,"_損益.txt" )

