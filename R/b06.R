# source( file.path( i.path ,"【別表六(一) 所得税額の控除に関する明細書】.R" ) )
# 2021-09-11(土) rename 【別表六(一) 所得税額の控除に関する明細書】.R
#                    to b06.R
# 2020-09-19(土) copy o_⑥有価証券.R
#i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"

#'
#' 【別表六(一) 所得税額の控除に関する明細書】
#' HOB016_6.0.csv # 2021(令和3年4月1日以後終了事業年度又は連結事業年度分)
#' 同上           # 2020(令和2年4月1日以後終了事業年度又は連結事業年度分)
#' HOB016_5.0.csv # 2019(平成31年4月1日以後終了事業年度又は連結事業年度分)
#' HOB016_4.0.csv # 2018(平成30年4月1日以後終了事業年度又は連結事業年度分)
#'
#' 法人税申告書別表等(明細記載を要する部分)の
#' 標準フォーム等
#' (令和3年4月1日以後終了事業年度又は連結事業年度分)
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho2_beppyo_03.htm
#'
#' ○ レコードの内容及び留意事項
#'  【別表六（一） 所得税額の控除に関する明細書】
#' （令和３年４月１日以後終了事業年度分）
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho1/7/HOB016.pdf
#'
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho1/7/HOB016.pdf

#' @export
#' @param s Settlement date
#' @name b06
b06.sheet <- function( s="2021-04-01" ) {
#  if ( s >= "2020-04-01" )  return("HOB016_6.0_所得税額" )
#  else
#  if ( s >= "2019-04-01" )  return("HOB016_5.0_所得税額" )
#  else                      return("HOB016_4.0_所得税額" )

  sheet <- read.csv( system.file( "b0601_sheet.csv", package = "office" ) )

  for( i in 1:nrow( sheet ) )  if ( sheet[ i ,1 ] <= s ) break
  return( sheet[ i ,2 ] )
}

#' @export
#' @param n format
#' @name b06
b0601_init <- function( n ) {
  b0601_n <- read.csv(
    system.file( "b0601_n.csv", package = "office" ) )
  message( b0601_n[ n ,1 ]  )

	# 行列、ひな型
	switch( n
	, "1" = {
		# 個別法
#		message( "1:個別法"  )

		b06 <- data.frame( matrix(  NA ,0 ,8 ) )
		col <- read.csv(
		  system.file( "b0601_1_col.csv", package = "office" ) )
		colnames( b06 ) <- t( col )

#		colnames( b06 ) <- c( "フォーマット区分=0601-1"
#		  ,"銘柄=" ,"収入金額=" ,"所得税額="
#		  ,"配当等の計算期間="
# #		  ,"(9)のうち元本所有期間="	#項番5のうち元本所有期間を記録してください。
#		  ,"元本所有期間="	#項番5のうち元本所有期間を記録してください。
#		  ,"所有期間割合="
#		  ,"控除を受ける所得税額="
#		 )
	  }
	, "2" = {
		# 銘柄別簡便法　※種類：株式出資or受益権
	  #               ※区分：剰余金の配当、利益の配当、基金利息、剰余金の分配
	  #               ※〃  ：公社債投資信託以外の証券投資信託の収益分配金
#	  message( "2:銘柄別簡便法" )
	  b06 <- data.frame( matrix(  NA ,0 ,9 ) )
		col <- read.csv(
		  system.file( "b0601_2_col.csv", package = "office" ) )
		colnames( b06 ) <- t( col )
# 		colnames( b06 ) <- c( "フォーマット区分=0601-2"
# 		  ,"銘柄=" ,"収入金額=" ,"所得税額="
# #		  ,"配当等の計算期末の所有元本数等="
# #		  ,"配当等の計算期首の所有元本数等="
# ##		  ,"計算期末の所有元本数等="
# ##		  ,"計算期首の所有元本数等="
# 		  ,"計算期末="
# 		  ,"計算期首="
# 		  ,"((15)-(16))/2又は12="	#○ (項番5-項番6)/2又は12を記録してください。
# 											#○ マイナスの場合は「0」と記録することに留意してください。
# 		  ,"所有元本割合="	# ○ (項番6+項番7)/項番5の割合を記録してください。
# 		 							# ○ 小数点以下3位未満は切り上げることに留意してください。
# 									#  また、1を超える場合は「1」と記録することに留意してください。
# 									# (例) 0.5012(50.12%)の場合 → 0.502
# 		  ,"控除を受ける所得税額="
# 		 )
	  }
	, "3" = {
		# その他に係る控除を受ける所得税額の明細
		b06 <- data.frame( matrix(  NA ,0 ,6 ) )
		col <- read.csv(
		  system.file( "b0601_3_col.csv", package = "office" ) )
		colnames( b06 ) <- t( col )
		#		colnames( b06 ) <- c( "フォーマット区分=0601-3"
#		  ,"	支払者の氏名又は法人名 ="
#		  ,"	支払者の住所又は所在地 ="
#		  ,"	支払を受けた年月日.元号="
#		  ,"年="
#		  ,"月="
#		 )
	  }
	)
	return( b06 )
}

# ゴミかも？　#' @export
# #' @param code 銘柄
# #' @param base 配当基準日
# #' @name b06
# b06.銘柄 <- function( code ,base ) {
#   #return(
#   paste(
#     gsub( "[()]" ,"" ,code )
#     ,base
#   )
#   # )
# }

#' @export
#' @param sbi df
#' @param o 順序
#' @name b06
b06_1.sbi <- function( sbi ,o=1 ) {
  message( paste0( " p1 : "
                   ,nrow( sbi ) ," rows" ," x " ,ncol( sbi ) ," cols" ) )
  for( i in 1:ncol( sbi )){
    message( paste( "     #col" ,i
                    ,colnames( sbi[i] ) ,mode( sbi[,i]) ) )
  }
  #	print( sbi )
	b06 <- b0601_init( 1 )
#	print( b06 )
# 行数
	b06[ 1:nrow( sbi ),1] <- "0601-1" #列1 ()
#	message( paste0( " Re : " ,nrow( b06 ) ," rows ," ," 1 <- 0601-1" ) )
	message( paste0( " Re : "
	                 ,nrow( b06 ) ," rows" ," x " ,ncol( b06 ) ," cols" ) )
	#	print( b06 )
	b06[ ,2 ] <-                      #列2 () 銘柄=
	  paste( sbi[ ,2 ]    #col 2 銘柄コード character
          ,sbi[ ,11 ] ) #col 11 配当基準日 numeric
        # ,sbi[ ,11 ] |> format.Date("%Y-%m") )
	#	  b06.銘柄( sbi[ ,2 ]             #col 2 銘柄コード character
	b06[ ,3 ] <- as.integer(          #列3 (7) 収入金額=
	  gsub(
	      "," ,"" ,sbi[ ,6 ] ) )        #col 6 配当金額 numeric
	b06[ ,4 ] <- as.integer(          #列4 (8) 所得税額=
	  gsub(
	      "," ,"" ,sbi[ ,7 ] ) )        #col 7 所得税 numeric
	b06[ ,5 ] <-                      #列5 (9) 配当等の計算期間=
	  6	 # 省略値（６ヶ月)
# 並べ替え
	# b06 <- b06.o( b06 ,o )
	message( paste0( " # sort cols ," ," 2" ) )
	r <- order( b06[ ,2 ] )    #列2 () 銘柄=
	b06 <- b06[ r , ]

	for( i in 1:ncol( b06 )){
	  message( paste( "     #col" ,i
	                  ,format( colnames( b06[ i ] ) ,width=24 )
	                  ,mode( b06[ ,i ]) ) )
	}
	for( i in c( 3 ,4 ) ) {
	  #列3 (7) 収入金額=
	  #列4 (8) 所得税額=
	  message( paste0( " # sum( col " ,sprintf("%1d" ,i ) ,"(" ,i + 4 ,") ) = "  ,
	                   format(  sum( b06[ ,i ] ,na.rm=TRUE )
	                            ,big.mark="," ,width=16 ,scientific=F )
	                   ,"     #col " ,sprintf("%1d" ,i )
	                   ," " ,format( colnames( b06[i] ) ,width=16 )
	                   ," " ,mode( b06[,i] ) ) )
	}

	return( b06 )
}

#' @export
#' @param sbi df
#' @param o 順序
#' @name b06
b06_2.sbi <- function( sbi ,o=1 ) {
# message( paste0( " p1 : " ,nrow( sbi ) ," rows" ) )
  message( paste0( " p1 : "
                   ,nrow( sbi ) ," rows" ," x " ,ncol( sbi ) ," cols" ) )
  for( i in 1:ncol( sbi )){
    message( paste( "     #col" ,i
                    ,colnames( sbi[i] ) ,mode( sbi[,i]) ) )
  }

#	print( sbi )
	b06 <- b0601_init( 2 )
#	print( b06 )
# 行数
	b06[ 1:nrow( sbi ) ,1 ] <- "0601-2" #列1 () フォーマット区分
#	message( paste0( " Re : " ,nrow( b06 ) ," rows ," ," 1 <- 0601-2" ) )
	message( paste0( " Re : "
	                 ,nrow( b06 ) ," rows" ," x " ,ncol( b06 ) ," cols" ) )

#	print( b06 )
	b06[ ,2 ] <-                        #列2 () 銘柄=
	  paste( sbi[ ,2 ]    #col 2 銘柄コード character
          ,sbi[ ,11 ] ) #col 11 配当基準日 numeric
	       # ,sbi[ ,11 ] |> format.Date("%Y-%m") )
# 	b06.銘柄( sbi$"銘柄コード" ,sbi$"配当基準日"  )
	b06[ ,3 ] <- as.integer(            #列3 (13) 収入金額=
	  gsub( "," ,"" ,sbi[ ,6 ] ) )        #col 6 配当金額 numeric
	b06[ ,4 ] <- as.integer(            #列4 (14) 所得税額=
	  gsub( "," ,"" ,sbi[ ,7 ] ) )        #col 7 所得税 numeric
	b06[ ,5 ] <- as.integer(            #列5 (15) 計算期末= の元本 "C"
	  gsub( "," ,"" ,sbi[ ,5 ] ) )        #col 5 数量 numeric
# 並べ替え
	# b06 <- b06.o( b06 ,o )
	message( paste0( " # sort cols ," ," 2" ) )
	r <- order( b06[ ,2 ] )    #列2 () 銘柄=
	b06 <- b06[ r , ]

	for( i in 1:ncol( b06 )){
	  message( paste( "     #col" ,i
	                  ,format( colnames( b06[ i ] ) ,width=24 )
	                  ,mode( b06[ ,i ]) ) )
	}

	for( i in c( 3 ,4 ) ) {
	  #列3 (13) 収入金額=
	  #列4 (14) 所得税額=
	  message( paste0( " # sum( col " ,sprintf("%1d" ,i ) ,"(" ,i + 10 ,") ) = "  ,
	                   format(  sum( b06[ ,i ] ,na.rm=TRUE )
	                            ,big.mark="," ,width=16 ,scientific=F )
	                   ,"     #col " ,sprintf("%1d" ,i )
	                   ," " ,format( colnames( b06[i] ) ,width=16 )
	                   ," " ,mode( b06[,i] ) ) )
	}

return( b06 )
}

#' @export
#' @param b06_1 df 個別法
#' @param b06_2 df 銘柄別簡便法
# #' @param idou 異動明細（前期＆当期の有価証券内訳書）
#' @param i06_pt 前期
#' @param i06_ct 当期
#' @name b06
# b06_月別元本 <- function( b06_1 ,b06_2 ,i06 ) {
# b06_月別元本 <- function( b06_1 ,b06_2 ,idou ) {
# b06_MP <- function( b06_1 ,b06_2 ,idou ) {
b06_MP <- function( b06_1 ,b06_2 ,i06_pt ,i06_ct ) {
  message( paste0( " p1 : "
                   ,nrow( b06_1 ) ," rows" ," x "
                   ,ncol( b06_1 ) ," cols" ) )
  message( paste0( " p2 : "
                   ,nrow( b06_2 ) ," rows" ," x "
                   ,ncol( b06_2 ) ," cols" ) )
  message( paste0( " p3 : "
                   ,nrow( i06_pt ) ," rows" ," x "
                   ,ncol( i06_pt ) ," cols" ) )
  message( paste0( " p4 : "
                   ,nrow( i06_ct ) ," rows" ," x "
                   ,ncol( i06_ct ) ," cols" ) )

  message( paste0( "# call i06_p( i06_pt ,i06_ct ) " ) )
  i06_p <- i06_p( i06_pt ,i06_ct )                # KEY項目を抽出
  message( paste0( "# end  i06_p() " ) )

  df <- b06_2
  key <- data.frame( matrix(  NA ,nrow(b06_2) ,6 ) )
  message( paste0( " Re : "
                   ,nrow( key ) ," rows" ," x " ,ncol( key ) ," cols" ) )
  colnames( key ) <- c( "code=","base="
                        ,"B=","T="
                        ,"M=" ,"P=" )
  ve <- strsplit( b06_2[ ,2 ] ," " ) #列2 () 銘柄=
  key[ ,1:2 ] <- matrix( unlist(ve) ,ncol = 2 ,byrow=TRUE )
  #  ve[[1]][ 1:2 ]
  #l <- length( ve[[  ]] )
  #l <- length( ve )
  #  key <- data.frame( matrix( unlist(ve) ,ncol = 2 ,byrow=TRUE ) )
  # mode( key[,2])
  key[ ,2] <- as.Date( key[,2] )
  key[ ,2] <- gsub( "-0" ,"-" ,key[ ,2] )    # 前ゼロ削除

  # key[ ,3] <- key[,2] - as.integer( format( key[,2] ,"%d" ) ) + 1
  B1m <- key[ ,2]
  B1m <- format(                     # 上記の月初日
    as.Date( B1m ) -
    as.integer( format( as.Date( B1m ) ,"%d" ) ) + 1
                        ,"%Y-%m-%d" )
  B1m <- gsub( "-0" ,"-" ,B1m )      # 前ゼロ削除

  key[ ,3] <- b06_2[ ,5 ]            #列5 (15) 計算期末= の元本 "C"
  key[ ,4] <- b06_1[ ,5 ]            #列5 (9) 配当等の計算期間=

  # key <-cbind( key ,月初日 ,"計算期間" = b06_1$`配当等の計算期間=`)
  #  seq( key[ 63 ,"月初日"] ,by = "-1 month" ,len=6 )
  #ve <-  seq( key[ 63 ,"月初日"] ,by = "-1 month" ,len=key[ 63 ,"計算期間"] )
  #月別 <- list(ve)
  #ve <-  seq( key[ 62 ,"月初日"] ,by = "-1 month" ,len=key[ 62 ,"計算期間"] )
  #月別 <- rbind( 月別 ,list(ve) )
  #月別[[2]]
  #mode( 月別 );mode( 月別[[2]])
  #plan A
  #  月別 <- as.list( NULL )
  #  for( i in 1:nrow( key ) ) {
  #    ve <- seq( key[ i ,"月初日"] ,by = "-1 month" ,len=key[ i ,"計算期間"] )
  #    if ( i == 0 ) {
  #      月別 <- list(ve)
  #    }
  #    else {
  #      月別 <- rbind( 月別 ,list(ve) )
  #    }
  #  }
  #plan B
  #  月別 <- as.list( NULL )
  #  for( i in 1:nrow( key ) ) {
  #    ve <- seq( key[ i ,"月初日"] ,by = "-1 month" ,len=key[ i ,"計算期間"] )
  #    月別 <- rbind( 月別 ,list(ve) )
  #  }
  #  月別[1:nrow(月別)]
  #  mode(ve) ; ve <- as.Date(ve)
  # nrow(月別)
  # nrow(key)

  M <- as.list( NULL )
  for( i in 1:nrow( key ) ) {
    ve <- seq(
      as.Date( B1m[ i ] )
      ,by = "-1 month"
      ,len=key[ i ,"T="] )
    ve <- gsub( "-0" ,"-"
              # ,format( ve ,"%Y-%m-%d" ) )    # 前ゼロ削除
              # ,format( ve ,"%Y-%m" ) )       # 前ゼロ削除
                ,format( ve ,"%y-%m" ) )       # 前ゼロ削除 年２桁
    M <- rbind( M ,list(ve) )
  }
  # 月別[1:nrow(月別)]
  # mode( 月別 )
  key[ ,5] <- list( M )
  #
  # 第３パラメタ
#  # if ( isFALSE( i06_check_異動明細( i06 ) ) )
#  if ( isFALSE( i06_check_idou( i06 ) ) )
#    return( key )
#  # idou <- i06_異動明細( i06 )
#  message( paste0( "# call i06_idou( p3 ) " ) )
#  idou <- i06_idou( i06 )
#  message( paste0( "# end  i06_idou( ) " ) )

# 月初残高 <- as.list( NULL )
  P <- as.list( NULL )
  # for( i in 1 ) {
  for( i in 1:nrow( key ) ) {
    ve <- rep( 0 ,length=key[ i ,"T=" ] )
    for( j in 1:key[ i ,"T="] ) {
      k <- which(  i06_p[,1] ==       #col 1 種類= character
                                key[ i ,1 ]
        & as.Date( i06_p[,2] ) >=       #col 2 異動年月日= character
              # as.Date( key[ i ,"M=" ][[ 1 ]][ j ] )  # from 月初
          as.Date( paste0( "20" ,
                                key[ i ,"M=" ][[ 1 ]][ j ] ,"-1" )
                         )  # from 月初
        & as.Date( i06_p[,2] ) <=       #col 2 異動年月日= character
          as.Date(              key[ i ,2 ] )       #  to  基準日
      )
#              &  !( idou[,4] == 0    # 割当株式、対象外
#                    & idou[,3] >  0 )
      Amt <- key[ i ,"B=" ] - sum( i06_p[ k ,3 ] ) #col 3 数量=
      ve[j]  <-Amt
    }
#    月初残高 <- rbind( 月初残高 ,list( ve ) )
    P <- rbind( P ,list( ve ) )
  }
#  key[ ,"元本" ] <- list( 月初残高 )
  key[ ,"P=" ] <- list( P )

  for( i in 1:ncol( key )){
    message( paste( "     #col" ,i
                    ,format( colnames( key[ i ] ) ,width=24 )
                    ,mode( key[ ,i ]) ) )
  }
  return( key )
}

#' @export
#' @param b06_2 df 銘柄別簡便法
#' @param MP 異動明細（前期＆当期の有価証券内訳書）
#' @name b06
# b06_2_控除所得税額 <- function( b06_2 ,MP ) {
b06_2_De <- function( b06_2 ,MP ) {
  message( paste0( " p1 : "
                   ,nrow( b06_2 ) ," rows" ," x " ,ncol( b06_2 ) ," cols" ) )
  message( paste0( " p2 : "
                   ,nrow( MP ) ," rows" ," x " ,ncol( MP ) ," cols" ) )
  # 項番6:(16)
  for (i in 1:nrow( b06_2 ) ) {
    m <- MP[ i ,"T=" ]                   #col 4 T= 計算期間
#    print( m ) #D
#    print( MP[ i ,"P="][[1]][ m ] ) #D
    b06_2[ i ,6 ] <-                   #列 6(16) 計算期首=
      MP[ i ,"P="][[1]][ m ]
  }
#  print( b06_2[ ,6 ] ) #D

  # 項番7:(17)
  # ○ (項番5－項番6）／2又は12を記録してください。
  # （決め打ち) 2
  b06_2[ ,7 ] <-                       #列 7(17) ((15)-(16))/2又は12=
    ( b06_2[ ,5 ] -                      #列 5(15) 計算期末=
      b06_2[ ,6 ] ) / 2                  #列 6(16) 計算期首=

  # ○ マイナスの場合は｢0｣と記録することに留意してください｡
  i <- ( b06_2[ ,7 ] < 0 )            #列 7(17) ((15)-(16))/2又は12=
  b06_2[ i ,7 ] <- 0                  #列 7(17) ((15)-(16))/2又は12=

  # 項番8:(18)
  # ○ （項番6＋項番7）／項番5の割合を記録してください。
  b06_2[ ,8 ] <-                      #列 8(18) 所有元本割合=
    ( b06_2[ ,6 ] +                     #列 6(16) 計算期首=
      b06_2[ ,7 ] ) /                   #列 7(17) ((15)-(16))/2又は12=
      b06_2[ ,5 ]                       #列 5(15) 計算期末=

  # ○ 小数点以下3位未満は切り上げることに留意してください｡
  b06_2[ ,8 ] <-                      #列 8(18) 所有元本割合=
    ceiling( b06_2[ ,8 ] *              #列 8(18) 所有元本割合=
               1000 ) / 1000
  #   また､1を超える場合は｢1｣と記録することに留意してください｡
  #  （例） 0.5012（50.12％）の場合 → 0.502
  i <- ( b06_2[   ,8 ] > 1 )          #列 8(18) 所有元本割合=
         b06_2[ i ,8 ] <- 1

  # 項番9:(19)
  b06_2[ ,9 ] <-                      #列 9(19) 控除を受ける所得税額=
    floor( b06_2[ ,4 ] *                #列 4(14) 所得税額=
             b06_2[ ,8 ] )              #列 8(18) 所有元本割合=

  message( paste0( " Re : calc cols , 6 7 8 9 " ) )
  #列 6(16) 計算期首=
  #列 7(17) ((15)-(16))/2又は12=
  #列 8(18) 所有元本割合=
  for( i in c( 3 ,4 ,9 )  ) {
    #列3 (13) 収入金額=
    #列4 (14) 所得税額=
    #列9 (19) 控除を受ける所得税額=
    message( paste0( " # sum( col " ,sprintf("%1d" ,i ) ,"(" ,i + 10 ,") ) = "  ,
                     format(  sum( b06_2[ ,i ] ,na.rm=TRUE )
                              ,big.mark="," ,width=16 ,scientific=F )
                     ,"     #col " ,sprintf("%1d" ,i )
                     ," " ,format( colnames( b06_2[i] ) ,width=20 )
                     ," " ,mode( b06_2[,i] ) ) )
  }

  return( b06_2 )
}

#' @export
#' @param b06_1 df
#' @param MP df
#' @name b06
# b06_1_控除所得税額 <- function( b06_1 ,月別元本 ) {
b06_1_De <- function( b06_1 ,MP ) {
  message( paste0( " p1 : "
                   ,nrow( b06_1 ) ," rows" ," x " ,ncol( b06_1 ) ," cols" ) )
  message( paste0( " p2 : "
                   ,nrow( MP ) ," rows" ," x " ,ncol( MP ) ," cols" ) )
  # 項番6:(10) (9)のうち元本所有期間
  for ( i in 1:nrow( b06_1 ) ) {
    ve <- unlist( MP[ i ,"P=" ] )
    j <- ( ve > MP[ i ,"B=" ] )
    ve[j]  <-   MP[ i ,"B=" ]

    b06_1[ i ,6 ] <-                #列6(10) 元本所有期間=
      ( sum( ve ) / MP[ i ,"B=" ] )
  }
  # 見解の相違があるかも？

  # 項番7:(11) 所有期間割合
    # ○ 項番6／項番5の割合を記録してください。
  b06_1[ ,7 ] <-                    #列7(11) 所有期間割合=
    ( b06_1[ ,6 ] /                   #列6(10) 元本所有期間=
      b06_1[ ,5 ] )                   #列5( 9) 配当等の計算期間=
    # ○ 小数点以下3位未満は切り上げることに留意してください｡
    #  （例） 0.5012（50.12％）の場合 → 0.502
  b06_1[ ,7 ] <-                    #列7(11) 所有期間割合=
    ceiling( b06_1[ ,7 ] *            #列7(11) 所有期間割合=
               1000 ) / 1000
  # 項番8:(12)
  b06_1[ ,8 ] <-                    #列8(12) 控除を受ける所得税額=
    floor( b06_1[ ,4 ] *              #列4 ( 8) 所得税額=
           b06_1[ ,7 ]  )             #列7(11) 所有期間割合=

  message( paste0( " Re : calc cols , 6 7 8" ) )
  #列 6(16) 計算期首=
  #列 7(17) ((15)-(16))/2又は12=
  #列 8(18) 所有元本割合=
  for( i in c( 3 ,4 ,8 )  ) {
    #列3 ( 7) 収入金額=
    #列4 ( 8) 所得税額=
    #列8 (12) 控除を受ける所得税額=
    message( paste0( " # sum( col " ,sprintf("%1d" ,i )
                     ,"(" ,sprintf("%2d",( i + 4 ) ) ,") ) = "  ,
                     format(  sum( b06_1[ ,i ] ,na.rm=TRUE )
                              ,big.mark="," ,width=16 ,scientific=F )
                     ,"     #col " ,sprintf("%1d" ,i )
                     ," " ,format( colnames( b06_1[i] ) ,width=20 )
                     ," " ,mode( b06_1[,i] ) ) )
  }

  return( b06_1 )
}

#' @export
#' @param b06_1 個別法
#' @param b06_2 銘柄別簡便法
#' @param i06_pt 前期
#' @param i06_ct 当期
# #' @param MP df
#' @name b06
#b0601_De <- function( b06_1 ,b06_2 ,MP ) {
b0601_De <- function( b06_1 ,b06_2 ,i06_pt ,i06_ct ) {
  message( paste0( " p1 : "
                   ,nrow( b06_1 ) ," rows" ," x " ,ncol( b06_1 ) ," cols" ) )
  message( paste0( " p2 : "
                   ,nrow( b06_2 ) ," rows" ," x " ,ncol( b06_2 ) ," cols" ) )
#  message( paste0( " p3 : "
#                   ,nrow( MP ) ," rows" ," x " ,ncol( MP ) ," cols" ) )
  # b06_1 <- b06_1_控除所得税額( b06_1 ,月別元本 )
  message( paste0( " p3 : "
                   ,nrow( i06_pt ) ," rows" ," x "
                   ,ncol( i06_pt ) ," cols" ) )
  message( paste0( " p4 : "
                   ,nrow( i06_ct ) ," rows" ," x "
                   ,ncol( i06_ct ) ," cols" ) )

  message( paste0( "# call b06_MP( b06_1 ,b06_2 ,i06_pt ,i06_ct ) " ) )
  MP <- b06_MP( b06_1 ,b06_2 ,i06_pt ,i06_ct )
  message( paste0( "# end  b06_MP() " ) )

  message( paste0( "# call b06_1_De( b06_1 ,MP ) " ) )
  b06_1 <- b06_1_De( b06_1 ,MP )
  message( paste0( "# end  b06_1_De() " ) )

  # b06_2 <- b06_2_控除所得税額( b06_2 ,月別元本 )
  message( paste0( "# call b06_2_De( b06_2 ,MP ) " ) )
  b06_2 <- b06_2_De( b06_2 ,MP )
  message( paste0( "# end  b06_2_De() " ) )

  # 有利判定
  De_1 <- sum( b06_1[,8] ) #_1 列 8(12)  控除を受ける所得税額=
  De_2 <- sum( b06_2[,9] ) #_2 列 9(19)  控除を受ける所得税額=
  b0601_init( 1 )
  message( paste0( " # sum( _1  8(12) ) ="
                   ,format(  De_1
                            ,big.mark=","
                            ,width=16
                            ,scientific=F )
                   ," , " ,colnames( b06_1[ 1 ] )
             ," " ,format( colnames( b06_1[ 8 ] )
                            ,width=26 ) ) )
  b0601_init( 2 )
  message( paste0( " # sum( _2  9(19) ) ="
                   ,format(  De_2
                            ,big.mark=","
                            ,width=16
                            ,scientific=F ) )
           ," , " ,colnames( b06_2[ 1 ] )
     ," " ,format( colnames( b06_2[ 9 ] )
                            ,width=26 ) )

  # 普通は簡便法が有利
  b0601 <- b06_2   #_2 列 9(19)  控除を受ける所得税額=
  if ( De_1 > De_2 ) {
    b0601 <- b06_1    #_1 列 8(12)  控除を受ける所得税額=
  }
  b0601[,2] <- b0601[,2] |> substr( 1 ,15 ) # 2025-09-25 add
  message( paste0( " # Advantage judgment : "
               ,colnames( b0601[ 1 ] ) , " , "
                   ,nrow( b0601 ) ," rows" ," x "
                   ,ncol( b0601 ) ," cols" ) )
return( b0601 )
}

# # ? #共通モジュール
# # ?#' @export
# #' @param 配当基準日 配当基準日
# #' @param n len
# #' @name b06
# b06_計算期間.init <- function( 配当基準日 ,n ) {
# 	# print( 配当基準日)
# 		# 列数
# 	計算期間 <- data.frame( matrix(  NA ,0 ,2 ) )
# 		# 列名
# 	colnames( 計算期間 ) <- c( "基準日" ,"元本"	)
# 		# 行数（前月末日ごと)
# 	計算期間[ 1:n ,"基準日" ] <-
# 	 as.character( seq( as.Date( 配当基準日 ) + 1  ,by = "-1 month" ,len = n )  - 1 )
# 	 # as.character( seq( as.Date( gsub( "-..$" ,"-01" ,配当基準日 ) ) ,by = "-1 month" ,len = n )  - 1 )
# return( 計算期間 )
#  }

# ?#共通モジュール
# ?#' @export
# #' @param base 配当基準日
# #' @param idou df
# #' @name b06
# 計算期間_元本 <- function( base ,idou ) {
# 	for( j in 1 : nrow( base ) ) {
# 		ve[ j ] <- 0
# 		w2 <- subset( idou ,idou[ "異動事由=" ] < as.character( as.Date( base[ j ,"基準日" ] ) + 1 ) )
# 		if( nrow( w2 ) != 0 )
# 			ve[ j ] <- as.integer( w2[ nrow( w2 ) ,"期末現在高.数量=" ] )
# 	}
# 	return( unlist( ve ) )
# }
# #	b06_2_項番17 <- function( 期末 , 期首 ) {
# #	# 項番７
# #		#		  ,"((15)-(16))/2又は12="	#○ (項番5-項番6)/2又は12を記録してください。
# #		( 期末 - 期首 ) / 2
# #	return()
# #	}

# # ?#' @export
# #' @param df df
# #' @name b06
# b06_2_所有元本割合 <- function( df ) {
# # 項番８
# 	w <- ( df[ ,"計算期首=" ] + df[ ,"((15)-(16))/2又は12=" ] ) / df[ ,"計算期末=" ]
# 	w <- ceiling( w * 1000 ) / 1000
# 		# 以上、通達通り
# return( w )
# }

# #' @export
# #' @param n num of rows
# #' @param b06_1 df
# #' @param b06_2 df
# #' @param stack df
# #' @name b06
# 照合_b06 <- function( n ,b06_1 ,b06_2 ,stack) {
# 	# n <- EQ
# 	for( i in n ) {
# 		dif <- b06_1[ i,"控除を受ける所得税額=" ] - b06_2[ i,"控除を受ける所得税額=" ]
# 			print( c(
# 				 paste( i ,b06_1[ i ,"銘柄=" ] )
# 				,paste(
# 				  b06_2[ i ,"計算期末=" ]
# 				 ,b06_2[ i ,"計算期首=" ]
# 					)
# 				,paste(
# 				 b06_1[ i ,"所有期間割合=" ] ,"vs"
# 				,b06_2[ i ,"所有元本割合=" ]
# 				,"=" ,dif )
# 				)
# 			 )
# 			print( unlist( stack[ i ,"元本" ] ) )
# 	}
# return( c( "差額(1-2)=" ,sum( b06_1[ n,"控除を受ける所得税額=" ] - b06_2[ n,"控除を受ける所得税額=" ] ) ) )
# }
# ゴミかも？　#' @export
# #' @param b06 df
# #' @param o 順序
# #' @name b06
# # 並べ替え
# b06.o <- function( b06 ,o ) {
#   #	print( o )
#   switch( o
#           ,"1" = {
#             r <- order( b06[ ,"銘柄="] )
#             return( b06[ r , ] )
#           }
#   )
#   print( o )
#   return( b06 )
# }
