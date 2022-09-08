# source( file.path( i.path ,"【別表八(一) 受取配当等の益金不算入に関する明細書】.R" ) )

# 2021-09-11(土) rename 【別表八(一) 受取配当等の益金不算入に関する明細書】.R
#                    to b08.R
# 2020-09-19(土) copy o_⑥有価証券.R
#i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"

#'
#' 【別表八(一) 受取配当等の益金不算入に関する明細書】
#' HOB806_1.0.csv  # 2022(令和4年4月1日以後終了事業年度又は連結事業年度分)
#' HOB800_16.0.csv # 2021(令和3年4月1日以後終了事業年度又は連結事業年度分)
#' HOB800_15.0.csv # 2020（令和２年)
#' HOB800_14.0.csv # 2019(平成31年4月1日以後終了事業年度又は連結事業年度分)
#' HOB800_13.0.csv # 2018(平成30年4月1日以後終了事業年度又は連結事業年度分)
#' s_b08 <- "HOB810_5.0"　# バージョン番号(_5.0)
#'
#' 法人税申告書別表等(明細記載を要する部分)の
#' 標準フォーム等
#' (令和3年4月1日以後終了事業年度又は連結事業年度分)
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho2_beppyo_03.htm
#'
#' ○ レコードの内容及び留意事項
#' 【別表八（一） 受取配当等の益金不算入に関する明細書】
#'  (令和4年4月1日以後終了事業年度又は連結事業年度分)
#'  HOB806_1.0.csv
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho1/8/HOB806.pdf
#' （令和３年４月１日以後終了事業年度分）
#'  HOB800_16.0.csv
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho1/7/HOB800.pdf
#'

#' @export
#' @param s Settlement date
#' @name b08
b08.sheet <- function( s="2022-04-01" ) {
#  if ( s >= "2021-04-01" )  return("HOB800_16.0_受取配当等の益金不算入" )
#  else
#  if ( s >= "2020-04-01" )  return("HOB800_15.0_受取配当等の益金不算入" )
#  else
#  if ( s >= "2019-04-01" )  return("HOB800_14.0_受取配当等の益金不算入" )
#  else                      return("HOB800_13.0_受取配当等の益金不算入" )

  sheet <- read.csv( system.file( "b0801_sheet.csv", package = "office" ) )

  for( i in 1:nrow( sheet ) )  if ( sheet[ i ,1 ] <= s ) break
  return( sheet[ i ,2 ] )
  }

#' @export
#' @param n format
#' @name b08
# s_b08 <- "HOB800_15.0_受取配当等の益金不算入"　# バージョン番号(_15.0) 2020（令和２年)
b0801_init <- function( n=4 ) {
  b0801_n <- read.csv(
    system.file( "b0801_n.csv", package = "office" ) )
  message( b0801_n[ n ,1 ]  )

	# 行列、ひな型
	switch( n
	, "1" = {
		#完全子法人株式等
		b08 <- data.frame( matrix(  NA ,0 ,12 ) )
#		colnames( b08 ) <- c( "フォーマット区分=0801-1"
#		 )
	  }
	, "2" = {
		# 関連法人株式等
		b08 <- data.frame( matrix(  NA ,0 ,15 ) )
#		colnames( b08 ) <- c( "フォーマット区分=0801-2"
#		 )
	  }
	, "3" = {
		# その他株式等
		b08 <- data.frame( matrix(  NA ,0 ,6 ) )
# 		colnames( b08 ) <- c( "フォーマット区分=0801-3"
#		 )
	  }
	, "4" = {
		# 非支配目的株式等
		b08 <- data.frame( matrix(  NA ,0 ,11 ) )
		col <- read.csv(
		  system.file( "b0801_4_col.csv", package = "office" ) )
		colnames( b08 ) <- t( col )
		#colnames( b08 ) <- c( "フォーマット区分=0801-4"
		#  ,"銘柄="
		#  ,"所在地="
		#  ,"基準日.元号="
		#  ,"年="
		#  ,"月="
		#  ,"日="
		#  ,"保有割合="
		#  ,"受取配当等の額="
		#  ,"益金算入="
		#  ,"益金不算入="
		# )
	  }
	)
return( b08 )
}

#' @export
#' @param sbi df
#' @param o 順序
#' @name b08
b08_4.sbi <- function( sbi ,o=1 ) {
  message( paste0( " p1 : " ,nrow( sbi ) ," rows" ) )
  for( i in 1:ncol( sbi )){
    message( paste( "     #col" ,i
                    ,colnames( sbi[i] ) ,mode( sbi[,i]) ) )
  }

	b08 <- b0801_init( 4 )               # 非支配
	# 行数
#	b08[ 1:nrow( sbi ) ,1 ]	<- "0801-4" #列1 ()
#	message( paste0( " Re : " ,nrow( b08 ) ," rows ," ," 1 <- 0801-4" ) )
	b08[ 1:nrow( sbi ) ,1 ]	<- "0801_01-4" #列1 ()
	message( paste0( " Re : " ,nrow( b08 ) ," rows ," ," 1 <- 0801_01-4" ) )
	#	列
	message( paste0( " #  set cols ," ," 2 3 4:7 8 9" ) )
	b08[ ,2 ]    <-            #列2 () 銘柄=
	  sbi[ ,2 ]                  #col 2 銘柄コード character
	b08[ ,3 ]  <-              #列3 (38) 所在地=
	  sbi[ ,1 ]           #col 1 銘柄名 character

	b08[ ,4:7 ] <- gymd_y4md(  #列4:7 (39) 基準日
	  sbi[ ,11 ] )               #col 11 配当基準日 numeric
	# ワーク（計算用紙）
	b08[ ,8 ] <- as.integer(   #列8 (40) 保有割合= （計算用紙）
	  gsub(
      "," ,"" ,sbi[ ,5 ] ) )   #col 5 数量 numeric
	message( paste0( " #    ( col 8 is C ,sbi col 5 )" ) )

	b08[ ,9 ] <- as.integer(   #列9 (41) 受取配当等の額=
	  gsub(
	    "," ,"" ,sbi[ ,6 ] ) )   #col 6 配当金額 numeric
# 並べ替え
	# b08 <- b08.o( b08 ,o )
	message( paste0( " # sort cols ," ," 2" ) )
	r <- order( b08[ ,2 ] )    #列2 () 銘柄=
	b08 <- b08[ r , ]

	return( b08 )
}

#' @export
#' @param b08_4 df
#' @param p 異動明細（前期＆当期の有価証券内訳書）
#' @name b08
# b08_4.基準日以前１月以内に取得 <- function( b08_4 ,i06 ) {
# b08_4.B1m <- function( b08_4 ,idou ) {
b08_4.B1m <- function( b08_4 ,p ) {
  # 第１パラメタ
  message( paste0( " p1 : "
                   ,nrow( b08_4 ) ," rows" ," x "
                   ,ncol( b08_4 ) ," cols" ) )

  message( paste0( " p2 : "
                   ,nrow( p ) ," rows" ," x "
                   ,ncol( p ) ," cols" ) )


  B <- data.frame( matrix(  NA ,nrow( b08_4 ) ,9 ) )
  message( paste0( " Re : "
                   ,nrow( B ) ," rows" ," x "
                   ,ncol( B ) ," cols" ) )

  # colnames( B ) <- c( "銘柄=","基準日","B1m","C","A","B" )
  colnames( B ) <- c( colnames( b08_4[ 2 ] )      #列2 () 銘柄=
                      ,"base=","C","B1m","A","B","A2m","D","E" )
  # 列数
  B[,1] <- b08_4[ ,2 ]                 #列2 () 銘柄=
  B[,"base="] <- y4md_gymd( b08_4[ ,4:7 ] ) #列4:7 (39) 基準日
  B[,3] <- b08_4[ ,8 ]                 #列8 (40) 保有割合= （計算用紙）
  B[,4] <- format(                    # 上記の月初日
    as.Date( B[,"base=" ] ) -
    as.integer( format( as.Date( B[,"base="] ) ,"%d" ) ) + 1
    ,"%Y-%m-%d" )
  B[,4] <- gsub( "-0" ,"-" ,B[,4] )    # 前ゼロ削除

  for( i in 1:4 ){
    message( paste( "     #col" ,i
                    ,colnames( B[i] ) ,mode( B[,i]) ) )
  }

  # 第２パラメタ
#  # if ( isFALSE( i06_check_異動明細( i06 ) ) )
#  if ( isFALSE( i06_check_idou( i06 ) ) ) return( B )
#
#  # idou <- i06_異動明細( i06 )
#  message( paste0( "# call i06_idou( p2 ) " ) )
#  idou <- i06_idou( i06 )
#  message( paste0( "# end  i06_idou( ) " ) )

#  idou <-  y4md_gymd( i06[ ,9:12 ] )  # `期中増(減).異動年月日.元号=`
#  idou <-  cbind( i06$`種類=`
#                 ,idou    # `期中増(減).異動年月日.元号=`
#                 ,i06$`数量=`
#                 ,i06$`金額=`
#            )

  message( paste0( " Re : calc cols , 5 6 7 8 9 " ) )
  # for( i in 1:length( B ) )
  for( i in 1:nrow( B ) )
    {
    j <- which( p[,1] == B[ i,1 ] #col 1 銘柄= character
        & as.Date( p[,2] ) >=
          as.Date( B[ i,"B1m" ] )  #col 3 B1m numeric
        & as.Date( p[,2] ) <=
          as.Date( B[ i,"base=" ] )  #col 2 base= character
      #  &  !( idou[,4] == 0    # 割当株式、対象外
      #      & idou[,3] >  0 )
         )
#        異動明細 <- subset( i06
#    ,( i06$"種類="                   == b08_4[ i,"銘柄=" ]
#     & i06$"期中増(減).異動年月日.元号=" == b08_4[ i,"基準日.元号="]
#     & i06$"年="                         == b08_4[ i,"年="]
#     & i06$"月="                         == b08_4[ i,"月="]
#    ) )

    if ( length( j ) == 0 )
    {
#      b08[ i ,"保有割合="]   <- NA
#      b08[ i ,"益金算入=" ]   <- 0
#      b08[ i ,"益金不算入=" ] <- b08[ i  ,"受取配当等の額=" ]
      B[ i,"A"] <- B[ i,"C"]
      B[ i,"B"] <- 0
      next
    }
    B[ i,"A"] <- B[ i,"C"] -
                 sum( p[ j ,3 ] ) #col 3 数量=
#      print(     sum( sel[ ,3 ] ) ) #D
    sel <- p[ j , ]
    k <- which( sel[ ,3 ] > 0 )
    B[ i,"B"] <- sum( sel[ k ,3 ] )
#    print(       sum( sel[ k ,3 ] ) ) #D
  }
  # 元"E"
  B[,"A2m"] <-  as.Date( B[,"base="] )  + 1 #基準日・翌月初日
  for( i in 1:nrow(B) ) {
    ve <- seq( B[ i ,"A2m" ] ,by = "2 month" ,len=2 )
    B[ i ,"A2m" ] <- ve[2]
  }
  B[,"A2m"] <- format( B[,"A2m"],"%Y-%m-%d" )
  B[,"A2m"] <- gsub( "-0" ,"-" ,B[,"A2m"] )    # 前ゼロ削除

  for( i in 1:nrow( B ) )
  {
    j <- which( p[,1] == B[ i ,1 ]   #col 1 銘柄= character
                & as.Date( p[,2] ) >
                  as.Date( B[ i ,"base=" ] ) #col 2 base= character
                & as.Date( p[,2] ) <
                  as.Date( B[ i ,"A2m" ] ) #col 3 A2m numeric
              #  & !(  idou[,4] == 0    # 割当株式、対象外
              #        & idou[,3] >  0 )
    )

    if ( length( j ) == 0 )
    {
      B[ i,"D"] <- 0
      B[ i,"E"] <- 0
      next
    }
    #    print( ( idou[ j , ] ) )#D
    sel <- p[ j , ]
    k <- which( sel[ ,3 ] > 0  )
    B[ i,"D"] <- sum( sel[ k ,3 ] )
    k <- which( sel[ ,3 ] < 0  )
    B[ i,"E"] <- sum( sel[ k ,3 ] ) * -1
  }
  # message
  for( i in 5:ncol( B )){
    message( paste( "     #col" ,i
                    ,colnames( B[i] ) ,mode( B[,i]) ) )
  }
  return( B )
}

# ゴミかも#' @export
#' @param b08_4 df
#' @param idou 前期＆当期の有価証券内訳書
#' @name b08
# b08_4.基準日後２月以内に譲渡 <- function( b08_4 ,i06 ) {
b08_4.A2m <- function( b08_4 ,idou ) {
  # 第１パラメタ
  message( paste0( " p1 : " ,nrow( b08_4 ) ," rows" ) )

  # print(df)
  # 列数
  E <- data.frame( matrix(  NA ,nrow(b08_4) ,6 ) )
  message( paste0( " Re : " ,nrow( E ) ," rows" ) )

  E[,1] <- b08_4[,2] #列2 () 銘柄=
  E[,2] <- y4md_gymd( b08_4[ ,4:7 ] ) #列4:7 (39) 基準日
  # E[,3] <-  E[,2] - as.integer( format( B[,2] ,"%d" ) ) + 1 #基準日・月初日
  E[,3] <-  as.Date( E[,2] )  + 1 #基準日・翌月初日

  for( i in 1:nrow(E) ) {
    ve <- seq( E[ i ,3] ,by = "2 month" ,len=2 )
    E[ i ,3 ] <- ve[2]
  }
  E[,3] <- format( E[,3],"%Y-%m-%d" )
  E[,3] <- gsub( "-0" ,"-" ,E[,3] )    # 前ゼロ削除

  E[,4] <- b08_4[ ,8 ] #列8 (40) 保有割合= （計算用紙）
  #  colnames( E ) <- c( "銘柄=","基準日","A2m","C","E","D" )
  colnames( E ) <- c( colnames( b08_4[ 2 ] )       #列2 () 銘柄=
                      ,"base=","A2m","C","D","E" )
  for( i in 1:ncol( E )){
    message( paste( "     #col" ,i
                    ,colnames( E[i] ) ,mode( E[,i]) ) )
  }

  # 第２パラメタ
#  # if ( isFALSE( i06_check_異動明細( i06 ) ) )
#  if ( isFALSE( i06_check_idou( i06 ) ) )
#    return( E )
#  # idou <- i06_異動明細( i06 )
#  idou <- i06_idou( i06 )
  message( paste0( " p2 : " ,nrow( idou ) ," rows" ) )
  #  idou <-  y4md_gymd( i06[ ,9:12 ] )  # `期中増(減).異動年月日.元号=`
#  idou <-  cbind( i06$`種類=`
#                  ,idou    # `期中増(減).異動年月日.元号=`
#                  ,i06$`数量=`
#                  ,i06$`金額=`
#  )
  for( i in 1:nrow( E ) )
  {
    j <- which( idou[,1] == E[ i ,1 ]   #col 1 銘柄= character
     & as.Date( idou[,2] ) >=
                   as.Date( E[ i ,2 ] ) #col 2 base= character
     & as.Date( idou[,2] ) <
                   as.Date( E[ i ,3 ] ) #col 3 A2m numeric
          & !(  idou[,4] == 0    # 割当株式、対象外
              & idou[,3] >  0 )
           )

    if ( length( j ) == 0 )
    {
      E[ i,"D"] <- 0
      E[ i,"E"] <- 0
      next
    }
#    print( ( idou[ j , ] ) )#D
    sel <- idou[ j , ]
    k <- which( sel[ ,3 ] > 0  )
    E[ i,"D"] <- sum( sel[ k ,3 ] )
    k <- which( sel[ ,3 ] < 0  )
    E[ i,"E"] <- sum( sel[ k ,3 ] ) * -1
  }
  message( paste0( " Re : calc cols , 5 6 " ) )
  for( i in 5:ncol( E )){
    message( paste( "     #col" ,i
                    ,colnames( E[i] ) ,mode( E[,i]) ) )
  }
  return( E )
}

#' @export
#' @param b08_4 df
#' @param i06_pt 前期
#' @param i06_ct 当期
# #' @param B df b08_4.基準日以前１月以内に取得()
# #' @param E df b08_4.基準日後２月以内に譲渡()
#' @name b08
#  exclusion of dividend received from gross revenue
# b08_4.益金不算入 <- function( b08_4 ,B ,E ) {
# b08_4.ex <- function( b08_4 ,B ,E ) {
b08_4.ex <- function( b08_4 ,i06_pt ,i06_ct ) {
  # 第１パラメタ
  message( paste0( " p1 : "
                   ,nrow( b08_4 ) ," rows" ," x "
                   ,ncol( b08_4 ) ," cols" ) )
  message( paste0( " p2 : "
                   ,nrow( i06_pt ) ," rows" ," x "
                   ,ncol( i06_pt ) ," cols" ) )
  message( paste0( " p3 : "
                   ,nrow( i06_ct ) ," rows" ," x "
                   ,ncol( i06_ct ) ," cols" ) )

  message( paste0( "# call i06_p( i06_pt ,i06_ct ) " ) )
  i06_p <- i06_p( i06_pt ,i06_ct )                # KEY項目を抽出
  message( paste0( "# end  i06_p() " ) )

  message( paste0( "# call b08_4.B1m( b08_4 ,i06_p ) " ) )
  B <- b08_4.B1m( b08_4 ,i06_p ) # 短期保有
  message( paste0( "# end  b08_4.B1m() " ) )

  b08 <- b08_4
  b08[ ,8 ]  <- NA          #列8 (40) 保有割合= （計算用紙）
  for( i in 1:nrow( b08 ) )
  {
    if ( B[ i ,"B"] == 0 )
    {
      b08[ i ,11 ] <-              #列11(43) 益金不算入=
        b08[ i ,9 ]                  #列9 (41) 受取配当等の額=
      b08[ i ,10 ] <- NA           #列10(42) 益金算入=
      next
    }
    if ( B[ i ,"E"] == 0 )
    {
      b08[ i ,11 ] <-             #列11(43) 益金不算入=
        b08[ i ,9 ]                 #列9 (41) 受取配当等の額=
      b08[ i ,10 ]  <- NA         #列10(42) 益金算入=
      next
    }
    P <- B[ i ,"E"] *
       ( B[ i ,"B"] / ( B[ i ,"B"] + B[ i ,"A"] ) ) *
       ( B[ i ,"C"] / ( B[ i ,"C"] + B[ i ,"D"] ) )
    #       ( E[ i ,"C"] / ( E[ i ,"C"] + E[ i ,"D"] ) )
    b08[ i ,10 ] <-               #列10(42) 益金算入=
      b08[ i ,9 ] * P / B[ i ,"C"]  #列9 (41) 受取配当等の額=
    b08[ i ,11 ] <-               #列11(43) 益金不算入=
      b08[ i ,9 ] -                 #列9 (41) 受取配当等の額=
      b08[ i ,10 ]                  #列10(42) 益金算入=
  }
  message( paste0( " Re : init cols , 8 " ) )
  message( paste0( " Re : calc cols , 10 11 " ) )
  for( i in c( 9 ,10:11 )  ) {
    #列9 (41) 受取配当等の額=
    #列10(42) 益金算入=
    #列11(43) 益金不算入=
    message( paste0( " # sum( col "
    ,sprintf("%2d" ,i ) ,"(" ,i + 32 ,") ) = "
    ,format( sum( b08[ ,i ] ,na.rm=TRUE )
             ,big.mark=","
             ,width=16
             ,scientific=F )
    ,"     #col "
    ,sprintf("%2d" ,i )
    ," " ,format( colnames( b08[i] )
                  ,width=16 )
    ," " ,mode( b08[,i] ) ) )
  }

#  for( i in c( 8 ,10:ncol( b08 ) ) ) {
#    message( paste( "     #col" ,i
#                    ,colnames( b08[i] ) ,mode( b08[,i]) ) )
#  }
#  message( paste0( "     # sum( col  9 (41) ) = " ) ,
#           format( sum( b08[ ,9 ] )      #列9 (41) 受取配当等の額=
#                   , big.mark="," , scientific=F ) )
#  message( paste0( "     # sum( col 10 (42) ) = " ) ,
#           format( sum( b08[ ,10 ] )     #列10(42) 益金算入=
#                   , big.mark="," , scientific=F ) )
#  message( paste0( "     # sum( col 11 (43) ) = " ) ,
#           format( sum( b08[ ,11 ] )     #列11(43) 益金不算入=
#                   , big.mark="," , scientific=F ) )

  return( b08 )
}

# # ゴミかも？　#' @export
# #' @param code 銘柄
# #' @param base 配当基準日
# #' @name b08
# b08.銘柄 <- function( code ,base ) {
#  #return(
#  paste(
#    gsub( "[()]" ,"" ,code )
#    ,base
#  )
#  # )
# }
