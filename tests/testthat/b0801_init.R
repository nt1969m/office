# source( file.path( i.path ,"【別表八(一) 受取配当等の益金不算入に関する明細書】.R" ) )

# 2021-09-11(土) rename 【別表八(一) 受取配当等の益金不算入に関する明細書】.R
#                    to b08.R
# 2020-09-19(土) copy o_⑥有価証券.R
#i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"

#'
#' 【別表八(一) 受取配当等の益金不算入に関する明細書】
#' HOB800_16.0.csv # 2021(令和3年4月1日以後終了事業年度又は連結事業年度分)
#' HOB800_15.0.csv # 2020（令和２年)
#' HOB800_14.0.csv # 2019(平成31年4月1日以後終了事業年度又は連結事業年度分)
#' HOB800_13.0.csv # 2018(平成30年4月1日以後終了事業年度又は連結事業年度分)
#' s_b08 <- "HOB810_5.0"　# バージョン番号(_5.0)
#'

# #' @export
# #' @param s Settlement date
# #' @name b08
# b08.sheet <- function( s="2021-04-01" ) {
#  if ( s >= "2021-04-01" )  return("HOB800_16.0_受取配当等の益金不算入" )
#  if ( s >= "2020-04-01" )  return("HOB800_15.0_受取配当等の益金不算入" )
#  if ( s >= "2019-04-01" )  return("HOB800_14.0_受取配当等の益金不算入" )
#                            return("HOB800_13.0_受取配当等の益金不算入" )
  sheet <- matrix( c(
     "2021-04-01" ,  "HOB800_16.0_受取配当等の益金不算入"
    ,"2020-04-01" ,  "HOB800_15.0_受取配当等の益金不算入"
    ,"2019-04-01" ,  "HOB800_14.0_受取配当等の益金不算入"
    ,"2018-04-01" ,  "HOB800_13.0_受取配当等の益金不算入" )
    ,4 ,2 ,TRUE )
  write.csv( sheet
             ,"b0801_sheet.csv"
             ,row.names=F )
  # }

# #' @export
# #' @param n format
# #' @name b08
# # s_b08 <- "HOB800_15.0_受取配当等の益金不算入"　# バージョン番号(_15.0) 2020（令和２年)
# b0801_init <- function( n ) {
	# 行列、ひな型
  col  <- c(
   "1:完全子法人株式等"
  ,"2:関連法人株式等"
  ,"3:その他株式等"
  ,"4:非支配目的株式等"
  )
  write.csv( col
             ,"b0801_n.csv"
             ,row.names=F )

# 	switch( n
# 	, "1" = {
		#完全子法人株式等
# 		b08 <- data.frame( matrix(  NA ,0 ,12 ) )
#		colnames( b08 ) <- c( "フォーマット区分=0801-1"
#		 )
# 	  }
# 	, "2" = {
# 		# 関連法人株式等
# 		b08 <- data.frame( matrix(  NA ,0 ,15 ) )
#		colnames( b08 ) <- c( "フォーマット区分=0801-2"
#		 )
# 	  }
# 	, "3" = {
		# その他株式等
# 		b08 <- data.frame( matrix(  NA ,0 ,6 ) )
# 		colnames( b08 ) <- c( "フォーマット区分=0801-3"
#		 )
# 	  }
# 	, "4" = {
		# 非支配目的株式等
# 		b08 <- data.frame( matrix(  NA ,0 ,11 ) )
		col  <- c(
   		  		 # "フォーマット区分=0801-4" #項番1 ( )
   		  		  "0801-4"         #項番1 ( )
   		  		 ,"銘柄="          #項番2 ( )
		  		  ,"所在地="         #項番3 (38)
		  		  # ,"基準日.元号="    #項番4 (39-1)
		  		  ,"元号="    #項番4 (39-1)
		  		  ,"年="             #項番5 (39-2)
		  		  ,"月="             #項番6 (39-3)
		  		  ,"日="             #項番7 (39-4)
		  		  ,"保有割合="       #項番8 (40)
		  		  ,"受取配当等の額=" #項番9 (41)
		  		  ,"益金算入="       #項番10(42)
		  		  ,"益金不算入="     #項番11(43)
		  		 )

		write.csv( col
		           ,"b0801_4_col.csv"
		           ,row.names=F )

# 	  }
# 	)
# return( b08 )
# }

#' @export
#' @param sbi df
#' @param o 順序
#' @name b08
b08_4.sbi <- function( sbi ,o=1 ) {
#	print( sbi )
	b08 <- b08_01.init( 4 )
# 行数
	print( paste0( "nrow = " ,nrow( sbi ) )
	      )
	b08[ 1:nrow( sbi )	,"フォーマット区分=0801-4"]	<-	"0801-4"
#	print( b08 )
	# b08[ ,"銘柄="] <- b08.銘柄( sbi$"銘柄コード" ,sbi$"配当基準日"  )
	b08[ ,"銘柄="]    <- sbi[ ,"銘柄コード" ] # (2)
	b08[ ,"所在地="]  <- sbi[ ,"銘柄名" ]     # (3)

	# b08[ ,"基準日.元号="] <- 5 # 令和
	# b08[ ,"年="] <- as.integer( format( as.Date( sbi[ ,"配当基準日" ] ) ,"%Y" ) ) - 2018
	# b08[ ,"月="] <- format( as.Date( sbi[ ,"配当基準日" ] ) ,"%b" )
	# b08[ ,"日="] <- format( as.Date( sbi[ ,"配当基準日" ] ) ,"%d" )
	b08[ ,4:7 ] <- gymd_y4md( as.Date( sbi[ ,"配当基準日" ] ) )

	b08[ ,"受取配当等の額="] <- as.integer( gsub( "," ,"" ,sbi[ ,"配当金額" ] ) )
# ワーク（計算用紙）
	b08[ ,"保有割合="] <- as.integer(  gsub( "," ,"" ,sbi[ ,"数量" ] ) )
# 並べ替え
	b08 <- b08.o( b08 ,o )
  return( b08 )
}

#' @export
#' @param b08_4 df
#' @param i06 異動明細（前期＆当期の有価証券内訳書）
#' @name b08
b08_4.基準日以前１月以内に取得 <- function( b08_4 ,i06 ) {
  print( paste( nrow( b08_4 ) ,nrow( i06 ) ) )
  # 列数
  B <- data.frame( matrix(  NA ,nrow(b08_4) ,6 ) )
  colnames( B ) <- c( "銘柄=","基準日","B1m","C","A","B" )
  B[,1] <- b08_4[,"銘柄="]
  B[,2] <-  y4md_gymd( b08_4[ ,4:7 ] ) # 基準日
  B[,3] <-  as.Date( B[,2] ) - as.integer(
    format( as.Date( B[,2] ) ,"%d" )      ) + 1
  B[,4] <- b08_4[ ,"保有割合=" ]
#  B[,6] <- B[,4]
# 第２パラメタ
  if ( isFALSE( i06_check_異動明細( i06 ) ) )
  return( B )
  idou <- i06_異動明細( i06 )
#  idou <-  y4md_gymd( i06[ ,9:12 ] )  # `期中増(減).異動年月日.元号=`
#  idou <-  cbind( i06$`種類=`
#                 ,idou    # `期中増(減).異動年月日.元号=`
#                 ,i06$`数量=`
#                 ,i06$`金額=`
#            )
  # for( i in 1:length( B ) )
  for( i in 1:nrow( B ) )
    {
    j <- (idou[,1] == B[ i,"銘柄=" ]
        & idou[,2] >= B[ i,"B1m" ]
        & idou[,2] <= B[ i,"基準日" ]
        & idou[,4] != 0 　　　　　　　# 割当株式、対象外
    )
#        異動明細 <- subset( i06
#    ,( i06$"種類="                   == b08_4[ i,"銘柄=" ]
#     & i06$"期中増(減).異動年月日.元号=" == b08_4[ i,"基準日.元号="]
#     & i06$"年="                         == b08_4[ i,"年="]
#     & i06$"月="                         == b08_4[ i,"月="]
#    ) )
    異動明細 <- idou[ j, ]

    if ( nrow( 異動明細 ) == 0 )
    {
#      b08[ i ,"保有割合="]   <- NA
#      b08[ i ,"益金算入=" ]   <- 0
#      b08[ i ,"益金不算入=" ] <- b08[ i  ,"受取配当等の額=" ]
      B[ i,"B"] <- 0
      B[ i,"A"] <- B[ i,"C"]
      next
    }
    B[ i,"B"] <- sum( 異動明細[異動明細$"数量=" > 0 ,"数量=" ] )
    B[ i,"A"] <- B[ i,"C"] - sum( 異動明細$"数量=" )
  }
  return( B )
}

#' @export
#' @param b08_4 df
#' @param i06 前期＆当期の有価証券内訳書
#' @name b08
b08_4.基準日後２月以内に譲渡 <- function( b08_4 ,i06 ) {
  # print(df)
  # 列数
  E <- data.frame( matrix(  NA ,nrow(b08_4) ,6 ) )
  colnames( E ) <- c( "銘柄=","基準日","A2m","C","E","D" )
  E[,1] <- b08_4[,"銘柄="]
  E[,2] <-  y4md_gymd( b08_4[ ,4:7 ] ) # 基準日
  # E[,3] <-  E[,2] - as.integer( format( B[,2] ,"%d" ) ) + 1 #基準日・月初日
  E[,3] <-  as.Date( E[,2] )  + 1 #基準日・翌月初日
  for( i in 1:nrow(E) ) {
    ve <- seq( E[ i ,3] ,by = "2 month" ,len=2 )
    E[ i ,3 ] <- ve[2]
  }
  E[,4] <- b08_4[ ,"保有割合=" ]
  E[,5] <- E[,4]
  E[,6] <- E[,4]
  # 第２パラメタ
  if ( isFALSE( i06_check_異動明細( i06 ) ) )
  return( E )
  idou <- i06_異動明細( i06 )
#  idou <-  y4md_gymd( i06[ ,9:12 ] )  # `期中増(減).異動年月日.元号=`
#  idou <-  cbind( i06$`種類=`
#                  ,idou    # `期中増(減).異動年月日.元号=`
#                  ,i06$`数量=`
#                  ,i06$`金額=`
#  )
  for( i in 1:nrow( E ) )
  {
    j <- (idou[,1] == E[ i,"銘柄=" ]
          & idou[,2] >= E[ i,"基準日" ]
          & idou[,2] <  E[ i,"A2m" ]
          & idou[,4] != 0 　　　　　　　# 割当株式、対象外
    )
    異動明細 <- idou[ j, ]

    if ( nrow( 異動明細 ) == 0 )
    {
      E[ i,"E"] <- 0
      E[ i,"D"] <- 0
      next
    }
    E[ i,"E"] <- sum( 異動明細[異動明細$"数量=" < 0 ,"数量=" ] ) * -1
    E[ i,"D"] <- sum( 異動明細[異動明細$"数量=" > 0 ,"数量=" ] )
  }
  return( E )
}

#' @export
#' @param b08_4 df
#' @param B df b08_4.基準日以前１月以内に取得()
#' @param E df b08_4.基準日後２月以内に譲渡()
#' @name b08
b08_4.益金不算入 <- function( b08_4 ,B ,E ) {
  b08 <- b08_4
  for( i in 1:nrow( b08 ) )
  {
    if ( B[ i ,"B"] == 0 )
    {
      b08[ i ,"益金不算入="] <- b08[ i,"受取配当等の額=" ]
      b08[ i ,"益金算入=" ]  <- NA
      b08[ i ,"保有割合=" ]  <- NA
      next
    }
    if ( E[ i ,"E"] == 0 )
    {
      b08[ i ,"益金不算入="] <- b08[ i,"受取配当等の額=" ]
      b08[ i ,"益金算入=" ]  <- NA
      b08[ i ,"保有割合=" ]  <- NA
      next
    }
    P <- E[ i ,"E"]　*
       ( B[ i ,"B"] / ( B[ i ,"B"] + B[ i ,"A"] ) ) *
       ( E[ i ,"C"] / ( E[ i ,"C"] + E[ i ,"D"] ) )
    b08[ i ,"益金算入=" ]  <- b08[ i ,"受取配当等の額="] * P / E[ i ,"C"]
    b08[ i ,"益金不算入="] <- b08[ i ,"受取配当等の額="] -
      b08[ i ,"益金算入=" ]
    b08[ i ,"保有割合=" ]  <- NA
  }
  return( b08 )
}

# ゴミかも？　#' @export
#' @param code 銘柄
#' @param base 配当基準日
#' @name b08
b08.銘柄 <- function( code ,base ) {
  #return(
  paste(
    gsub( "[()]" ,"" ,code )
    ,base
  )
  # )
}
# ゴミかも？　#' @export
#' @param b08 df
#' @param o 順序
#' @name b08
# 並べ替え
b08.o <- function( b08 ,o ) {
  #	print( o )
  switch( o
          ,"1" = {
            r <- order( b08[ ,"銘柄="] )
            return( b08[ r , ] )
          }
  )
  print( o )
  return( b08 )
}
