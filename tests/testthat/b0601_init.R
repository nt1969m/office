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

# #' @export
# #' @param s Settlement date
# #' @name b06
# b06.sheet <- function( s="2021-04-01" ) {
#   if ( s >= "2020-04-01" )  return("HOB016_6.0_所得税額" )
#   else
#   if ( s >= "2019-04-01" )  return("HOB016_5.0_所得税額" )
#   else                      return("HOB016_4.0_所得税額" )
sheet <- matrix( c(
   "2020-04-01" ,"HOB016_6.0_所得税額"
  ,"2019-04-01" ,"HOB016_5.0_所得税額"
  ,"2018-04-01" ,"HOB016_4.0_所得税額" )
  ,3 ,2 ,TRUE )
write.csv( sheet
           ,"b0601_sheet.csv"
           ,row.names=F )

# }

# #' @export
# #' @param n format
#' @name b06
# b06_01.init <- function( n ) {
	# 行列、ひな型
col  <- c(
   "1:個別法"
  ,"2:銘柄別簡便法"
  )
write.csv( col
           ,"b0601_n.csv"
           ,row.names=F )
switch( n
	, "1" = {
		# 個別法
		print( "1:個別法"  )
		b06 <- data.frame( matrix(  NA ,0 ,8 ) )
# 		colnames( b06 ) <- c( "フォーマット区分=0601-1"
# 		  ,"銘柄=" ,"収入金額=" ,"所得税額="
#		  ,"配当等の計算期間="
# #		  ,"(9)のうち元本所有期間="	#項番5のうち元本所有期間を記録してください。
#		  ,"元本所有期間="	#項番5のうち元本所有期間を記録してください。
#		  ,"所有期間割合="
#		  ,"控除を受ける所得税額="
#		 )
		col  <- c(
		   # "フォーマット区分=0601-1" #項番1 ( )
		   "0601-1"                  #項番1 ( )
		   ,"銘柄="                  #項番2 ( )
		  ,"収入金額="               #項番3 (7)
		  ,"所得税額="               #項番4 (8)
		  ,"配当等の計算期間="       #項番5 (9)
		  ,"元本所有期間="	        #項番6 (10) #項番5のうち元本所有期間を記録してください。
		  ,"所有期間割合="          #項番7 (11)
		  ,"控除を受ける所得税額="  #項番8 (12)
		)

		write.csv( col
		           ,"b0601_1_col.csv"
		           ,row.names=F )

	  }
	, "2" = {
		# 銘柄別簡便法　※種類：株式出資or受益権
	  #               ※区分：剰余金の配当、利益の配当、基金利息、剰余金の分配
	  #               ※〃  ：公社債投資信託以外の証券投資信託の収益分配金
	  print( "2:銘柄別簡便法" )
		b06 <- data.frame( matrix(  NA ,0 ,9 ) )
# 		colnames( b06 ) <- c( "フォーマット区分=0601-2"
#		  ,"銘柄=" ,"収入金額=" ,"所得税額="
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
		col  <- c(
		#  "フォーマット区分=0601-2"　#項番1 ( )
		    "0601-2"　                #項番1 ( )
		   ,"銘柄="                   #項番2 ( )
		   ,"収入金額="               #項番3 (13)
		   ,"所得税額="               #項番4 (14)
		  # #		  ,"配当等の計算期末の所有元本数等="
		  # #		  ,"配当等の計算期首の所有元本数等="
		   ,"計算期末="　　　　　　　 #項番5 (15)
		   ,"計算期首="               #項番6 (16)
		   ,"((15)-(16))/2又は12="	  #項番7 (17)
		   #○ (項番5-項番6)/2又は12を記録してください。
		   #○ マイナスの場合は「0」と記録することに留意してください。
		   ,"所有元本割合="	          #項番8 (18)
		   # ○ (項番6+項番7)/項番5の割合を記録してください。
		   # ○ 小数点以下3位未満は切り上げることに留意してください。
		   #  また、1を超える場合は「1」と記録することに留意してください。
		   # (例) 0.5012(50.12%)の場合 → 0.502
		   ,"控除を受ける所得税額="   #項番9 (19)
		)

		write.csv( col
		           ,"b0601_2_col.csv"
		           ,row.names=F )
	}
	, "3" = {
		# その他に係る控除を受ける所得税額の明細
		b06 <- data.frame( matrix(  NA ,0 ,6 ) )
		col <- c(
		  # "フォーマット区分=0601-3"
		   "0601-3"
		  ,"支払者の氏名又は法人名 ="
		  ,"支払者の住所又は所在地 ="
		  ,"支払を受けた年月日.元号="
		  ,"年="
		  ,"月="
		 )

		write.csv( col
		           ,"b0601_3_col.csv"
		           ,row.names=F )
	}
	)
#	return( b06 )
# }

#' @export
#' @param sbi df
#' @param o 順序
#' @name b06
b06_1.sbi <- function( sbi ,o=1 ) {
#	print( sbi )
	b06 <- b06_01.init( 1 )
#	print( b06 )
# 行数
	b06[ 1:nrow( sbi ),"フォーマット区分=0601-1"] <- "0601-1"
#	print( b06 )
	b06[ ,"銘柄="] <- b06.銘柄( sbi$"銘柄コード" ,sbi$"配当基準日"  )
	b06[ ,"収入金額="] <- as.integer( gsub( "," ,"" ,sbi[ ,"配当金額" ] ) )
	b06[ ,"所得税額="] <- as.integer( gsub( "," ,"" ,sbi[ ,"所得税" ] ) )
# 省略値
	b06[ ,"配当等の計算期間="] <- 6	 # 決め打ち（６ヶ月)
# 並べ替え
	b06 <- b06.o( b06 ,o )
	return( b06 )
}

#' @export
#' @param sbi df
#' @param o 順序
#' @name b06
b06_2.sbi <- function( sbi ,o=1 ) {
#	print( sbi )
	b06 <- b06_01.init( 2 )
#	print( b06 )
# 行数
	b06[ 1:nrow( sbi ),"フォーマット区分=0601-2"] <- "0601-2"
#	print( b06 )
	b06[ ,"銘柄="] <- b06.銘柄( sbi$"銘柄コード" ,sbi$"配当基準日"  )
	b06[ ,"収入金額="] <- as.integer( gsub( "," ,"" ,sbi[ ,"配当金額" ] ) )
	b06[ ,"所得税額="] <- as.integer( gsub( "," ,"" ,sbi[ ,"所得税" ] ) )
# 固有値
	b06[ ,"計算期末="] <- as.integer(  gsub( "," ,"" ,sbi[ ,"数量" ] ) )
# 並べ替え
	b06 <- b06.o( b06 ,o )
return( b06 )
}

#' @export
#' @param b06_1 df 個別法
#' @param b06_2 df 銘柄別簡便法
#' @param i06 異動明細（前期＆当期の有価証券内訳書）
#' @name b06
b06_月別元本 <- function( b06_1 ,b06_2 ,i06 ) {
  df <- b06_2
  key <- data.frame( matrix(  NA ,nrow(b06_2) ,7 ) )
  colnames( key ) <- c( "銘柄=","基準日"
                        ,"B1m","B","計算期間"
                        ,"月別" ,"元本" )
  ve <- strsplit( b06_2[ ,"銘柄=" ] ," " )
  key[ ,1:2 ] <- matrix( unlist(ve) ,ncol = 2 ,byrow=TRUE )
  #  ve[[1]][ 1:2 ]
  #l <- length( ve[[  ]] )
  #l <- length( ve )
  #  key <- data.frame( matrix( unlist(ve) ,ncol = 2 ,byrow=TRUE ) )
  # mode( key[,2])
  key[ ,2] <- as.Date( key[,2] )
  key[ ,3] <- key[,2] - as.integer( format( key[,2] ,"%d" ) ) + 1
  key[ ,4] <- b06_2$`計算期末=`
  key[ ,5] <- b06_1$`配当等の計算期間=`
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
  月別 <- as.list( NULL )
  for( i in 1:nrow( key ) ) {
    ve <- seq( key[ i ,"B1m"] ,by = "-1 month" ,len=key[ i ,"計算期間"] )
    月別 <- rbind( 月別 ,list(ve) )
  }
  # 月別[1:nrow(月別)]
  # mode( 月別 )
  key[ ,6] <- list( 月別 )
  key[ ,6][[1]]
  #
  # 第３パラメタ
  if ( isFALSE( i06_check_異動明細( i06 ) ) )
    return( key )
  idou <- i06_異動明細( i06 )

  月初残高 <- as.list( NULL )
  # for( i in 1 ) {
  for( i in 1:nrow( key ) ) {
    ve <- rep( 0 ,length=key[ i ,"計算期間" ] )
    for( j in 1:key[ i ,"計算期間"] ) {
      k <- (    idou[,1] == key[ i ,1 ]  　　 # "銘柄="
                & idou[,2] >= key[ i ,"月別" ][[ 1 ]][ j ]  # from 月初
                & idou[,2] <= key[ i ,2 ]       #  to  基準日
                & idou[,"金額="] != 0 　　　　　　　# 割当株式、対象外
      )
      Amt <- key[ i ,"B" ] - sum( idou[ k ,"数量=" ] )
      ve[j]  <-Amt
    }
    月初残高 <- rbind( 月初残高 ,list( ve ) )
  }
  key[ ,"元本" ] <- list( 月初残高 )
  # key[ ,-6 ]

  return( key )
}

#' @export
#' @param b06_2 df 銘柄別簡便法
#' @param 月別元本 異動明細（前期＆当期の有価証券内訳書）
#' @name b06
b06_2_控除所得税額 <- function( b06_2 ,月別元本 ) {
  # 項番6:(16)
  for (i in 1:nrow( b06_2 ) ) {
    m <- 月別元本[ i ,"計算期間" ]
    b06_2[ i ,"計算期首=" ] <- 月別元本[ i ,"元本"][[1]][ m ]
  }

  # 項番7:(17)
  # ○ (項番5－項番6）／2又は12を記録してください。
  # （決め打ち) 2
  b06_2[ ,"((15)-(16))/2又は12=" ] <-
    ( b06_2$`計算期末=` - b06_2$`計算期首=` ) / 2

  # ○ マイナスの場合は｢0｣と記録することに留意してください｡
  i <- ( b06_2[ ,"((15)-(16))/2又は12=" ] < 0 )
  b06_2[ i ,"((15)-(16))/2又は12=" ] <- 0

  # 項番8:(18)
  # ○ （項番6＋項番7）／項番5の割合を記録してください。
  b06_2[ ,"所有元本割合=" ] <-
    ( b06_2$`計算期首=` + b06_2$`((15)-(16))/2又は12=` ) / b06_2$`計算期末=`

  # ○ 小数点以下3位未満は切り上げることに留意してください｡
  b06_2[ ,"所有元本割合=" ] <-
    ceiling( b06_2[ ,"所有元本割合=" ] * 1000 ) / 1000
  #   また､1を超える場合は｢1｣と記録することに留意してください｡
  #  （例） 0.5012（50.12％）の場合 → 0.502
  i <- ( b06_2[ ,"所有元本割合=" ] > 1 )
  b06_2[ i ,"所有元本割合=" ] <- 1

  # 項番9:(19)
  b06_2[ ,"控除を受ける所得税額=" ] <-
    floor( b06_2$`所得税額=` * b06_2$`所有元本割合=` )

  return( b06_2 )
}

#' @export
#' @param b06_1 df
#' @param 月別元本 df
#' @name b06
b06_1_控除所得税額 <- function( b06_1 ,月別元本 ) {
  # 項番6:(10) (9)のうち元本所有期間
  for ( i in 1:nrow( b06_1 ) ) {
    ve <- unlist( 月別元本[ i ,"元本" ] )
    j <- ( ve > 月別元本[ i ,"B" ] )
    ve[j]  <-   月別元本[ i ,"B" ]

    b06_1[ i ,"元本所有期間=" ] <-
      ( sum( ve ) / 月別元本[ i ,"B" ] )
  }
  # 見解の相違があるかも？

  # 項番7:(11) 所有期間割合
    # ○ 項番6／項番5の割合を記録してください。
  b06_1[ ,"所有期間割合=" ] <-
    ( b06_1$`元本所有期間=` / b06_1$`配当等の計算期間=`)
    # ○ 小数点以下3位未満は切り上げることに留意してください｡
    #  （例） 0.5012（50.12％）の場合 → 0.502
  b06_1[ ,"所有期間割合=" ] <-
    ceiling( b06_1[ ,"所有期間割合=" ] * 1000 ) / 1000
  # 項番8:(12)
  b06_1[ ,"控除を受ける所得税額=" ] <-
    floor( b06_1$`所得税額=` * b06_1$`所有期間割合=` )

  return( b06_1 )
}

# ? #共通モジュール
# ?#' @export
#' @param 配当基準日 配当基準日
#' @param n len
#' @name b06
b06_計算期間.init <- function( 配当基準日 ,n ) {
	# print( 配当基準日)
		# 列数
	計算期間 <- data.frame( matrix(  NA ,0 ,2 ) )
		# 列名
	colnames( 計算期間 ) <- c( "基準日" ,"元本"	)
		# 行数（前月末日ごと)
	計算期間[ 1:n ,"基準日" ] <-
	 as.character( seq( as.Date( 配当基準日 ) + 1  ,by = "-1 month" ,len = n )  - 1 )
	 # as.character( seq( as.Date( gsub( "-..$" ,"-01" ,配当基準日 ) ) ,by = "-1 month" ,len = n )  - 1 )
return( 計算期間 )
 }

# ?#共通モジュール
# ?#' @export
#' @param base 配当基準日
#' @param idou df
#' @name b06
計算期間_元本 <- function( base ,idou ) {
	for( j in 1 : nrow( base ) ) {
		ve[ j ] <- 0
		w2 <- subset( idou ,idou[ "異動事由=" ] < as.character( as.Date( base[ j ,"基準日" ] ) + 1 ) )
		if( nrow( w2 ) != 0 )
			ve[ j ] <- as.integer( w2[ nrow( w2 ) ,"期末現在高.数量=" ] )
	}
	return( unlist( ve ) )
}
#	b06_2_項番17 <- function( 期末 , 期首 ) {
#	# 項番７
#		#		  ,"((15)-(16))/2又は12="	#○ (項番5-項番6)/2又は12を記録してください。
#		( 期末 - 期首 ) / 2
#	return()
#	}

# ?#' @export
#' @param df df
#' @name b06
b06_2_所有元本割合 <- function( df ) {
# 項番８
	w <- ( df[ ,"計算期首=" ] + df[ ,"((15)-(16))/2又は12=" ] ) / df[ ,"計算期末=" ]
	w <- ceiling( w * 1000 ) / 1000
		# 以上、通達通り
return( w )
}

#' @export
#' @param n num of rows
#' @param b06_1 df
#' @param b06_2 df
#' @param stack df
#' @name b06
照合_b06 <- function( n ,b06_1 ,b06_2 ,stack) {
	# n <- EQ
	for( i in n ) {
		dif <- b06_1[ i,"控除を受ける所得税額=" ] - b06_2[ i,"控除を受ける所得税額=" ]
			print( c(
				 paste( i ,b06_1[ i ,"銘柄=" ] )
				,paste(
				  b06_2[ i ,"計算期末=" ]
				 ,b06_2[ i ,"計算期首=" ]
					)
				,paste(
				 b06_1[ i ,"所有期間割合=" ] ,"vs"
				,b06_2[ i ,"所有元本割合=" ]
				,"=" ,dif )
				)
			 )
			print( unlist( stack[ i ,"元本" ] ) )
	}
return( c( "差額(1-2)=" ,sum( b06_1[ n,"控除を受ける所得税額=" ] - b06_2[ n,"控除を受ける所得税額=" ] ) ) )
}

# ゴミかも？　#' @export
#' @param code 銘柄
#' @param base 配当基準日
#' @name b06
b06.銘柄 <- function( code ,base ) {
  #return(
  paste(
    gsub( "[()]" ,"" ,code )
    ,base
  )
  # )
}
# ゴミかも？　#' @export
#' @param b06 df
#' @param o 順序
#' @name b06
# 並べ替え
b06.o <- function( b06 ,o ) {
  #	print( o )
  switch( o
          ,"1" = {
            r <- order( b06[ ,"銘柄="] )
            return( b06[ r , ] )
          }
  )
  print( o )
  return( b06 )
}

