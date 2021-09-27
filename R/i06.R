# source( file.path( i.path ,"【⑥有価証券】.R" ) )

# 2021-09-09(木) move o_⑥有価証券.R  to R and rename it to i06.R
# 2020-09-30(水) copy o_⑥有価証券.R

#i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"


#'
#' 【⑥有価証券の内訳書】
#' HOI060_4.0.csv # 2021(令和3年4月1日以後終了事業年度又は連結事業年度分)
#' 同上           # 2020(令和2年4月1日以後終了事業年度又は連結事業年度分)
#' 同上           # 2019(平成31年4月1日以後終了事業年度又は連結事業年度分)
#' HOI060_3.0.csv # 2018(平成30年4月1日以後終了事業年度又は連結事業年度分)
#'（命名規約)バージョン番号(_4.0)までは必須
#'
#' 勘定科目内訳明細書の標準フォーム等
#' (平成31年4月1日以後終了事業年度分)
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho1/2/HOI060.pdf
#'
#' (平成30年4月1日以後終了事業年度分)
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho2_meisai_30.htm
#'
#' ○ レコードの内容及び留意事項(2-6)
#'  【勘定科目内訳明細書（⑥有価証券の内訳書）】
#' （平成31年４月１日以後終了事業年度分）
#' https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho1/2/HOI060.pdf

#' @export
#' @param s Settlement date
#' @name i06
i06.sheet <- function( s="2021-04-01" ) {
  # if ( s >= "2019-04-01" )  return("HOI060_4.0_⑥有価証券" )
  # else                      return("HOI060_3.0_⑥有価証券" )

  sheet <- read.csv( system.file( "i06_sheet.csv", package = "office" ) )

  for( i in 1:nrow( sheet )) if ( sheet[ i ,1 ] <= s ) break
  return( sheet[ i ,2 ] )
  }

#' @export
# #' @param ... nothing
# #' @return i06 df
#' @name i06
#i06.init <- function() {
i06_init <- function() {
    # 列数
	i06 <- data.frame( matrix(  NA ,0 ,18 ) )
# 列名
#	colnames( i06 ) <- c( "フォーマット区分=" ,"行区分="
#	  ,"区分="
#	  ,"種類="
#	  ,"銘柄=" ,"期末現在高.数量=" ,"帳簿価額=" ,"時価及び帳簿価額="
#	  ,"期中増(減).異動年月日.元号=" ,"年=" ,"月=" ,"日="
#	  ,"異動事由=" ,"数量=" ,"金額="
#	  ,"証券会社=" ,"住所=" ,"摘要=" )
	col <- read.csv(
	  system.file( "i06_col.csv", package = "office" ) )
	colnames( i06 ) <- t( col )

  return( i06 )
  }

#' @export
#' @param sbi dataframe
#' @name i06
#i06_sbi_約定 <- function( sbi ) {
i06_sbi_idou <- function( sbi ) {
  message( paste0( " p1 : " ,nrow( sbi ) ," rows" ) )
  # sbi : SBI証券の約定明細
  i06 <- i06_init()
  # 行数の初期化
  i06[ 1:nrow( sbi ) ,1 ] <- 6 #列1 () フォーマット区分=
  # 列の初期値
  i06[ ,2 ]	<-0                #列2 () 行区分= 0:明細部分
#  i06[ ,3 ]	<-"その他"	       #列3 () 区分=
  i06[ ,4 ]	<-                 #列4 () 種類=
            sbi[ ,3 ]           #col 3 銘柄コード numeric
  i06[ ,5 ] <-                 #列5 () 銘柄=
    substr( sbi[ ,2 ] ,1 ,10 )   #col 2 銘柄 character 10文字まで
  # 項番13、9〜12
  i06[ ,13 ] <-                #列13() 異動事由=
    gsub( "/" ,"-" ,sbi[ ,13 ] ) #col 13 受渡日 character
  # gsub( "/" ,"-" ,sbi[ ,1 ] ) #col 1 約定日 character
  i06[ ,13 ] <- gsub( "-0" ,"-" ,i06[ ,13 ] ) # 前ゼロ削除
  i06[ ,9:12 ] <-              #列9:12() 異動年月日=
     gymd_y4md( i06[ ,13 ] )
    i06[ ,15 ] <- 0              #列15() 金額=
  i06[ ,16 ] 	<-	"SBI"        #列16() 証券会社=　"SBI証券"

  #col 5 取引 x col 14 受渡金額.決済損益 character
  sbi_5 <- read.csv( system.file( "sbi_5.csv", package = "office" ) )

#  e <- c( NULL )
  e <- which( is.na( i06[ ,4 ] )    #列4 () 種類=
             )
  if ( length( e ) != 0 ) {
    warning( c("skip NA col 4 ," ,list( e ) ) )  #列4 () 種類=
  }

  for (i in 1:nrow( sbi_5 )) {
    j <- grep( sbi_5[i,1]  #inst 列1
              ,sbi[ ,5 ] )      #col 5 取引

      # 対象なし
    if ( length( j ) == 0 ) next

      # 異常データ
    if ( is.na( sbi_5[ i ,2 ] ) ) {
      warning( c("skip " ,sbi_5[ i ,1 ]
                 ," ," ,list( j )  ) )  #col 5 取引
      e <- c( e , j )
      next
    }

      # 正常データ
    if ( sbi_5[i,2] < 0 ) { # 売りのみ設定
      i06[ j ,13 ] <- paste(        #列13() 異動事由=
      i06[ j ,13 ]   ,sbi_5[i,3] )  # 「売却」文言を追加

      i06[ j ,14 ]	<-              #列14() 数量=
      sbi[ j ,9 ] * -1   #col 9 約定数量 numeric に  マイナスを付ける

      i06[ j ,17 ]	<-              #列17() "住所=" # 計算用紙に
      sbi[ j ,14 ]       #col 14 受渡金額.決済損益 character
    }
    if ( sbi_5[i,2] > 0 ) { # 買いのみ設定
      i06[ ,14 ] <-                #列14() 数量=
        sbi[ ,9 ]       #col 9 約定数量 numeric
      i06[ j ,15 ]   <-             #列15() 金額=
      sbi[ j ,14 ]      #col 14 受渡金額.決済損益 character
    }
  }

  j <- which( is.na( i06[ ,14 ] ) ) #列14() 数量=
  if ( length( j ) != 0 ) {
    warning( c( "skip NA col 14 ," ,list( j ) ) )
    e <- c( e ,j )
  }

  r <- i06           # 異常データを削除する前
  if ( length( e ) != 0 ) {
    r <- i06[ -e , ] # 異常データを削除
  }

  message( paste0( " Re : " ,nrow( r ) ," rows" ) )

  return( r )
}

#' @export
#' @param kabu dataframe
#' @name i06
i06_kabu_idou <- function( kabu ) {
  message( paste0( " p1 : " ,nrow( kabu ) ," rows" ) )
  # kabu : カブドットコム証券の約定明細
  i06 <- i06_init()
  # 行数の初期化
  i06[ 1:nrow( kabu ) ,1 ] <- 6 #列1 () フォーマット区分=
  # 列の初期値
  i06[ ,2 ]	<-0                #列2 () 行区分= 0:明細部分
  #  i06[ ,3 ]	<-"その他"	       #列3 () 区分=
  i06[ ,4 ]	<-                 #列4 () 種類=
    kabu[ ,5 ]           #col 5 銘柄コード numeric
  i06[ ,5 ] <-                 #列5 () 銘柄=
    substr( kabu[ ,4 ] ,1 ,10 )   #col 4 銘柄名 character 10文字まで
  # 項番13、9〜12
  i06[ ,13 ] <-                #列13() 異動事由=
    gsub( "/" ,"-" ,kabu[ ,2 ] ) #col 2 受渡日 character
  i06[ ,13 ] <- gsub( "-0" ,"-" ,i06[ ,13 ] ) # 前ゼロ削除
  i06[ ,9:12 ] <-              #列9:12() 異動年月日=
    gymd_y4md( i06[ ,13 ] )
  i06[ ,15 ] <- 0              #列15() 金額=
  i06[ ,16 ] 	<-	"kabu.com"   #列16() 証券会社=　"カブドットコム証券"

  #col 6 売買区分 x #col 9 受渡金額 numeric
  kabu_6 <- read.csv( system.file( "kabu_6.csv", package = "office" ) )

  e <- c( NULL )
  for (i in 1:nrow( kabu_6 )) {
    j <- grep( kabu_6[i,1]  #inst 列1
               ,kabu[ ,6 ] )      #col 6 売買区分 character

    # 対象なし
    if ( length( j ) == 0 ) next

    # 異常データ
    if ( is.na( kabu_6[ i ,2 ] ) ) {
      warning( c("skip " ,kabu_6[ i ,1 ] ," ," ,j  ) )  #col 6 売買区分
      e <- c( e , j )
      next
    }

    # 正常データ
    if ( kabu_6[i,2] < 0 ) { # 売りのみ設定
      i06[ j ,13 ] <- paste(        #列13() 異動事由=
        i06[ j ,13 ]   ,kabu_6[i,3] )  # 「売却」文言を追加

      i06[ j ,14 ]	<-              #列14() 数量=
        kabu[ j ,7 ] * -1   #col 7 数量 numeric に  マイナスを付ける

      i06[ j ,17 ]	<-              #列17() "住所=" # 計算用紙に
        kabu[ j ,9 ]        #col 9 受渡金額 numeric
    }
    if ( kabu_6[i,2] > 0 ) { # 買いのみ設定
      i06[ ,14 ] <-                #列14() 数量=
        kabu[ ,7 ]           #col 7 数量 numeric
      i06[ j ,15 ]   <-             #列15() 金額=
        kabu[ j ,9 ]        #col 9 受渡金額 numeric
    }
  }

  r <- i06           # 異常データを削除する前
  if ( length( e ) == 0 ) { # 異常データを削除
    j <- r[ is.na( i06[    ,14 ] ) ]     #列14() 数量=
  }else {
    j <- r[ is.na( i06[ -e ,14 ] ) ]     #列14() 数量=
    r <- i06[ -e , ]
  }
  if ( length( j ) != 0 ) {          # 未処理があるのでは？
    warning( c( "skip others ," ,j  ) )  #col 5 取引
    e <- c( e ,j )
    r <- i06[ -e , ]
  }

  message( paste0( " Re : " ,nrow( r ) ," rows" ) )

  return( r )
}

#' @export
#' @param sbi dataframe
#' @name i06
# i06_sbi_割当 <- function( sbi ) {
i06_sbi_As <- function( sbi ) {
  message( paste0( " p1 : "
                   ,nrow( sbi ) ," rows" ," x " ,ncol( sbi ) ," cols" ) )
  for( i in 1:ncol( sbi )){
    message( paste( "     #col" ,i
                   ,colnames( sbi[i] ) ,mode( sbi[,i]) ) )
  }

  # sbi : SBI証券の割当株式
  i06 <- i06_init()
  # 行数の初期化
  i06[ 1:nrow( sbi ) ,1 ] <- 6 #列1 () フォーマット区分=
  # 列の初期値
  i06[ ,2 ]	<-0		        	   #列2 () 行区分= 0:明細部分
#  i06[ ,3 ]	<-"その他"         #列3 () 区分=
  i06[ ,4 ]	<-                 #列4 () 種類=
    sbi[ ,2 ]	   #col 2 銘柄コード character
  #     as.integer( sbi[ ,2 ] )	   #col 2 銘柄コード character
  i06[ ,5 ] <- substr(         #列5 () 銘柄=
     sbi[ ,1 ] ,1 ,10 )          #col 1 銘柄 character 10文字まで
  # 項番9～12、13
  i06[ ,9:12 ] <- gymd_y4md(   #列9:12() 異動年月日=
     sbi[ ,4 ] )                 #col 4 権利割当日 numeric
  i06[ ,13 ] <-paste(          #列13() 異動事由=
     sbi[ ,4 ]                   #col 4 権利割当日 numeric
   , substr(               # 追加文言　「割当」
      colnames( sbi[ 8 ] )       #col 8 割当数量 character
      ,1 ,2 )
    )
  i06[ ,13 ] <- gsub( "-0" ,"-" ,i06[ ,13 ] ) # 前ゼロ削除

  i06[ ,14 ] <-                #列14() 数量=
       sbi[ ,8 ]                 #col 8 割当数量 character
  i06[ ,15 ] 	<-	0            #列15() 金額=
  i06[ ,16 ] 	<-	"SBI"        #列16() 証券会社=　"SBI証券"

  message( paste0( " Re : "
                   ,nrow( i06 ) ," rows" ) )
  return( i06 )
}

#' @export
#' @param df1 dataframe
#' @param df2 dataframe
#' @name i06
i06_merge <- function( df1 ,df2 ) {
  message( paste0( " p1 : " ,nrow( df1 ) ," rows" ) )
  message( paste0( " p2 : " ,nrow( df2 ) ," rows" ) )
  # 銘柄別に並び替え
    # 異動年月日で並び替え
  i06 <- rbind( df1 ,df2 )
  message( paste0( "     # sort cols ,"
                  ," 4 2 9 10 11 12" ) )

  i06 <- i06[ order( i06[ ,4 ]  #列4 () 種類=
                    ,i06[ ,2 ]  #列2 () 行区分= 0:明細部分
                    ,i06[ ,9 ]  #列9:12() 異動年月日=
                    ,i06[ ,10 ]
                    ,i06[ ,11 ]
                    ,i06[ ,12 ]
                    ) , ]
  message( paste0( " Re : "
                   ,nrow( i06 ) ," rows" ) )
  return( i06 )
}

#' @export
#' @param i06_pt the previous term　前期
#' @param i06_ct the current  term  当期・明細行
#' @name i06
# i06_帳簿価額 <- function( i06_pt ,i06_ct) {
i06_Book_value <- function( i06_pt ,i06_ct ) {
  message( paste0( " p1 : " ,nrow( i06_pt ) ," rows" ) )
  # i06_前期 <- i06_残高繰越( i06_1 ) # 合計行のみ抽出
  # i06 <- i06_merge( i06_前期 ,i06_0 )
#  i06_1 <- i06_pt[ ( i06_pt$"行区分=" == 1 　        # 合計行のみ抽出
#                     & i06_pt$"時価及び帳簿価額=" != 0 # 残高ありを抽出
#                      ) , ] #
  message( paste0( "     # copy bottom lines ,col 2 == 1 " ) )
  i06_1 <- i06_pt[ (  i06_pt[2] == 1           # 合計行のみ抽出
                    & i06_pt[6] != 0           # 残高ありを抽出
                    ) , ] #

  message( paste0( "     # " ,nrow( i06_1 ) ," rows( col 2 == 1 )" ) )

  message( paste0( " p2 : " ,nrow( i06_ct ) ," rows" ) )
  if ( isFALSE( i06_check_idou( i06_ct ) ) ) {
    stop( "Execution halted" )
  }

  i06   <- rbind( i06_1 ,i06_ct )
  message( paste0( " p1 + p2 : " ,nrow( i06 ) ," rows" ) )
  message( paste0( "     # sort cols ,"
                   ," 4 2(D) 9 10 11 12" ) )
  i06   <- i06[ order( i06[ ,4 ]   #列4 () 種類=
                      ,-i06[ ,2 ]  #列2 () 行区分= ※降順
                      ,i06[ ,9 ]   #列9:12() 異動年月日= 元号
                      ,i06[ ,10]            # 年
                      ,i06[ ,11]            # 月
                      ,i06[ ,12]            # 日
  )
              , ]
  message( paste0( "     # " ,nrow( i06 ) ," rows( p1 + p2 )" ) )

  message( paste0( "     # calc cols , 6 8 " ) )
  message( paste0( "     # (sell only) , 15 18 " ) )
  i06[ ,14 ] <- as.integer( i06[ ,14 ] ) #列14() 数量=
  i06[ ,15 ] <- as.integer( i06[ ,15 ] ) #列15() 金額=

  Key <- "0000"
  # Key <- as.integer( "0000" )
  for( i in 1:nrow( i06 ) )
  {
    # 前期末であれば
    # if ( !is.na(    i06[ i ,"期末現在高.数量=" ] ) )
    if ( i06[ i ,2 ] == 1 ) {       #列2 () 行区分= 1 合計行 ※降順
      Key <- i06[ i ,4 ]            #列4 () 種類=
      Quantity   <-                 #累計数量
        i06[ i ,6 ]     #列6 () 期末現在高.数量=
      Book_value <-                 #帳簿価額
        i06[ i ,8 ]     #列8 () 時価及び帳簿価額=
      next
    }

    if( !(i06[ i ,4 ] == Key ) ) {      #列4 () 種類=
      Key <- i06[ i ,4 ]            #列4 () 種類=
      Quantity		<- 0              #累計数量
      Book_value	<- 0              #帳簿価額
    }

    if( i06[ i ,14 ] < 0 )      #列14() 数量= 売り
    {
      if ( Quantity ==          #累計数量
        i06[ i ,14 ]  * -1 )    #列14() 数量= スクエア
      {
          i06[ i ,15 ]	<-               #列15() 金額=  全額売り
            - Book_value            #帳簿価額
          # 売買損益
          i06[ i ,18 ]	<- as.integer(   #列18() 摘要= 売却損益
             i06[ i ,17 ] )	+            #列17() 住所= 受渡金額
             i06[ i ,15 ]                #列15() 金額= 簿価(マイナス)
      }
      else if ( Quantity >=          #累計数量
             i06[ i ,14 ]  * -1 )    #列14() 数量= 一部売却
      {
          i06[ i ,15 ]	<-               #列15() 金額= 一部売却
            round( Book_value	*     #帳簿価額
          i06[ i ,14 ]	/	               #列14() 数量=(マイナス)
            Quantity )              #累計数量
          # 売買損益
          i06[ i ,18 ]	<- as.integer(   #列18() 摘要= 売却損益
           i06[ i ,17 ] )	+              #列17() 住所= 受渡金額
           i06[ i ,15 ]                  #列15() 金額= 簿価(マイナスあり)
      }
      else {
        warning( paste0( "col 6 <  zero , row of " ,i ) )
        i06[ i ,15 ]	<-               #列15() 金額=  (マイナスあり)
        i06[ i ,17 ] *-1  	           #列17() 住所= 受渡金額
      }
    }
    else  # 買い
      if ( Quantity < 0 ) {      #累計数量
        if ( Quantity ==          #累計数量
             i06[ i ,14 ]  * -1 )    #列14() 数量= スクエア
        {
          warning( paste0( "col 6 == zero , row of " ,i ) )
          i06[ i ,17 ] 	<-               #列17() 住所= 受渡金額
          i06[ i ,15 ] * -1                #列15() 金額=
          i06[ i ,15 ]	<-               #列15() 金額=
            - Book_value                   #帳簿価額
          # 売買損益
          i06[ i ,18 ]	<-               #列18() 摘要= 売却損益
            i06[ i ,17 ] 	+                #列17() 住所= 受渡金額
            i06[ i ,15 ]                   #列15() 金額= 簿価
        }
        else {
          warning( paste0( "col 6 <  zero , row of " ,i ) )
        }
    }
    #D  print( mode( i ) )
    #D  print( mode( Quantity ) )
    #D  print( mode( i06[ i ,"数量=" ] ) )
    #D  print( paste( i ,Quantity ,i06[ i ,"数量=" ] ) )
    Quantity		<-                 #累計数量
    Quantity		  + i06[ i ,14 ]         #列14() 数量= (マイナスあり)
    i06[ i ,6 ]	<-	Quantity       #列6 () 期末現在高.数量=
    Book_value	<-                 #帳簿価額
    Book_value	+ i06[ i ,15 ]           #列15() 金額= 簿価(マイナスあり)
    i06[ i ,8 ]	<-	Book_value     #列8 () 時価及び帳簿価額=
  }
#  i <- i06[ ,"行区分=" ] == 0 # 明細行のみ、合計行（前期）
#  return( i06[ i , ] )
  message( paste0( " Re : "
                   ,nrow( i06 ) ," rows" ) )
  return( i06 )
}

#' @export
#' @param Xs 株式交換
#' @param i06_pt the previous term　前期
#' @param i06_ct the current  term  当期・明細行
#' @name i06
i06_Xs <- function( Xs ,i06_pt ,i06_ct ) {
  message( paste0( " p1 : "
                   ,nrow( Xs ) ," rows" ," x "
                   ,ncol( Xs ) ," cols" ) )
  for ( i in 1:nrow( Xs )) {
    #    for ( j in 1:ncol( Xs )) {
    message( paste( " #  : "
                    ,Xs[ i ,1 ]
                    ,Xs[ i ,2 ]
                    ,Xs[ i ,3 ]
                    ,Xs[ i ,4 ]
                    ,Xs[ i ,5 ]
                    ,Xs[ i ,6 ]
                    ,Xs[ i ,7 ] )
    )
    #    }
  }

  i <- list( Xs[ ,1 ] )

  message( paste0( " p2 : "
                   ,nrow( i06_pt ) ," rows" ," x "
                   ,ncol( i06_pt ) ," cols" ) )
  j <- which( i06_pt[ ,4 ] #列4 () 種類=
              %in% i )       # 旧株式のみ抽出
  message( paste0( "     # " ,length( j ) ," rows( col 4 == old )" ) )

  message( paste0( " p3 : "
                   ,nrow( i06_ct ) ," rows" ," x "
                   ,ncol( i06_ct ) ," cols" ) )
  k <- which( i06_ct[ ,4 ] #列4 () 種類=
              %in% i )       # 旧株式のみ抽出
  message( paste0( "     # " ,length( k ) ," rows( col 4 == old )" ) )

  message( paste0( "# i06_Book_value( i06_pt ,i06_ct) " ) )
  i06_B <- i06_Book_value( i06_pt[ j ,] ,i06_ct[ k ,]) # 簿価を仮計算
  message( paste0( "# end  i06_Book_value() " ) )


  #  for( i in 1:ncol( sbi )){
  #    message( paste( "     #col" ,i
  #                    ,colnames( sbi[i] ) ,mode( sbi[,i]) ) )
  #  }

  # sbi : SBI証券の株式交換・旧株式
  i06 <- i06_init()
  # 行数の初期化
  i06[ 1:nrow( Xs ) ,1 ] <- 6 #列1 () フォーマット区分=
  # 列の初期値
  i06[ ,2 ]	<-0		        	   #列2 () 行区分= 0:明細部分
  #  i06[ ,3 ]	<-"その他"         #列3 () 区分=
  i06[ ,4 ]	<-                 #列4 () 種類=
    Xs[ ,1 ]	                   #仮 col 1 銘柄コード character
  #  i06[ ,5 ] <- substr(         #列5 () 銘柄=
  #    Xs[ ,1 ] ,1 ,10 )          #col 1 銘柄 character 10文字まで
  # 項番9～12、13
  i06[ ,9:12 ] <- gymd_y4md(   #列9:12() 異動年月日=
    Xs[ ,3 ] )                  #仮 col 3 権利確定日
  i06[ ,13 ] <-paste(          #列13() 異動事由=
    Xs[ ,3 ] # 仮 col 3 権利確定日
    ,Xs[ ,4 ] # 仮 col 4 文言
  )
  #  i06[ ,13 ] <- gsub( "-0" ,"-" ,i06[ ,13 ] ) # 前ゼロ削除

  i06[ ,14 ] <-                #列14() 数量=
    as.integer( Xs[ ,2 ] ) * -1               #仮 col 2 数量=
  #  i06[ ,16 ] 	<-	"SBI"        #列16() 証券会社=　"SBI証券"

  # 簿価を求める
  #  i06[ ,15 ] 	<-	0            #列15() 金額=
  for ( i in 1:nrow( Xs ) ) {
    j <- which( i06_B[ ,4 ] == i06[ i ,4 ] ) #列4 () 種類=
    if ( length( j ) == 0 ) {
      warning( paste( " nothing " ,i06[ i ,4 ] ) )
    }
    i06[ i , 5 ] <-              #列5 () 銘柄=
      i06_B[ j[ length(j) ] ,5 ]
    i06[ i ,15 ] <-              #列15() 金額=
      i06_B[ j[ length(j) ] ,8 ] # 列8() 時価及び帳簿価額=
  }
  i06[ ,15 ] <- i06[ ,15 ] * -1 #列15() 金額=

  # 新株式
  i06_n <- i06
  #  i06_新株式[ 1 ,4 ]   <- 5019                      #列4 種類=
  i06_n[ ,4 ]	<-                 #列4 () 種類=
    Xs[ ,5 ]	                   #仮 col 5 銘柄コード character
  #  i06_新株式[ 1 ,5 ]   <- "出光興産"                #列5 銘柄=
  i06_n[ ,5 ]	<-                 #列5 銘柄=
    Xs[ ,7 ]	                   #仮 col 7 銘柄=
  #  i06_新株式[ 1 ,14 ]  <- 123
  i06_n[ ,14 ] <-                  #列14() 数量=
    as.integer( Xs[ ,6 ] )               #仮 col 6 数量=
  i06_n[ ,15 ] <- i06_n[ ,15 ] * -1 #列15() 金額=

  r <- rbind( i06 ,i06_n )
  message( paste0( " Re : "
                   ,nrow( r ) ," rows" ) )
  return( r )
}

#' @export
#' @param i06_0 dataframe 期中
#' @name i06
# i06_合計行 <- function( i06_0 ) {
i06_lines_sum <- function( i06_0 ) {
  message( paste0( " p1 : " ,nrow( i06_0 ) ," rows" ) )
  if ( nrow( i06_0 ) == 0 ) return( i06_0 )

  i <- which( is.na( i06_0[ ,4 ] ) ) #列4() 種類=
  if ( length( i ) != 0 ) {
    stop( paste( "NA col 4 , row of " ,list( i ) ) )
  }

  i <- which( is.na( i06_0[ ,8 ] ) ) # (8)時価及び帳簿価額=
  if ( length( i ) != 0 ) {
    stop( paste( "NA col 8 , row of " ,list( i ) ) )
  }

  Key <- "9999"
  l <- c( NULL )
  for( i in nrow( i06_0 ):1 ) {
    if( Key != i06_0[ i ,4 ] )     #列4 () 種類=
    {
      Key <- i06_0[ i ,4 ]         #列4 () 種類=
      i06_0[ i ,2 ] <- 1           #列2 () 行区分= 1 合計行
      l <- c( i , l )
    }
  }
  message( paste0( "     # copy bottom lines by col 4" ) )
  #ve <- i06_0[ ,2 ] ==  1          #列2 () 行区分= 1 合計行
  i06_1 <- i06_0[ l , ]
  message( paste0( " Re : " ,nrow( i06_1 ) ," rows" ) )
  #D print( ve )
  message( paste0( "     # sum( col 8 ) = " ) ,
    format( sum( i06_1[ ,8 ] )     # (8)時価及び帳簿価額=
          , big.mark=","
          , scientific=F )
  )
  return( i06_1 )
}

#' @export
#' @param i06_0 明細行
#' @param i06_1 合計行
#' @param n   2 損益行
#' @name i06
i06_ct <- function( i06_0 ,i06_1 = i06_init() ,n=1 ) {
  message( paste0( " p1 : " ,nrow( i06_0 ) ," rows" ) )
   #  明細行のみ抽出
  message( paste0( "     # copy lines ,col 2 == 0 " ) )

  i <- which( i06_0[ ,2 ] == 0 ) # 明細行のみ抽出
  message( paste0( "     # " ,length( i )
                   ," rows  ( col 2 == 0 )" ) )
  i06_ct <- i06_0[ i , ]

  if ( n == 0 ) {
    message( paste0( " Re : "
                      ,nrow( i06_ct ) ," rows" ) )
    return( i06_ct )
  }

  i <- which( !is.na( i06_ct[ ,18 ] ) )   #列18() 金額=
  # i <- which( is.numeric( i06_ct[ ,18 ] ) )   #列18() 金額=
  message( paste0( " # " ,length( i )
                   ," rows of P/L ( col 18 != NA )" ) )
  message( paste0( " # sum( col 15 ) = " ) ,
           format( sum( i06_ct[ i ,15 ] )  #列15() 金額=
                   , big.mark=","
                   , scientific=F )
           ," Book"
  )
  message( paste0( " # sum( col 17 ) = " ) ,
           format( sum(
            as.integer( i06_ct[ i ,17 ] ) )
#                        ,na.rm = TRUE)  #列17() 住所= 受渡金額
                   , big.mark=","
                   , scientific=F )
           ," Cash"
  )
  message( paste0( " # sum( col 18 ) = " ) ,
           format( sum( i06_ct[ i ,18 ] )  #列18() 摘要= 売却損益
                   , big.mark=","
                   , scientific=F )
           ," P/L"
  )
  if ( n == 2 ) {
    message( paste0( " Re : "
                     ,length( i ) ," rows" ) )
    return( i06_ct[ i , ] )
  }

  message( paste0( " p2 : " ,nrow( i06_1 ) ," rows" ) )

  message( paste0( "# call i06_merge( i06_ct ,i06_1 ) " ) )
  i06_ct <- i06_merge( i06_ct ,i06_1 )
  message( paste0( "# end  i06_merge " ) )
  return( i06_ct )
}

#' @export
#' @param i06 dataframe
#' @name i06
#  i06_check_異動明細 <- function( i06 ) {
i06_check_idou <- function( i06 ) {
  i <- which( is.na( i06[ ,4 ] ) ) #列4() 種類=
  if ( length( i ) != 0 ) {
    warning( paste( "NA col 4 , row of " ,list( i ) ) )
    return( FALSE )
    }

  i <- which( is.na( i06[ , 9 ] ) #列9:12() 異動年月日=
            | is.na( i06[ ,10 ] )
            | is.na( i06[ ,11 ] )
            | is.na( i06[ ,12 ] ) )
  if ( length( i ) != 0 ) {
     warning( paste("NA cols 9:12 , row of  " ,list( i ) ) )
     return( FALSE )
     }
#  if ( anyNA( i06[ ,15 ] ) ) #列15() 金額= 簿価(マイナスあり)
#  { warning( c("Error in 2nd param : NA col 15"
#             ,"Execution halted"            )  )
#    return( FALSE )
#  }

  i <- which( is.na( i06[ ,14 ] ) ) #列14() 数量=
  if ( length( i ) != 0 ) {
    warning( paste( "NA col 14 , row of  " ,list( i ) ) )
    return( FALSE )
    }
  return( TRUE )
}

# ゴミかも #' @export
#' @param i06 dataframe
#' @name i06
# i06_異動明細 <- function( i06 ) {
i06_idou <- function( i06 ) {
  message( paste0( " p1 : "
                   ,nrow( i06 ) ," rows" ," x " ,ncol( i06 ) ," cols" ) )
  i06_0 <- i06[ i06[ ,2 ] == 0 , ]
  message( paste0( " () : "
                   ,nrow( i06_0 ) ," rows ," ," col 2 == 0" ) )

  idou <- data.frame( ( matrix( NA ,nrow( i06_0 ) ,4 ) ) )
  message( paste0( " Re : "
                   ,nrow( idou ) ," rows" ," x " ,ncol( idou ) ," cols" ) )
  # colnames( idou )  <- c( "種類=" ,"異動年月日" ,"数量=" ,"金額=" )
  #  idou <-  cbind(
#     "種類="  = i06$`種類=`
#                                # `期中増(減).異動年月日.元号=`
#    ,"異動年月日" = y4md_gymd( i06[ ,9:12 ] )
#    ,"数量="      = i06$`数量=`
#    ,"金額="      = i06$`金額=`
#  )
  idou[ ,1 ] <-             i06_0[ ,4 ] #列4 () 種類=
  idou[ ,2 ] <- y4md_gymd( i06_0[ ,9:12 ] ) #列9:12() 異動年月日=
  idou[ ,3 ] <- as.integer( i06_0[ ,14 ] )  #列14() 数量=
  idou[ ,4 ] <- as.integer( i06_0[ ,15 ] )  #列15() 金額=

  col <- read.csv(
    system.file( "i06_idou_col.csv", package = "office" ) )
  colnames( idou ) <- t( col )
  for( i in 1:ncol( idou )){
    message( paste("     #col" ,i
                   ,colnames( idou[i] ) ,mode( idou[,i]) ) )
  }
  message( paste0( " # sort cols ,"
                   ," 1 2 as.Date()" ) )
  o <- order( idou[ ,1 ] ,as.Date( idou[ ,2 ] ) )
  return( idou[ o ,] )
}

#' @export
#' @param i06_pt 前期
#' @param i06_ct 当期
#' @name i06
# i06_idou <- function( i06 ) {
i06_p <- function( i06_pt ,i06_ct ) {
  message( paste0( " p1 : "
                   ,nrow( i06_pt ) ," rows" ," x "
                   ,ncol( i06_pt ) ," cols" ) )
  i06_0_pt <- i06_pt[ i06_pt[ ,2 ] == 0 , ]
  message( paste0( " () : "
                   ,nrow( i06_0_pt ) ," rows ," ," col 2 == 0" ) )
#
  message( paste0( " p2 : "
                   ,nrow( i06_ct ) ," rows" ," x "
                   ,ncol( i06_ct ) ," cols" ) )
  i06_0_ct <- i06_ct[ i06_ct[ ,2 ] == 0 , ]
  message( paste0( " () : "
                   ,nrow( i06_0_ct ) ," rows ," ," col 2 == 0" ) )
  #
  i06_0 <- rbind( i06_0_pt ,
                  i06_0_ct )

  p <- data.frame( ( matrix( NA ,nrow( i06_0 ) ,3 ) ) )
  message( paste0( " Re : "
                   ,nrow( p ) ," rows" ," x " ,ncol( p ) ," cols" ) )
  p[ ,1 ] <-             i06_0[ ,4 ]     #列4 () 種類=
  p[ ,2 ] <- y4md_gymd( i06_0[ ,9:12 ] ) #列9:12() 異動年月日=
  p[ ,3 ] <- as.integer( i06_0[ ,14 ] )  #列14() 数量=
  col <- read.csv(
    system.file( "i06_idou_col.csv", package = "office" ) )
  colnames( p ) <- t( col[ 1:3 ,] )

  for( i in 1:ncol( p )){
    message( paste("     #col" ,i
                   ,colnames( p[i] ) ,mode( p[,i]) ) )
  }

  e <- which(           # 株式分割、対象外　※割当株式
    (   i06_0[,14] >  0          #列14 数量=　買いなのに
       & i06_0[,15] == 0  )      #列15 金額=  zero
    |                   # 株式交換 対象外　※旧株式
    (   i06_0[,14] <  0          #列14 数量=　売りなのに
       & is.na( i06_0[,17] )     #列17 住所=  zero  ※受渡金額
       & is.na( i06_0[,18] ) )   #列18 摘要=  zero  ※売却損益
             )
  r <- p               # 異常データを削除する前
  if ( length( e ) != 0 ) { # 異常データを削除
    message( paste0( "w skip  "
                     ,length( e ) ," rows"  ) )
    for ( i in 1:length( e ) ) {
      warning( paste(
        "skip"
        ,i06_0[ e[i] ,4  ] #列 4 種類=
        ,i06_0[ e[i] ,13 ] #列13 異動事由=
        ,","
        ,i06_0[ e[i] ,5  ] #列 5 銘柄=
      ) )
    }
    r <- p[ -e , ]
  }
  message( paste0( " # sort cols ,"
                   ," 1 2 as.Date()" ) )
  o <- order( r[ ,1 ] ,as.Date( r[ ,2 ] ) )
  message( paste0( " Re : "
                   ,nrow( r ) ," rows" ," x " ,ncol( r ) ," cols" ) )
  return( r[ o ,] )
}

