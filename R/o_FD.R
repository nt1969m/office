# source( file.path( i.path, "o_ファイル定義.R" ) )

# 2021-09-08(水) rename o_ファイル定義.R to o_FD.R
# 2021-09-08(水) move to
# 2020-10-08(木) copy o_csv.R

#i.path ="/Users/nt1969m/Library/Mobile Documents/iCloud~com~kappsmart~rcompiler/Documents"
#		print( c( "i.path" ,i.path ) )

	 # 会計年度(Fiscal Year)、会計期間(Accounting period)
## FY <- "/Users/nt1969m/OneDrive/office/確定申告/23期-2020.08.31"
#FY <- "/Users/nt1969m/OneDrive/office/確定申告/24期-2021-08-31"

# setwd( paste0( FY ,"/eTax" ) )
# 	print( c( "getwd()" ,getwd() ) )  # 目検（目視にて検証する)

  # 税務ソフト(eTax)に反映
#'
#' Write_eTax
#'

#' @export
#' @importFrom  utils write.table
#' @description
#' df:入力データフレーム型（行列型でも動くと思う）
#'  ↓
#'  s:出力シート名（MSエクセルのあれが必須です）
#'
#'  ソフトウェア開発業者の方へ
#'  https://www.e-tax.nta.go.jp/shiyo/index.htm
#'
#' @param df dataframe
#' @param s sheet name
#' @name FD
Write_eTax <- function( df ,s ) {
	message( paste0( s, ".csv" ," : nrow=" ,nrow( df ) ) )
	write.table(
		 df
		,paste0( s ,".csv" )
		,quote=F
		,sep=","
		,row.names=F
		,col.names=F
		,eol="\r\n"
		,na=""        # 2021-09-12 追加
		,fileEncoding="CP932"
	)
}

	# 勘定科目⑥有価証券 2019
#'
#' @export
#' @importFrom  utils read.csv
  # f:ファイル名(除く、拡張子)
  # e:拡張子(extension)
  # n:skipの引数　不要か？
#' @param d directory
#' @param f file name without ext
#' @param e ext of f default(.csv)
#' @param n skip line of f default( 0 )
#' @name FD
Read_eTax <- function( d ,f ,e=".csv" ,n=0 ) {
  message( paste0( " p1 : " ,d ) )
  message( paste0( " p2 : " ,f ) )
  message( paste0( " p3 : e = " ,e ) )
  fn <- paste0(
      file.path( d ,f ) ,e )
#  if ( n != 0 ) {
    message( paste0( " p4 : skip = " ,n ) )
#  }

  l <-readLines( fn
              ,n= n + 1 )
  writeLines( iconv(l ,from="CP932") )


  df <-
  read.csv( fn
		 ,skip=n
		 ,fileEncoding="CP932"
		 ,stringsAsFactors=F
	)
  # return("HOI060_3.0_⑥有価証券" )
  switch( substr( f ,1 ,6  )
    , "HOI060" = {   # "HOI060_4.0_⑥有価証券" )
        colnames( df ) <- colnames( i06_init() )
    }
    , "HOB800" = {   # HOB800_16.0_受取配当等の益金不算入"
        n <- substr( df[ 1 , 1] ,6 ,6 )
        colnames( df ) <- colnames( b0801_init( n ) )
    }
    , "HOB016" = {   # "HOB016_6.0_所得税額"
        n <- substr( df[ 1 , 1] ,6 ,6 )
        colnames( df ) <- colnames( b0601_init( n ) )
    }
  )
  message( paste0( " Re : "
                   ,nrow( df ) ," rows" ," x "
                   ,ncol( df ) ," cols" ) )
	return( df )
}

#'
#' @export
#' @importFrom  utils read.csv
  # f:ファイル名(除く、拡張子)
  # e:拡張子(extension)
  # n:skipの引数
#' @param d directory
#' @param f file name without ext
#' @param e ext of f default(.csv)
#' @param n skip line of f default( 0 )
#' @name FD
  # Read_Bank <- function( f ,e=".csv" ,n=0 ) {
Read_MS <- function( d ,f ,e=".csv" ,n=0 ) {
  message( paste0( " p1 : " ,d ) )
  message( paste0( " p2 : " ,f ) )
  message( paste0( " p3 : e = " ,e ) )
  fn <- paste0(
    file.path( d ,f ) ,e )

  message( paste0( " p4 : skip " ,n ," rows" ) )

  l <-readLines( fn ,n= n + 2 )
  writeLines( iconv(l ,from="CP932") )

  df <-
  read.csv( fn
#	 ,header = TRUE
	 ,skip=n
	 ,fileEncoding="CP932" # MicroSoft
	 ,stringsAsFactors=F
	 )
  message( paste0( " Re : " ,nrow( df ) ," rows" ," x " ,ncol( df ) ," cols" ) )
  for( i in 1:ncol(df)){
    message( paste("     #col" ,i ,colnames( df[i] ) ,mode(df[,i]) ) )
  }
  return( df )
}

# 会計ソフトに反映
# ゴミかも #' @export
#' @importFrom  utils write.csv
#' @param df dataframe
#' @param f file name without ext
#' @name FD
#Write_会計 <- function( df ,f ) {
Write_FA <- function( df ,f ) {
  print( gsub( ".PDF" ,".txt" ,f ) )
	write.csv(
		df
		,gsub( ".PDF" ,".txt" ,f )
		,row.names=F
#		,fileEncoding="CP932"
	)
}

#' @export
#' @param gymd df(元号 年 月 日)
#' @name FD
y4md_gymd <- function( gymd ) {
#  y4md <- gymd[,1]   # 元号
#  y4md <- as.integer( y4md )
#  for( i in 1:length( y4md ) )
#  {
#    if      ( y4md[ i ] == 5 )
#      { y4md[ i ] <- 2018;next }  # 令和
#    else if ( y4md[ i ] == 4 )
#      { y4md[ i ] <- 1988;next } # 平成
#    else if ( y4md[ i ] == 3 )
#      { y4md[ i ] <- 1925;next  } # 昭和
#    else if ( y4md[ i ] == 2 )
#      { y4md[ i ] <- 1911;next  } # 大正
#    else
#        y4md[ i ] <- 1868       # 明治
#  }
  y4md <- gymd[,1]
  y4md <- chartr("54321", "RHSTM", y4md )# 元号
  y4md <- gsub("R", "2018", y4md )
  y4md <- gsub("H", "1988", y4md )

  y4md <- as.integer( y4md )
  y4md <- y4md + as.integer( gymd[,2] ) # 年
  y4md <- paste0( y4md
          #  ,"-",as.integer( gymd[,3] ) # 月
          #  ,"-",as.integer( gymd[,4] ) # 日
            ,"-" ,gymd[,3] # 月
            ,"-" ,gymd[,4]  # 日
          )
  # y4md <- as.Date( y4md ,origin = "1970-1-1")
  return( y4md )
}

#' @export
#' @param y4md 西暦
#' @name FD
gymd_y4md <- function( y4md ) {
  ve <- c( as.Date( y4md ) )
  gymd <- data.frame( matrix( NA ,length( ve ) ,4 ) )

  #  gymd[ 1:length( y4md ) ,1 ] <- 5 # 令和
  gymd[ ,1 ] <- 4 # 平成
  gymd[ ,2 ] <- as.integer( format( ve ,"%Y" ) ) -1988

  i <-  ( ve >= as.Date( "2019-5-1" ) ) # 令和
  if ( length( i ) != 0) {
    gymd[ i ,1 ] <- 5 # 令和
    gymd[ i ,2 ] <- as.integer( format( ve[ i ] ,"%Y" ) ) -2018
  }

  gymd[ ,3] <- as.integer( format( ve ,"%m" ) )
  gymd[ ,4] <- as.integer( format( ve ,"%d" ) )

  return( gymd )
}

# 内部 #' @export
#' @param jp 西暦
#' @name FD
ascii_jp <- function( jp ) {
  # checking R files for non-ASCII characters ... WARNING
  # ascii_日本語()    # NG
  # message("日本語") # ガセネタ
  ascii <- jp
  return( ascii )
}
