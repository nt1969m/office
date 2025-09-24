#' bank_i01
#' @export
#' @param df file
#' @param inf pf
#' @param col_i01 i01_cols
#' @name read_bank
bank_i01 <- function( df="" ,inf="" ,col_i01="" ) {

  if ( inf$bal == "0" ) {
    l2 <- paste0( "Bal = " ,inf$bal )
    message( l2 )
    return()
  }

  if( col_i01 == "" ){
    col_i01 <- system.file( "i01_col.csv" ,package="office") |>
      read.table( header = F ) |> t()
    # read.table( header = F ,stringsAsFactors = FALSE ) # NG
  } else {
    message( "col_i01 init" )

    # install.packages( "openxlsx" )
    # library( openxlsx )

    # 留意事項等
#    "https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho1/2/all2.pdf" |>
#      browseURL()

    # 入力・標準フォーム
    col_i01 <-
"https://www.e-tax.nta.go.jp/hojin/gimuka/csv_jyoho2/2/HOI010_4.0_.xlsx" |>
      openxlsx::read.xlsx(startRow = 2) |> colnames()

#    col_i01 |> dim() |> print()
    col_i01 |> print()
    col_i01 |> write.table( "col_i01.csv" ,row.names = F )

      #    message( col_i01 )
  }

#  t <- i01 |> paste( collapse = " " )
#  message( "p4: i01 = " ,t )

  c <- col_i01 |> length() # 戻し
#  c <- col_i01 |> nrow() # 却下
#  message("ncols(i01) = ",c)
  i01 <- matrix( ,1 ,c )
  colnames( i01 ) <- col_i01

  i01[1,] <- ""
  # ○ 文字列を改行する場合は、文字列全体を「"（ダブルクォーテーション）」で囲んでください。
  i01[1,1] <- 1
  i01[1,2] <- 0
  i01[1,3] <- inf$bank  |> substr( 8 ,19 ) # 前提：B_9999_ 11 文字以内
  i01[1,4] <- inf$i01_4 |> substr( 1 ,11 ) # 支 店 名     11 文字以内
  i01[1,5] <- inf$i01_5 |> substr( 1 ,10 ) # 種 類        10 文字以内
  i01[1,6] <- inf$i01_6 |> substr( 1 ,14 ) # 口 座 番 号  14 文字以内 13 桁までの数字及び「-（ハイフン）」1 文字を記録することができます。

  i01[1,8] <- "" # 摘 要        30 文字以内

#  df_ymd_bal <- subset(df ,df[,inf$y ] != "" & df[,inf$m ] != ""
#                       ,select = c(inf$y ,inf$m ,inf$d ,inf$bal )
#  )
##  df_ymd_bal <- subset( df ,!is.na( df[ ,inf$bal] ) && df[ ,inf$bal] != ""
##                        ,select = c( inf$y ,inf$bal ) ) # row number
  df_ymd_bal <- subset( df ,select = c( inf$y ,inf$bal ) )

  df_ymd_bal[ ,1 ] <- df_ymd_bal[ ,1 ] |> as.character()

  if ( inf$m != 0 ) df_ymd_bal[ ,1 ] <-
                    df_ymd_bal[ ,1 ] |> paste0( "-" ,df[ ,inf$m ] )

  if ( inf$d != 0 ) df_ymd_bal[ ,1 ] <-
                    df_ymd_bal[ ,1 ] |> paste0( "-" ,df[ ,inf$d ] )

  df_ymd_bal[ ,1 ] <- df_ymd_bal[ ,1 ] |> as.Date( format = inf$ymd )

  i <- which( is.na(df_ymd_bal[ ,1 ] ) | df_ymd_bal[ ,1 ]  == "" )
  if( i |> length() != 0 ) {
    df_ymd_bal <- df_ymd_bal[ -i , ]
  }
#  df_ymd_bal |> print()

  y_h <- df_ymd_bal[ ,1 ] |> head( n=1 ) # ymd
  y_t <- df_ymd_bal[ ,1 ] |> tail( n=1 ) # ymd

  if( y_h > y_t )  i <- 1
  else             i <- df_ymd_bal |> nrow()

  #  df_ymd_bal[,4] <- gsub( "\\D" ,"" ,df_ymd_bal[,4] ) |> as.integer() # 消し過ぎ
  df_ymd_bal[ ,2 ] <- gsub( "[\\\\,]" ,"" ,df_ymd_bal[ ,2 ] ) |> as.integer()
  #D  df_ymd_bal |> print() #D

  i01[ 1 ,7 ] <- df_ymd_bal[ i ,2 ]

  l <- col_i01 |> paste( collapse = "\t" )
  l <- "# " |> paste( l )
  #  l <- col_i01 |> unlist() |> paste( collapse = "\t" ) # 独自路線、撤回

  # l2 <- "#" |> paste( "ymd = " ,df_ymd_bal[ i ,1 ] )
  l2 <- paste0( "Bal : " ,i01[ 1 ,7 ] ,"\t Ymd : " ,df_ymd_bal[ i ,1 ] )
  message( l2 )
  l <- "# " |> paste( l2 ) |> rbind( l )

  # i01 |> print()

  l3 <- "#" |> paste( inf$bank ,inf$i01_4 ,inf$i01_5 ,inf$i01_6 )
  l <- l3 |> rbind( l )

  f <- paste0( inf$bank ,"_" ,inf$i01_4 ,"_" ,inf$i01_5 ,"_i01.csv")
  l |> iconv( to="CP932")|> writeLines( f )
  #l |> writeLines( f )

  #  " Write_eTax() start " |> print()
  #  Write_eTax(i01,f)
  #  " Write_eTax() end " |> print()
  write.table( i01,f
               ,quote = F
               ,sep = ","
               ,row.names = F
               ,col.names = F
               ,eol = "\r\n"
               ,na = ""
               ,fileEncoding = "CP932"
               ,append = T
  )
  message( "write : " ,f )
  return( i01 )
} #
