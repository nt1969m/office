#' bank_tb
#' @param df file
#' @param inf pf
#' @param tax income tax
#' @param fuk special tax
#' @name read_bank
#' @export
bank_tb <- function( df ,inf ,tax ,fuk ) {
# B_0988_SBI証券・先物
  if ( inf$Dr == "0" & inf$Cr == "0") {
    return()
  }

  # 出入ではなく入出金なので。。
  dc <- c( inf$Dr ,inf$Cr )
  message( "Dr Cr = ",dc[1]," ",dc[2] ,"\t Rmk = " ,inf$Rmk )

  Rmk <- inf$Rmk |> strsplit(",") |> unlist()
#  message( "Rmk = " ,paste( Rmk ,collapse = " " ) ) # Too Much
  #  df[ ,c(dc)] <- df[ ,c(dc)] |> as.integer() # read_MS 不具合
  Rmk <- Rmk |> as.integer() # k.k.d. ここかっ!
#  n <- df[,Rmk] |> names()
#  df_Rmk <- subset( df ,select = n )
  df_Rmk <- subset( df ,select = Rmk )

  col_df_Rmk <- colnames( df_Rmk ) # |> list() # agg 対策

  col_df_Rmk |> paste( collapse = " " ) |> message()
  # print( c )

  i <- df_Rmk |> is.na()
  df_Rmk[ i ] <- ""
  #  df_Rmk |> print() #

  for (i in 1:ncol( df_Rmk ) ) {
    df_Rmk[,i] <- gsub("\\s+$" ,"" ,df_Rmk[,i])
  }

#  for ( i in 1:nrow( df_Rmk ) ) {
#    df_Rmk[i,1] <- df_Rmk[i,] |> paste( sep = " " )
#  }
#  df_Rmk[,1] <- paste(df_Rmk[,1:c] )

#  c <- df_Rmk |> ncol()
#  message( " cols " ,c )

  #  while(df_Rmk |> ncol() > 1 ) {
#    #    df_Rmk[,1] <- df_Rmk[,1] |> paste( df_Rmk[,2] )
#    df_Rmk[,1] <- df_Rmk[,1] |> paste( gsub("\\s+$" ,"" ,df_Rmk[,2]) )
#    df_Rmk <- df_Rmk[,-2]
#  }

#  while(df_Rmk |> ncol() |> length() != 0 ) {
#    df_Rmk[,1] <- df_Rmk[,1] |> paste( df_Rmk[,2] )
#    df_Rmk <- df_Rmk[,-2]
#  }

  #  df_Rmk |> print() #
  #  df_Rmk |> names() <- "Rmk"

# 個社対応：B_0009_三井住友銀行
  c <- ncol( df_Rmk )
  # print( c ) # ok
  # print( "r = " ,r)  #:invalid printing digits 90
  for ( i in nrow( df_Rmk ):2 ) {
    if ( inf$bal == "0" ) break # B_0988_SBI証券
    # print( i ) # ok
    if( # df[ i, inf$Dr ] |> is.na() &
        #df[ i ,inf$Cr ] |> is.na() &
        # df[ i ,inf$bal] |> is.na() ) {
       #df[ i, inf$Dr ] == "" & #:missing value where TRUE/FALSE needed
    #  !is.na(df[ i, inf$Dr ] ) & df[ i, inf$Dr ] == "" &
    #  !is.na(df[ i, inf$Cr ] ) & df[ i, inf$Cr ] == "" &
    #  !is.na(df[ i, inf$bal ]) & df[ i ,inf$bal] == "" ) { # Too Much
  #    !is.integer( df[ i, inf$Dr ] )  &&
  #    !is.integer( df[ i, inf$Cr ] )  &&
  #    !is.integer( df[ i, inf$bal ])  ) {                 # char
      ( is.na(df[ i, inf$bal ]) | df[ i ,inf$bal] == "" ) &&
      ( is.na(df[ i, inf$Dr ] ) | df[ i, inf$Dr ] == "" ) &&
      ( is.na(df[ i, inf$Cr ] ) | df[ i, inf$Cr ] == "" ) ) {
      #  print( i ,df[ i ,inf$m ] ,df[ i ,inf$d ] ) # invalid printing digits -2147483648
      j <- i - 1
      df_Rmk[ j ,c ] <-
      df_Rmk[ j ,c ] |> paste( df_Rmk[ i ,1:c ] ,sep = " " )
    }
  }

  # sum()の前に、
  # B_0988_SBI証券・先物
  if ( inf$Cr == "0" ) {
    df_dc_Rmk <- matrix( nrow = nrow(df) )
  } else {
    df_dc_Rmk <- subset( df ,select = inf$Cr )
  }
  if ( inf$Dr == "0" ) {
    df_dc_Rmk <- matrix( nrow = nrow(df) ) |> cbind( df_dc_Rmk )
  } else {
    df_dc_Rmk <- subset( df ,select = inf$Dr ) |> cbind( df_dc_Rmk )
  }
  df_dc_Rmk <- df_dc_Rmk |> cbind( df_Rmk )

#  df_dc_Rmk <- subset( df ,select = dc ) |> cbind( df_Rmk )
  #  i <- df_dc |> is.na()
  #  df_dc[i] <- ""
  #  df_dc <- gsub( "[¥,]" ,"" ,df_dc )
  # df_dc <- gsub( "[\\,]" ,"" ,df_dc ) # ¥, # "c(\"\" \"3000000\"
  # gsub( "\\D" ,"" ,df_dc ) # ¥, "" 消しすぎ
  # gsub( "\\D" ,"" ,df_dc[,1] ) |> as.integer() # ¥, ""
  #  df_dc[,1] <- gsub( "\\D" ,"" ,df_dc[,1] ) |> as.integer() # 消し過ぎ .ex - .
  #  df_dc[,2] <- gsub( "\\D" ,"" ,df_dc[,2] ) |> as.integer()
# B_0988_SBI証券・先物
  df_dc_Rmk[,1] <- gsub( "^--$" ,"" ,df_dc_Rmk[,1] )
  df_dc_Rmk[,2] <- gsub( "^--$" ,"" ,df_dc_Rmk[,2] )
# B_0988_SBI証券・現金
  df_dc_Rmk[,1] <- gsub( "^-$" ,"" ,df_dc_Rmk[,1] )
  df_dc_Rmk[,2] <- gsub( "^-$" ,"" ,df_dc_Rmk[,2] )
# B_0009_三井住友銀行
  df_dc_Rmk[,1] <- gsub( "[\\\\,]" ,"" ,df_dc_Rmk[,1] ) |> as.integer()
  df_dc_Rmk[,2] <- gsub( "[\\\\,]" ,"" ,df_dc_Rmk[,2] ) |> as.integer()

  # df_dc_Rmk |> print()

# 個社対応：B_99000_ゆうちょ銀行
  for ( i in nrow( df_dc_Rmk ):3 ) {
    if ( inf$bal == "0" ) break # B_0988_SBI証券
    # print( i ) # ok
    if( # 税金
        (  is.na(df[ i, inf$bal ]) | df[ i ,inf$bal] == "" ) &&
        ( !is.na(df[ i, inf$Dr ] ) & df[ i, inf$Dr ] != "" ) ) {
      j <- i - 1
      if( # 利子
        (  is.na(df[ j, inf$bal ]) | df[ j ,inf$bal] == "" ) &&
        ( !is.na(df[ j, inf$Cr ] ) & df[ j, inf$Cr ] != "" ) ) {
        k <- j - 1
        if( # 受取利子
          ( !is.na(df[ k, inf$bal ]) & df[ k ,inf$bal] != ""  ) &&
          ( !is.na(df[ k, inf$Cr ] ) & df[ k, inf$Cr ] != "" ) ) {
          df_dc_Rmk[ k ,1 ] <- df_dc_Rmk[ k ,2 ]
        }
      }
    }
  }

#  tb <-
#    df_dc |>
#    aggregate( by = list( df_Rmk )
#               ,FUN=sum, na.rm = TRUE)
# df_dc_Rmk <- df_dc |> cbind( df_Rmk )
#  df_dc_Rmk <- df_dc |> cbind( df_Rmk[,1] )

#  if ( col_df_Rmk |> length() == 1 ) {
#    col_df_Rmk |> mode() |> print()
    #    col_df_Rmk <- list( col_df_Rmk )
#  }

#  df_Rmk <- df_dc_Rmk[,-c(1,2)]  |> list()
#  df_dc_Rmk |> print() #debug
  tb <-
    df_dc_Rmk[ 1:2 ] |>
    aggregate( by = df_dc_Rmk[ col_df_Rmk ]
               ,FUN=sum, na.rm = TRUE)
#  df_dc_Rmk |> print() #debug
  #    df_dc_Rmk[,c(1,2)] |>
#    df_dc_Rmk[,c(1,2)] |>
#     aggregate( by = df_Rmk #
# #    aggregate( by = df_dc_Rmk[ ,-c(1,2) ] #
#  aggregate( by = df_dc_Rmk[ ,col_df_Rmk ] # ncol 1 : 'by' must be a list
# # aggregate( by = df_dc_Rmk[ ,Rmk ] #: undefined columns selected
# # aggregate( by = df_dc_Rmk[ ,c( col_df_Rmk ) ]  #: 'by' must be a list
# # aggregate( by = list( df_dc_Rmk[ ,col_df_Rmk ] ) #: undefined columns selected
# # aggregate( by = list( col_df_Rmk ) # arguments must have same length
# # aggregate( by = 3:4
# # aggregate( by = list( 3:(2+c) )
# # aggregate( by = list( df_dc_Rmk[,3:c] )

#  m <- ncol( df_dc_Rmk ) - 1
#  tb <- apply(df_dc_Rmk[,1:2] ,m ,function(x){
##  tb <- apply(df_dc_Rmk[,1:2] ,3 ,function(x){
##      tapply( x ,df_dc_Rmk[,3] ,FUN = sum ,na.rm=T)
    # tb <- apply(df_dc_Rmk[,1:2] ,2 ,function(x){
      #  tapply( x ,df_dc_Rmk[,-c(1,2)] ,FUN = sum ,na.rm=T)
##  })

#  tb |> print() # 1
  #  tb |> ncol() |> print()
##  Rmk <- tb |> rownames( tb )
##  tb <- tb |> cbind( Rmk ) #:
  #  tb <- tb[ ,c( 2 ,3 ,1 ) ] #: subscript out of bounds
#  tb <- tb[ ,c( m ,m+1 ,1:m-1 ) ]
  # m <- ncol(tb)
#  tb <- cbind( tb[ ,m ] ,tb[ ,-m ] ) # Cr
#  tb <- cbind( tb[ ,m ] ,tb[ ,-m ] ) # Dr
  #  tb |> View()

#  tb <- tb[ ,col_dc ] |> cbind( tb[ ,-col_dc ] )
#  tb <- tb[,c(m,m+1)] |> cbind( tb[,-c(m,m+1)] )
  # tb <- cbind( tb[,-c(col_df_Rmk) ] ) # : invalid argument to unary operator
  # tb <- tb[,c(2,3,1)]
  col_df_dc_Rmk <- df_dc_Rmk |> colnames()
  tb <- tb[,col_df_dc_Rmk]
  names(tb)[1] <- "Dr"
  names(tb)[2] <- "Cr"
#  names(tb)[3] <- "Rmk"
#  colnames( tb ) <- c( "Dr" ,"Cr" ,"Rmk" )

  Dr <- tb[,1] |> sum( na.rm=T )
  Cr <- tb[,2] |> sum( na.rm=T )
  Dif <- Cr - Dr

  i <- which(tb[,1] == 0 & tb[,2] == 0 )
#  i |> print()
  if( i |> length() != 0)  tb <- tb[-i,]

  tb |> print()
  # ok

#  #  f_tb  |> print()
# #  l <- "Dr\tCr\tRmk"
  col_tb <- colnames( tb )
  l <- col_tb |> paste( collapse = "\t")

#  l <- "#" |>  paste( Dr ,Cr ,": Dif( Cr - Dr ) = ",Dif ) # |> rbind( l )
  l2 <- "#" |>  paste( Dr ,Cr ,": Dif( Cr - Dr ) = ",Dif )
  message( l2 )
  l <- l2 |> rbind( l )

  l3 <- "#" |> paste( inf$bank ,inf$i01_4 ,inf$i01_5 ,inf$i01_6 )
  l <- l3 |> rbind( l )

  # f <- inf$bank |> paste0("_tb.csv")
  f <- paste0( inf$bank ,"_" ,inf$i01_4 ,"_" ,inf$i01_5 ,"_tb.csv")

  l |> writeLines( f )

  tb |> write.table( f
                     ,row.names = F
                     ,col.names = F
                     ,append = T
  )

  "inf$int = " |> message( inf$int )
  #  if( !is.na( inf$int ) ) {
  if( !is.na( inf$int ) ) {

    #    i <- which( df_Rmk[,1] == inf$int )
    # i <- grep( inf$int ,df_dc_Rmk[ ,col_df_Rmk ] ) # 返値、colとなる
    i <- grep( inf$int ,df_dc_Rmk[,3] )

    if( i |> length() == 0) {
     message("Nothing " ,inf$int )
    } else {
#      i |> print()
      t <- i |> paste( collapse = " " )
      message( t ," : Row of " ,inf$int )
      df_int <- df_dc_Rmk[ i , ]
      #      df_int <- df_dc[ i , ] |> cbind( df_Rmk[ i ] )
      df_int[,1] <- 0
      # cat(stringi::stri_escape_unicode("税引後"))
      #df_int[,2] |> paste(" ") |> message(":税引後")
      df_int[,2] |> paste(" ") |> message(":\u7a0e\u5f15\u5f8c")

      df_int[,2] <- df_int[,2] / (100 - (tax + tax * fuk / 100 ) ) * 100
      df_int[,2] <- df_int[,2] |> trunc() # 税引前
      #df_int[,2] |> paste(" ") |> message(":税引前　※推測")
      df_int[,2] |> paste(" ") |> message(":\u7a0e\u5f15\u524d\u3000\u203b\u63a8\u6e2c")

    #  df_int[,2] <- df_int[,2] / 100 * tax
      df_int[,2] <- df_int[,2] * ( tax + tax * fuk / 100 ) / 100
      df_int[,2] <- df_int[,2] |> trunc() # income tax
    #  df_int[,2] |> paste(" ") |> message(":所得税")

    #  df_int[,2] <- df_int[,2] + df_int[,2] * fuk / 100
    #  df_int[,2] <- df_int[,2] |> trunc() # 復興特別所得税

      #df_int[,2] |> paste(" ") |> message( ":国税")
      df_int[,2] |> paste(" ") |> message( ":\u56fd\u7a0e")
      colnames( df_int ) <- colnames( tb )

      df_int |> write.table( f
                       ,row.names = F
                       ,col.names = F
                       ,append = T
      )

#      df_tax <- subset( df_int ,select = c( 2,1,3) )
      df_tax <- df_int
      df_tax[,1] <- df_int[,2]
      df_tax[,2] <- df_int[,1]
      #df_tax[,3] <- "国税"
      df_tax[,3] <- "\u56fd\u7a0e"
      df_tax |> write.table( f
                           ,row.names = F
                           ,col.names = F
                           ,append = T
      )
      tb <- rbind( tb ,df_int ,df_tax )
    }
  }
  message( "write : " ,f )
  return(tb)
} # 本
