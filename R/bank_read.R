#' read_bank
#' @export
#' @param d folder
#' @param f file
#' @param p pf
#' @param cp code page
#' @param i01 cols
#' @param tax income tax
#' @param fuk special tax
#' @name read_bank
read_bank <- function( d=getwd(),f=".csv$",p="^Fin",cp="CP932",i01="" ,tax=15 ,fuk=2.1 ) {
  "p1: d=" |> message( "\"" ,d ,"\"" )

  "p2: f=" |> message( "\"" ,f ,"\"" )
  # f = "^2024"
  files <- dir(	d ,pattern = f ,ignore.case=T )
  if( files |> length() == 0 ) {
    message( "  : No files found !" )
    return()
  }
#  cp <- files[1] |> Encoding()
  message( "  :[1] " ,files[1] ,"\tEncoding = " ,cp )
#  df <- files[1] |> readLines()
  df <- files[1] |> readLines( warn=FALSE )

  f <- files[1]
  # message( f ,"\t", p   )
  #  row1 <- f |> readLines(n=1) |> iconv(from="Shift_JIS")
  # row1 <- f |> readLines(n=1) |>  iconv( to="UTF-8")
  row1 <- f |> readLines( n=1 ) |> iconv( from = cp )
#  cp <- f |> Encoding()
#  message( "Encoding = " ,cp )
#  if(cp!="unknown") row1 <- row1 |> iconv(from=cp)
  message( " row1 = ",gsub("[,\"]"," ",row1))
  # message("row1 = " ,row1)

  "p3: p=" |> message( "\"" ,p ,"\"" )
  pf <- dir(	d ,pattern = p ,ignore.case=T )
  if( pf |> length() == 0 ) {
    warning("Not found")
    return() }
  message( "  :[1] " ,pf[1] )

  infs <- pf[1] |> read.table(header=T)
  #  infs$r1[2] <- "操作日(年)" # Debug
  # i <- grep(row1,infs$r1)
  # grep( "^日付",row1)
  # grep( "操作日",row1)
  j <- nrow(infs)
  for ( i in 1:j ) {
    # i |> print() #
    message( i ," ",infs$bank[i] ,"\t : " ,infs$r1[i])
    if( length( grep( infs$r1[i] ,row1))) break
    if( i == j ) {
      warning( "All Rows (" ,j ,") mismatch")
      return()
    }
  }
  inf <- infs[i,]
  message("bank = ",inf$bank ,"\t : Row of Header = " ,inf$h )

#  if(inf$h != 1) {       # row of header
#    " Read_MS() start " |> print()
#    df <- Read_MS(d,f=f,"",n=inf$h) # temp
#    "Read_MS() end" |> print()
#  } else {
    # n <- n - 1}
    # df <- Read_MS(d,f=f,"")  # temp
    # read_fun <- read.csv(encoding = cp ,skip = inf$h - 1 )
#  }
  df <- NULL
  for( f in files ){
    "  : f=" |> message( "\"" ,f ,"\"" )
#    "  : f=" |> message( "\"" ,f ,"\":" ,inf$h )
    df <- rbind( df , read.csv( f ,fileEncoding = cp ,skip = (inf$h - 1) )
 )
  }
  i <- files |> length()
  r <- df |> nrow()
  message( "files : " ,i ,"\t rows : " ,r )

  if ( i > 1 ) {
    f <- inf$bank |> paste0( "_tra.csv" )
    df |> write.table( f
                 ,col.names = T )
    message( "write : " ,f )
  }

  i01 <- bank_i01( df=df ,inf=inf ,col_i01 = i01 ) # i01

  tb <- bank_tb( df=df ,inf=inf ,tax=tax ,fuk=fuk ) # tb

  return( tb )} # 本
