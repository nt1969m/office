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

# #' @export
# #' @param s Settlement date
# #' @name i06
# i06.sheet <- function( s="2021-04-01" ) {
#  if ( s >= "2019-04-01" )  return("HOI060_4.0_⑥有価証券" )
#  else                      return("HOI060_3.0_⑥有価証券" )
# }
sheet <- matrix(
    c( "2019-04-01" ,"HOI060_4.0_⑥有価証券"
      ,"2018-04-01" ,"HOI060_3.0_⑥有価証券"  )
    ,2 ,2 ,TRUE )
write.csv( sheet
           ,"i06_sheet.csv"
            ,row.names=F )

# #' @export
# # #' @param ... nothing
# # #' @return i06 df
# #' @name i06
# #i06.init <- function() {
# i06_init <- function() {
    # 列数
	i06_init <- data.frame( matrix(  NA ,0 ,18 ) )
# 列名
#	colnames( i06.init ) <- c(
	i06_col  <- c(
	 # "フォーマット区分="
	   "6"
	  ,"行区分="
	  ,"区分="
	  ,"種類="
	  ,"銘柄=" ,"期末現在高.数量=" ,"帳簿価額=" ,"時価及び帳簿価額="
	  # ,"期中増(減).異動年月日.元号=" ,"年=" ,"月=" ,"日="
	  ,"元号=" ,"年=" ,"月=" ,"日="
	  ,"異動事由=" ,"数量=" ,"金額="
	  ,"証券会社=" ,"住所=" ,"摘要=" )

	#	save( i06 , file = "i06.RData")
	# load("i06.RData")

	# devtools::use_data_raw()
	# devtools::use_data( i06.init )
	# package

	mode( i06_col )
  write.csv( i06_col
            ,"i06_col.csv"
            ,row.names=F )

	# return( i06 )
# }

#  i06_sbi_約定 <- function( sbi ) {
sheet <- matrix( c(
     "株式現物買" ,1  ,NA
    ,"株式現物売" ,-1 ,"売却"
    ,"信用新規売" ,NA ,NA
    ,"信用返済買" ,NA ,NA
     )
  ,4 ,3 ,TRUE )
colnames( sheet ) <- c( "取引" ,"受渡金額.決済損益" ,"追加文言" )
write.csv( sheet
           ,"sbi_5.csv" #col 5 取引 character ,
                        #col 14 受渡金額.決済損益 character
                        #i06 #列13() 異動事由= 追加文言
           ,row.names=F )

#  i06_kabu_約定 <- function( kabu ) {
sheet <- matrix( c(
   "買" ,1  ,NA
  ,"売" ,-1 ,"売却"
)
,2 ,3 ,TRUE )
colnames( sheet ) <- c( "売買区分" ,"受渡金額" ,"追加文言" )
write.csv( sheet
           ,"kabu_6.csv"
           #col 6 売買区分 character
           #col 9 受渡金額 numeric
           #i06 #列13() 異動事由= 追加文言
           ,row.names=F )

# i06_異動明細 <- function( i06 ) {
#  idou <- data.frame( ( matrix( NA ,nrow( i06 ) ,4 ) ) )
i06_idou_col <- c( "種類=" ,"異動年月日=" ,"数量=" ,"金額=" )
write.csv( i06_idou_col
           ,"i06_idou_col.csv"
           ,row.names=F )
