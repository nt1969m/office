library(devtools) # install.packages("devtools")
install_github("nt1969m/sbitools",force=T) #面倒な事を聞かれたら、3: Noneで、
packageVersion("sbitools")
library(sbitools) #

FY1 <- "/Users/nt1969m/OneDrive/office/確定申告/23期-2020.08.31"
FY <- "/Users/nt1969m/OneDrive/office/確定申告/24期-2021-08-31"
d <- file.path( FY ,"B_0988_SBI証券/CFD" )
csv <- CFD(d)
csv[,c(1,4,5,6)]

d <- file.path( FY ,"B_0988_SBI証券/割当株式" )
csv <- As(d)
d <- file.path( FY ,"B_0988_SBI証券/配当" )
csv <- Div(d)
sum(csv[,6])

dir.create( paste0( FY ,"/eTax" ) )
setwd( paste0( FY ,"/eTax" ) )
  print( c( "getwd()" ,getwd() ) )  # 目検（目視にて検証する)

  print( c("Read_eTax()" ,Read_eTax ) )

  # 前期を取り込み
  f_p_i06 <- file.path(	"/Users/nt1969m/OneDrive/office/確定申告/22期-2019.08.31"
                        ,"法人税"
                        ,"HOI060_4.0_有価証券"
  )
  print( c( "f_p_i06" ,f_p_i06 ) )

  # 当期を取り込み
  f_c_i06	<-	file.path( FY  ,"eTax" ,"HOI060_4.0_有価証券_控え" )
  print( c( "f_c_i06" ,f_c_i06 ) )

  print( c( "Read_Bank()" ,Read_Bank ) )

  f3 <- file.path( FY ,"B_0988_SBI証券" ,"約定_すべての商品_2020-08-31" )
  # sbi3 <-Read_Bank( f3 ,n=7 )
  print( c("f3" ,f3 ) )

  print( c( "Write_会計()" ,Write_会計 ) )

  # f3
  # 預け金　/ 有価証券
  #	write.csv(
  #		 sbi3[ sell , ]
  #		,paste0( f3 ,"_売り.txt" )
  #		,row.names=F
  #		)
  # 有価証券　/　預け金
  #,paste0( f3 ,"_買い.txt" )
  # 売買損益　/　有価証券
  #,paste0( f3 ,"_損益.txt" )

