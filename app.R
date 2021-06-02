library(shiny)

options(warn=-1)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
#dir='C:/Users/kdh2/Desktop/project/data/datasets/data'
dir="data/"
#dir='C:/Users/Dong Hyun/Downloads/tmp-main/tmp-main/data/'
file_list=list.files(dir)
data=data.frame()
df_list=c()
for (file in file_list){ 
  temp=read.csv(paste(dir,file,sep='/'),header=TRUE,sep=',',stringsAsFactors = FALSE)
  eval(parse(text=str_c(substr(file,0,nchar(file)-4),'=temp')))
  df_list=df_list %>% append(substr(file,0,nchar(file)-4))
}
hitter_df=hitter_2016
for (i in df_list[2:5]){
  eval(parse(text=str_c('hitter_df=rbind(hitter_df,',i,')')))
}


pitcher_df=pitcher_2016
for (i in df_list[7:10]){
  eval(parse(text=str_c('pitcher_df=rbind(pitcher_df,',i,')')))
}

profile_df=q_2016
for (i in df_list[12:15]){
  eval(parse(text=str_c('profile_df=rbind(profile_df,',i,')')))
}
team_hitter_df=team_hitter_2016
for (i in df_list[17:20]){
  eval(parse(text=str_c('team_hitter_df=rbind(team_hitter_df,',i,')')))
}


to_won=function(x){
  if(str_detect(x,'만원')){
    a=str_replace(x,'만원','') %>% as.numeric()*10000
  }else{
    x=str_replace(x,'엔','')
    a=str_replace(x,'달러','') %>% as.numeric()*1200
  }
  return(a)
}


hitter_df=hitter_df %>% mutate(GYEAR=as.integer(substring(GDAY_DS,0,4)))


profile_df=profile_df %>% rename('P_ID'='PCODE')
hitter_df=hitter_df %>% inner_join(profile_df,by=c('P_ID','T_ID','GYEAR'))
names(hitter_df)<-c('게임키','게임날짜','팀','상대팀','더블헤더','초말','선수코드','선발여부','타순','타자','타수',
                    '타점','득점','안타','이루타','삼루타','홈런','도루','도루실패','희타','희비','볼넷','고의4구','사구',
                    '삼진','병살타','실책','잔루','득점권타율','득점권타수','득점권안타','등록년도','이름','포지션','나이',
                    '연봉')

hitter_df$연봉=sapply(hitter_df$연봉,to_won)
hitter_df=hitter_df %>% mutate(SLG=(안타+사구+볼넷)/(타수+볼넷+사구+희비),
                     OBA=(안타+2*이루타+3*삼루타+4*홈런)/타수,
                     W_OPS=0.57*SLG+0.43*OBA)

library(lubridate)
hitter_df=hitter_df %>% na.omit() %>%  mutate(month=month(ymd(게임날짜)),day=wday(ymd(게임날짜),label=TRUE))
  
profile_df$MONEY=sapply(profile_df$MONEY,to_won)

year_team_money=profile_df %>% group_by(GYEAR,T_ID) %>% na.omit() %>%  summarise(총연봉=sum(MONEY))

ind_pit=pitcher_2016
for (i in df_list[7:10]){
  eval(parse(text=str_c('ind_pit=rbind(ind_pit,',i,')')))
}


win_df=team_hitter_df %>% inner_join(ind_pit %>% select(G_ID,T_ID,VS_T_ID,HEADER_NO,TB_SC,WLS)) %>% mutate(GYEAR=as.integer(substring(GDAY_DS,0,4))) %>% filter(WLS=='W') %>% group_by(GYEAR,T_ID) %>% summarise(WINS=n())

a=win_df %>% group_by(GYEAR)  %>% summarise(rank=rank(WINS, ties.method = "first")) 

win_df$rank=a$rank

money_and_wins=win_df %>% inner_join(year_team_money) 
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  textInput("text", label = h3("선수 이름을 입력하세요"),value="김강민") 
  ,
  plotOutput(outputId = "distPlot"),
  selectInput("team",label = h3("팀명을 선택하세요"),choices=c("HH","LG","HT","NC","SK","KT","LT","WO","SS","OB"))
               
  ,
  
  textOutput(outputId = "text1"),
  plotOutput(outputId = "distplot2")
)

server <- function(input, output){
  
  output$distPlot <- renderPlot({
    
    temp=hitter_df %>% filter(이름==input$text) %>% mutate(날짜=paste(as.character(등록년도),as.character(month))) %>% group_by(등록년도) %>%summarise(W_OPS평균=mean(W_OPS),연봉=mean(연봉)) 
    temp  %>% ggplot()+theme_light()+scale_fill_brewer(palette = 'PuRd',name='년도')+geom_bar(mapping=aes(x=등록년도,y=연봉/100000000,fill=factor(등록년도)),stat="identity")+geom_line(data=temp,mapping=aes(x=등록년도,y=W_OPS평균*10,group=1),color="white",size=1.5)+
      geom_line(data=temp,mapping=aes(x=등록년도,y=W_OPS평균*10,group=1),color="purple",size=1)+scale_y_continuous(name='연봉(억)',sec.axis = sec_axis(~.*1,name='W_OPS'))+
      xlab('년도')
    })
  output$text1 <-renderText({ 
    tmp2=hitter_df %>% filter(이름==input$text) %>% tail()
    tmp3=tmp2$팀 %>% unique()
    return(paste("현재",input$text,"의 팀은", tmp3[1],"입니다"))
  })
  output$distplot2 <- renderPlot({
    money_and_wins %>% filter(T_ID==input$team) %>% ggplot()+
      geom_bar(aes(x=GYEAR,y=총연봉/1000000000,fill=factor(GYEAR)),stat = 'identity')+
      theme_light()+
      scale_fill_brewer(palette = 'PuRd',name='년도')+
      geom_line(money_and_wins %>% filter(T_ID==input$team) ,mapping = aes(x=GYEAR,y=rank),color='white',size=1.5)+
      geom_line(money_and_wins %>% filter(T_ID==input$team) ,mapping = aes(x=GYEAR,y=rank),,color='purple',size=1)+
      scale_y_continuous(name='총연봉(십억)',sec.axis = sec_axis(~.*1,name='rank'))+
      xlab('년도')
  })
}

 
shinyApp(ui = ui, server = server)


