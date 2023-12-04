rm(list = ls())
library(RMeCab)
library(shiny)
library(tidyverse)
library(visNetwork)
library(geomnet)
library(igraph)
library(tm)
library(brio)
library(wordcloud2)

part <- c("名詞", "動詞",  "形容詞", "副詞")
reject_word = c("の","こと","す","よう","する","さ","もの", "いる", "てる", "ん")
na_mozi <- c("null", "NA", "*")

dic_path_list <- list.files(path = "mecab-ipadic-2.7.0-20070801-neologd-20200910/", pattern = "*.csv", full.names = T)

DF_DIC <- NULL

for (i in 1:length(dic_path_list)){
  dic_tmp <- read.csv(dic_path_list[i], header = F)
  
  DF_DIC <- rbind(DF_DIC, dic_tmp)
} 

DF_DIC <- DF_DIC[c(1, 11)]

names(DF_DIC) <-c("exercise", "origin")


ui <- shinyUI(navbarPage("共起ネットワークを作ってみよう",
                         tabPanel(
                           "データ整形",
                           sidebarPanel(
                             fileInput(
                               "csv_file",
                               label = "読み込ませたい元のcsvテーブルを入れてください",
                               accept = c("text/csv"),
                               multiple = FALSE,
                               width = "80%"
                             ),
                             uiOutput("free_write"),
                             uiOutput("group_array"),
                             uiOutput("group_label"),
                             
                           ),
                           
                           mainPanel(
                             downloadButton("downloadData",label = "綺麗にした文字列をダウンロードしてね。"),
                           )
                         ),
                         tabPanel("綺麗にしたテキストを使ってネットワーク図作成",sidebarLayout(
                           sidebarPanel(
                             fileInput(
                               "text_file",
                               label = "整形したテキストを読み込んでね"
                             ),
                             sliderInput(
                               "freq_slider",
                               "単語自体の出現回数でフィルター",
                               min = 0,
                               max = 100,
                               value = 100
                             )
                           ),
                           mainPanel(
                             tabsetPanel(
                               type = "tabs",
                               tabPanel("ネットワーク図", visNetworkOutput("Vis_Plot")),
                               tabPanel("ワードクラウド", wordcloud2Output("word_cloud")),
                               tabPanel("元の文章", tableOutput("original_table"))
                             )
                           ),
                         )
                         ) 
)
)

# Server logic
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 30*1024^2)
  
  ###ここからデータフレーム取り出して自由記述列とグループ処理をするよ
  DF <- reactive({
    tmp <- input$csv_file
    if (is.null(tmp)){
      return(NULL)
    } else {
      df <- read.csv(tmp$datapath, header=TRUE)
      return(df)
    }
  })
  
  output$free_write <- renderUI({
    selectInput(inputId = "selected_free_write", 
                label = "自由記述の列を選んでください", 
                choices = names(DF()))
  })
  
  output$group_array <- renderUI({
    selectInput(inputId = "selected_group", 
                label = "グループ判定の列を選んでください", 
                choices = names(DF()))
  })
  
  
  group_labels <- reactive({
    if(is.null(input$selected_group)){
      return(NULL)
    } else{
      label <- DF() %>% data.frame(
      ) %>% select(
        .,
        input$selected_group
      ) %>% distinct()
      return(label)
    }
  })
  
  
  output$group_label <- renderUI({
    selectInput(inputId = "selected_group_label", 
                label = "グループごとのラベルの値を選んでください", 
                choices = c("all",group_labels())
    )
  })
  
  DF_selected <- reactive({
    if (input$selected_group_label == "all"){
      DF() %>% data.frame() %>% select(
        .,
        input$selected_free_write
      ) %>% return()
    } else {
      DF() %>% data.frame() %>% filter(
        .,
        eval(parse(text = paste0(input$selected_group, "==", '"', input$selected_group_label, '"')))
      ) %>% select(
        .,
        input$selected_free_write
      )  %>% return()
    }
    
  })
  
  ###ここまででデータフレーム取り出して自由記述列とグループ処理をしたよ
  
  ###出てきた配列をくっつけて文字にまとめるよ
  
  
  text <- reactive({
    eval(parse(text = paste0("DF_selected()$",  input$selected_free_write))) %>% ifelse(
      . %in% na_mozi, 
      NA, 
      .
    ) %>% data.frame(
      .
    ) %>% na.omit(
      .
    ) %>% data.frame(
      .
    ) %>% unlist(
      .
    ) %>% as.array(
      .
    ) %>% paste(
      ., 
      collapse = ""
    ) %>% gsub(
      "\n", "", .
    ) %>% tm::stripWhitespace(
      .
    ) %>% tm::removePunctuation(
      .
    ) 
    
    
  })
  
  
  ## まとめたものをテキストにして吐き出すよ。
  output$downloadData = downloadHandler(
    filename = "text.txt",
    content = function(filename){
      text() %>% brio::write_file(
        .,
        path = filename                         
      )
    }
  )
  
  
  ## 吐き出したテキストを読み込んで、諸々に必要なデータセット作るよ。

  
  ##　単語の頻度を取り出すよ
  RAW_DF_FREQ <- reactive({
    tmp <- input$text_file
    if (is.null(tmp)){
      return(NULL)
    } else {
      tmp_df <- RMeCab::RMeCabFreq(filename = tmp$datapath) %>% data.table::data.table(.) %>% data.frame(.)
      tmp_df$Term <- str_remove(tmp_df$Term, "\n")
      
      tmp_df <- tmp_df %>% filter(
        .,
        Info1 %in% part
      ) %>% select(
        .,
        Term,
        Freq
      ) %>% group_by(
        .,
        Term
      ) %>% summarise(
        .,
        count = sum(Freq)
      ) %>% ungroup(
        .
      ) %>% filter(
        ., 
        !(Term %in% reject_word)
      )  
      
      return(tmp_df)
    }
  })
  
  ##　単語の共起頻度を取り出すよ
  RAW_DF_COOC <- reactive({
    tmp <- input$text_file
    if (is.null(tmp)){
      return(NULL)
    } else {
      
      tmp_df <-  RMeCab::NgramDF(filename = tmp$datapath, type = 1, N = 2, pos = part) %>% data.frame(.) 
      
      tmp_df <- tmp_df %>% filter(
        ., 
        !(Ngram1 %in% reject_word)
      ) %>% filter(
        .,
        !(Ngram2 %in% reject_word)
      )
      
      return(tmp_df)
    }
  })
  
  ##　出現頻度フィルターの上限下限をアップデート
  observeEvent(input$text_file, {
    updateSliderInput(
      session,
      "freq_slider",
      min = min(RAW_DF_FREQ()$count), 
      max = max(RAW_DF_FREQ()$count),
      value = 1,
      step = 1
    )
  })
  
  DF_FREQ <- reactive({
    freq_filter_num = input$freq_slider
    if (is.null(freq_filter_num)){
      return(NULL)
    } else {
      tmp_freq_df <- RAW_DF_FREQ() 
      tmp_cooc_df <- RAW_DF_COOC() 
      
      tmp_freq_df <- tmp_freq_df %>% filter(
        .,
        count >= freq_filter_num
      )
      tmp_freq_df <- tmp_freq_df %>% filter(
        .,
        Term %in% c(unlist(tmp_cooc_df[,1:2]))
      )
      tmp_cooc_df <- tmp_cooc_df %>% 
        filter(., Ngram1 %in% tmp_freq_df$Term) %>% 
        filter(., Ngram2 %in% tmp_freq_df$Term)
      
      return(tmp_freq_df) 
    }
    
  })
  
  
  ##　共起表現頻度をスライドバーでフィルターかけるよ　
  
  DF_COOC <- reactive({
    freq_filter_num = input$freq_slider
    if (is.null(freq_filter_num)){
      return(NULL)
    } else {
      tmp_freq_df <- RAW_DF_FREQ() 
      tmp_cooc_df <- RAW_DF_COOC() 
      
      tmp_freq_df <- tmp_freq_df %>% filter(
        .,
        count >= freq_filter_num
      )
      tmp_freq_df <- tmp_freq_df %>% filter(
        .,
        Term %in% c(unlist(tmp_cooc_df[,1:2]))
      )
      tmp_cooc_df <- tmp_cooc_df %>% 
        filter(., Ngram1 %in% tmp_freq_df$Term) %>% 
        filter(., Ngram2 %in% tmp_freq_df$Term)
      
      return(tmp_cooc_df) 
    }
    
  })
  
  ##単語出現頻度をノードサイズとして綺麗にするよ
  
  NODE_DF <- reactive({
    in_node_clean <- DF_FREQ() %>% as.data.frame()
    names(in_node_clean) <- c("id", "value")
    in_node_clean <- in_node_clean %>% mutate(
      .,
      label = id
    )
  })
  
  ##共起頻度を矢印の濃さとして表現するよ
  EDGE_DF <- reactive({
    in_edge_clean <- DF_COOC() %>% as.data.frame()
    names(in_edge_clean)<- c("from", "to", "width_original")
    in_edge_clean <- in_edge_clean %>% mutate(
      .,
      width = as.vector(
        (scale(width_original)) - min(scale(width_original))
      )
    ) %>% select(
      .,
      from,
      to,
      width
    )
    names(in_edge_clean) <- c("from", "to", "width")
    
    return(in_edge_clean)
    
  })
  
  
  ##共起頻度を共起ネットワークを書くよ。function(nodes)で、気になる単語を選ぶよ
  
  output$Vis_Plot <- renderVisNetwork({
    visNetwork(NODE_DF(),EDGE_DF()) %>% visIgraphLayout(
    ) %>%
      visOptions(
        highlightNearest = list(enabled = T, degree = 1, hover = T)
      ) %>% visLayout(randomSeed = 2525) %>% visEvents(
        click = "function(nodes) {
          Shiny.onInputChange('selected_node', nodes.nodes);
        }"
      )
  })
  
  node_exercised <- reactive({
    if (is.null(input$selected_node)){
      return(NULL)
    } else {
      tmp_df <- DF_DIC %>% filter(
        .,
        origin == input$selected_node
      )
      tmp_array <- tmp_df$exercise %>% as.array(.)
      return(tmp_array)
    }
  })
  
  ## ワードクラウドを作るよ。
  output$word_cloud <- renderWordcloud2({
    wordcloud2(RAW_DF_FREQ(), size=1.6, color='random-light', backgroundColor="black")
  })
  
  ##選んだノードの単語が含まれている元のテーブルの行を吐き出すよ
  
  output$original_table <- renderTable({
    DF_selected() %>% filter(
      .,
      str_detect(
        eval(parse(text = input$selected_free_write)),
        eval(parse(text = paste0('"', paste(node_exercised(),collapse = '|'), '"')))
      )
    )
  })
}

# Run the app
shinyApp(ui, server)


