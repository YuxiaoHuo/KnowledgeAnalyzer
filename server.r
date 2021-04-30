library(shinyjs)

NAME = "drfortino"
PASSWORD = "drfortino"

# load CSV file
f_testAnswer = read.csv("test_answer.csv")
f_testQuestion = read.csv("test_question.csv")
f_test= read.csv("test.csv")
f_domain = read.csv("Domain.csv")

colnames(f_domain)[1] <- "Test"
colnames(f_test)[1] <- "Test"
colnames(f_testAnswer)[1] <- "Test"
colnames(f_testQuestion)[1] <- "Test"

testQA <- merge(f_testQuestion,f_testAnswer,all.X= T, all.Y = T)
# define some reusable functions

#----------------func 0-----------------
# a query to select between test
get_test<- function(num){
  testQA[testQA$Test == num, ]
}
#---------------------------------------

# ---------------func 1-----------------
# get questions for selected test
get_questions <- function(test_df,num){
  paste(test_df$Question[num],".",test_df$Problem[num])
}
# --------------------------------------

# ---------------func 2-----------------
# get answers for selected test
get_answers<- function(test_df, num){
  rowVal = as.list(test_df[num,])
  out = c()
  for (i in 5:length(rowVal)) {
    if(rowVal[i] != ""){
      out <- c(out,as.character(rowVal[i]))
    }
  }
  out
}
# --------------------------------------


# ---------------func 3-----------------
# to random select answers and record the right one
shuffle_answers <- function(test_df){
  correct_answer = c()
  for (i in 1:nrow(test_df)) {
    r = test_df[i, -c(1:4)]
    answers = r[r != ""]
    num_valid_answer = length(answers)
    s = sample(num_valid_answer)
    correct_answer <- c(correct_answer, which(s == 1))
    test_df[i, c(5:(4 + num_valid_answer))] = answers[s]
  }
  foo <- vector("list", length = 2)
  names(foo) <- c("test_df", "correctAnswer")
  foo$test_df = test_df
  foo$correctAnswer = correct_answer
  foo
}
# --------------------------------------

# ---------------func 4-----------------
# get correct answer
get_correct_answer <- function(correctAnswer, num){
  correctAnswer[testQA$Test == num]
}
# --------------------------------------

# ---------------func 5-----------------
# after record all the answers, generate the domain grade
domainGrade <- function(domainList, answerList){
  c = rep(0,max(domainList))
  d = rep(0,max(domainList))
  for(i in 1:length(domainList)){
    c[domainList[i]] = c[domainList[i]] + answerList[i]
    d[domainList[i]] = d[domainList[i]] + 1
  }
  c/d
}
# --------------------------------------

assamble_table <- function(domainG,t,grade){
  if(length(domainG)< 7) {domainG = c(domainG, rep(0,2))}
  result = data.frame(
    test = t,
    domain_1 = domainG[1],
    domain_2 = domainG[2],
    domain_3 = domainG[3],
    domain_4 = domainG[4],
    domain_5 = domainG[5],
    domain_6 = domainG[6],
    domain_7 = domainG[7],
    total_score = grade
  )
  result
}
# ---------------initiating-----------------
foo = shuffle_answers(testQA)
testQA = foo$test_df 
correctAnswer = foo$correctAnswer
# ------------------------------------------

server <- function(input, output, session) {
  
  # initiatives
  counters <- reactiveValues()
  counters$page <- 0
  counters$n <- 0
  counters$selection <- vector(length = 0)
  counters$test <- testQA
  counters$correctAnswer <- c()
  counters$grade <- 0
  
  #-----------------------------------------------------------------------------
  # switch between test, javascript include
  observeEvent(input$start,{
    if(!is.null(input$RBstart)){
      counters$test <- get_test(input$RBstart)
      counters$correctAnswer <- get_correct_answer(correctAnswer,input$RBstart)
      counters$n <- nrow(counters$test)
      counters$selection <- vector(length = counters$n)
      shinyjs::hide("RBstart")
      shinyjs::hide("start")
      shinyjs::hide("scoreButton")
      shinyjs::show("goButton")
      shinyjs::show("backButton")
      shinyjs::show("question")
      shinyjs::show("answers") 
      shinyjs::show("submitButton")
    }
  })

  
  observeEvent(input$submitButton,{
    shinyjs::hide("goButton")
    shinyjs::hide("backButton")
    shinyjs::hide("question")
    shinyjs::hide("submitButton")
    shinyjs::hide("answers")
    shinyjs::show("grade")
    # update to file
    domainG = domainGrade(
      counters$test$Domain,
      as.integer(counters$correctAnswer == counters$selection)
    )
    result = assamble_table(domainG, input$RBstart, counters$grade)
    if(!file.exists("test_result.csv")){
      write.csv(result,"test_result.csv",row.names = F)
    }else{
      f = read.csv("test_result.csv")
      print(result)
      f = rbind(f, result)
      write.csv(f,"test_result.csv",row.names = F)
    }
  })
  
  viewscore <- observeEvent(input$scoreButton,{
    shinyjs::hide("RBstart")
    shinyjs::hide("start")
    shinyjs::show("toMain")
    shinyjs::show("nameInput")
    shinyjs::show("passwordInput")
    shinyjs::show("loginButton")
  })
  
  backscore<- observeEvent(input$toMain,{
    shinyjs::show("RBstart")
    shinyjs::show("start")
    shinyjs::show("scoreButton")
    shinyjs::hide("toMain")
    shinyjs::hide("nameInput")
    shinyjs::hide("passwordInput")
    shinyjs::hide("loginButton")
    shinyjs::hide("scoretable")
    shinyjs::hide("download")
  })
  #-----------------------------------------------------------------------
  
  page_forward <- observeEvent(input$goButton,{
    # ---------- test------------
    if(!is.null(input$answerButton)){
      counters$selection[counters$page+1] <- input$answerButton
    }
    # ---------------------------
    counters$grade <- sum(counters$correctAnswer == counters$selection)
    counters$page <- counters$page + 1
  })
  
  page_backward <- observeEvent(input$backButton,{
    counters$page <- counters$page - 1
  })
  
  output$question <- renderText({
    get_questions(counters$test,counters$page%%counters$n + 1)
  })

  output$answers <- renderUI({
    radioButtons("answerButton", "select from following", 
                 choiceNames = get_answers(counters$test,counters$page%%counters$n + 1), 
                 selected = counters$selection[counters$page%%counters$n+1],
                 choiceValues = 1:length(get_answers(counters$test,counters$page%%counters$n + 1))
                 )
  })
  
  output$grade <- renderTable({
    assamble_table(
      domainGrade(
        counters$test$Domain,
        as.integer(counters$correctAnswer == counters$selection)
      ), 
      input$RBstart, 
      counters$grade
    )
  })
  
  output$scoretable <- renderTable({
    read.csv("test_result.csv")
  })
  
  output$download <- downloadHandler(
    filename = function(){
      paste("studentScore","csv",sep=".")
    },
    content = function(file){
      f = read.csv("test_result.csv")
      write.csv(f,file = file,row.names = F)
    }
  )
  
  # -------------this is for unit testing-----------------
  output$test <- renderPrint({
    counters$test$Domain
  })
  
  output$test2 <- renderPrint({
    as.integer(counters$correctAnswer == counters$selection)
  })
  # -------------------------------------------------
  
  # -------------this is for password-----------------
  observeEvent(input$loginButton,{
    if(input$nameInput == NAME && input$passwordInput == PASSWORD){
      shinyjs::hide("loginButton")
      shinyjs::show("scoretable")  
      shinyjs::show("download")
      shinyjs::hide("nameInput")
      shinyjs::hide("passwordInput")
      shinyjs::hide("scoreButton")
    }
  })
  # -------------------------------------------------
  
}