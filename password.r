my_pass_word<-function(pass_length){

  pass_word<-vector("character",length = pass_length)


  char<-c(
    LETTERS,
    letters,
    as.character(0:9),
    strsplit("`~!@#$%^&*()_+-={}[]\\|:;\"'<>,.?/", "")[[1]] 
  )


  for(i in seq_len(length(pass_word))){

   pass_word[i]<-sample(char,1,replace = TRUE,prob = c(rep(1,length(char))))

  }


  return (paste(pass_word,collapse = ""))
}

checknum<-function(text){

   while (TRUE) {
        num <- readline(prompt = text)
        if (tolower(num) == "exit") {
            return(NA)  
        }
        check_num <- as.integer(num)
        if (!is.na(check_num) && check_num > 0) {
            return (check_num)
        } else {
            print("输入无效，请输入一个有效的正整数或输入 'exit' 退出")
        }
    }


}
evaluate_password_strength<-function(password){
 
  score<-0


  length_pass<-nchar(password)


 has_low<-any(grepl("[a-z]",password))
 has_upp<-any(grepl("[A-Z]",password))
 has_num<-any(grepl("[0-9]",password))
 has_char<-any(grepl("[`~!@#$%^&*()_+-={}\\[\\]|:;\"'<>,.?/]",password))


 diversity_score<-sum(c(has_char,has_low,has_num,has_upp))*5


 repeat_penalty<-length(password)-length(unique(strsplit(password,"")[[1]]))


score <- length_pass + diversity_score - repeat_penalty * 2


final_score<-min(max(score,0),100)


if(final_score>=80){
    return(list(score=final_score,rating="VeryStrong"))
}else if (final_score>=60) {
   return(list(score=final_score,rating="Strong"))
}else if (final_score>=40) {
    return(list(score=final_score,rating="Moderate"))
}else{
    return(list(score=final_score,rating="Weak"))
}

}



write_text <- function() {
    while (TRUE) {
        x <- checknum("请输入密码长度或输入 'exit' 退出:")
        if (is.na(x)) {  
            print("退出程序...")
            break
        }
        y <- my_pass_word(x)
        isstrong <- evaluate_password_strength(y)
        cat(sprintf("%s: Score: %d, Rating: %s\n", y, isstrong$score, isstrong$rating))

        option <- readline(prompt = "请问要将其写入txt文件吗(y/n)?输入 'exit' 退出程序:\n")
        if (tolower(option) == 'y') {
            path <- readline(prompt = "请填写文件路径:\n")
            if (nchar(path) > 0) {
                time = Sys.time()
                new_time = format(time, "%Y-%m-%d %H:%M:%S")
                cat(sprintf("[%s]: %s\n", new_time, y), file = path, append = TRUE)
            } else {
                print("输入无效，请重试")
            }
        } else if (tolower(option) == "exit") {
            print("退出程序...")
            break
        }
    }
}





main<-function(){


  write_text()

}


main()