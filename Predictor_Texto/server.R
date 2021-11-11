# Read Data
quadgram = readRDS("quadgram.RData")
trigram = readRDS("trigram.RData")
bigram = readRDS("bigram.RData")
mesg = ""

# Cleaning data before predict the next word

Predict = function(x) {
    xclean = removeNumbers(removePunctuation(tolower(x)))
    xs = strsplit(xclean, " ")[[1]]

# Back Off Algorithm
# Predict the next term of the user input sentence
# 1. For prediction of the next word, Quadgram is first used (first 
  # three words of Quadgram are the last three words of the user provided 
  # sentence).
# 2. If no Quadgram is found, back off to Trigram (first two words of 
  # Trigram are the last two words of the sentence).
# 3. If no Trigram is found, back off to Bigram (first word of Bigram is 
  # the last word of the sentence)
# 4. If no Bigram is found, back off to the most common word with 
  # highest frequency 'the' is returned.

    if (length(xs)>= 3) {
        xs = tail(xs,3)
        if (identical ( character( 0), 
                        head( quadgram[ quadgram$unigram == xs[1] & 
                                        quadgram$bigram == xs[2] & 
                                        quadgram$trigram == xs[3], 4],1))){
            Predict(paste(xs[2],xs[3],sep=" "))
        }
        else {mesg = "Fourth word to predict"; 
        head(quadgram[quadgram$unigram == xs[1] & 
                          quadgram$bigram == xs[2] & 
                          quadgram$trigram == xs[3], 4],1)}
    }
    else if ( length(xs) == 2){
        xs = tail(xs,2)
        if ( identical( character(0), 
                        head( trigram[ trigram$unigram == xs[1] &
                                       trigram$bigram == xs[2], 3],1))) {
            Predict(xs[2])
        }
        else {mesg = "Third word to predict"; 
        head(trigram[trigram$unigram == xs[1] & 
                         trigram$bigram == xs[2], 3],1)}
    }
    else if (length(xs) == 1){
        xs <- tail(xs,1)
        if ( identical( character(0), 
                        head( bigram[bigram$unigram == xs[1], 2],1))) 
        {mesg<-"No match found. Most common word 'the' is returned."; 
            head("the",1)}
        else {mesg = "Second word to predict."; 
        head(bigram[bigram$unigram == xs[1],2],1)}
    }
}

shinyServer(function(input, output) {
    
    prediccion = eventReactive(input$click, {
      result = Predict(input$inputString)
      output$text2 <- renderText({mesg})
      result
    })
    
    output$prediction = renderPrint({ prediccion() })

    
    grafico = eventReactive(input$click, {
      n = wc(input$inputString) # count word
      one = str_to_lower( unlist(strsplit(input$inputString, " ")))[1]
      two = str_to_lower( unlist(strsplit(input$inputString, " ")))[2]
      three = str_to_lower( unlist(strsplit(input$inputString, " ")))[3]
      four = str_to_lower( unlist(strsplit(input$inputString, " ")))[4]
      
      if (n==1) {
        
        p_prob = bigram[ bigram$unigram == str_to_lower(input$inputString),]
        p_prob$prob =  round(p_prob$freq/colSums(matrix(p_prob$freq)),3)
        p_prob = arrange(p_prob, desc(freq))
        p_prob$bigram = factor(p_prob$bigram, levels = p_prob$bigram)
        
        p= ggplot(p_prob[1:10,] , aes(x=prob, y=freq, fill=bigram )) +
          geom_bar(stat="identity" ) + theme_minimal() +
          labs(title="Probability next word",
               x="Probability", y = "Frecuency", fill="Word from")
        return(p)  }
      
      if(n==2){
        
        p_prob = trigram[trigram$bigram ==  two,]
        p_prob = p_prob %>%
          filter(unigram==one & bigram==two)
        p_prob$prob =  round(p_prob$freq /colSums(matrix(p_prob$freq)),3)
        p_prob = arrange(p_prob, desc(freq))[1:20,]
        p_prob = p_prob %>%
          group_by(trigram) %>%
          summarise(prob_2=sum(prob),freq_2=sum(freq)) %>%
          arrange(desc(freq_2) )
        p_prob$trigram=factor(p_prob$trigram, levels = p_prob$trigram)
        
        p= ggplot(p_prob[1:10,] , aes(x=prob_2, y=freq_2, fill=trigram ))+
          geom_bar(stat="identity" ) + theme_minimal() +
          labs(title="Probability next word",
               x="Probability", y = "Frecuency", fill="Word from
     highest to lowest")
        return(p) }
      if(n==3){
        
        p_prob = quadgram[quadgram$trigram ==  three,]
        p_prob = p_prob %>%
          filter(unigram==one & bigram==two & trigram==three)
        p_prob$prob =  round(p_prob$freq /colSums(matrix(p_prob$freq)),3)
        p_prob = arrange(p_prob, desc(freq))[1:20,]
        p_prob = p_prob %>%
          group_by(quadgram) %>%
          summarise(prob_2=sum(prob),freq_2=sum(freq)) %>%
          arrange(desc(freq_2) )
        p_prob$quadgram<-factor(p_prob$quadgram, levels = p_prob$quadgram)
        
        p= ggplot(p_prob[1:10,] , aes(x=prob_2, y=freq_2, fill=quadgram ))+
          geom_bar(stat="identity" ) + theme_minimal() +
          labs(title="Probability next word",
               x="Probability", y = "Frecuency", fill="Word from
     highest to lowest")
        return(p) }
      if (n==4) {
        p=ggplot(mapping = aes(1,1, fill=Predict(x)))+ geom_bar(stat="identity" ) + theme_minimal() +
          labs(title="Probability next word",
               x="Probability", y = "Frecuency", fill="Word from highest to lowest")+
          lims(x=c(0,2))
        return(p)
      }
      if (n>4){mesg<-"No match found. Most common word 'the' is returned."}
      
      })
    
    output$plot = renderPlotly({ grafico() })
})