library(shiny)
library(stringi)
library(data.table)
library(ggplot2)

#-----------------------------Loading Rds files--------------------------------------

unigrams = readRDS("unigrams.Rds") 
bigrams = readRDS("bigrams.Rds")
trigrams = readRDS("trigrams.Rds")
fourgrams = readRDS("fourgrams.Rds")
codes = readRDS("codes.Rds")

#--------------------------------Creating functions------------------------------------

word_code = function(w){
        l = list(w)
        codes[l]$code
}

code_word = function(a){
        where = sapply(a, function(x)which(codes$code == x))
        codes[where]$x
}


 #----------------------Creating necessary replacaments---------------------------------- 

shinyServer(function(input, output) {
        
        initialstage = function(expr){
                expr = tolower(expr)
                expr = paste(" BS ", expr)
                wordsinexpr = unlist(stri_extract_all_words(expr))
                rYEAR = "^[12]{1}[0-9]{3}$"
                wordsinexpr = stri_replace_all(wordsinexpr, replacement = "YEAR", regex = rYEAR)
                rNUM = "^[[:digit:]]+(,[[:digit:]]*)*\\.*[[:digit:]]*$"
                wordsinexpr = stri_replace_all(wordsinexpr, replacement = "NUM", regex = rNUM)
                pRegex = "[\\[\\]\\$\\*\\+\\.\\?\\^\\{\\}\\|\\(\\)\\#%&~_/<=>✬!,:;❵@]"
                wordsinexpr = stri_replace_all(wordsinexpr, replacement = "", regex = pRegex)
                wordsinexpr = stri_replace_all(wordsinexpr, replacement = "'", fixed = "’")
                nonEngChars = "[^A-Za-z'-[:space:]]"
                wordsinexpr = stri_replace_all(wordsinexpr, replacement = "", regex = nonEngChars)
                emptyWords = (wordsinexpr == "")|(wordsinexpr=="'")
                wordsinexpr = wordsinexpr[!emptyWords]
                return(wordsinexpr)
        } 
        
        #------------------------------N-Grams predictions--------------------------- 
        
        prediction1gram = function(prevWords=numeric(0), k=3){
                
                wordsn = numeric(0)
                probsn=numeric(0)
                
                selected = unigrams[!(x %in% prevWords)][order(pX, decreasing = TRUE)]
                k2 = min(k, nrow(selected))
                wordsn = selected$x[1:k2]
                probsn = selected$pX[1:k2]
                
                prediction = list(words = wordsn, probs = probsn)
                return(prediction)  
                
        }   
        
        
        prediction2gram = function(x0=numeric(0), prevWords=numeric(0), k=3){
                
                wordsn = numeric(0)
                probsn=numeric(0)
                
                (bigramsn = any(!is.na(bigrams[.(x0)][!(y %in% prevWords)]$pXY)) )
                
                if(bigramsn){
                        
                        selected = bigrams[.(x0)][!(y %in% prevWords)][order(pXY, decreasing = T)]
                        k2 = min(k, nrow(selected))
                        wordsn = selected$y[1:k2]
                        probsn = selected$pXY[1:k2]
                        
                        
                        
                        alpha = selected$alpha[1]
                        
                        sUnigrams = prediction1gram(c(prevWords, wordsn), k)
                        wordsn = c(wordsn, sUnigrams$words)
                        probsn = c(probsn, alpha * sUnigrams$probs)   
                        
                } else {
                        
                         
                        
                        sUnigrams = prediction1gram(prevWords, k)
                        wordsn = sUnigrams$words
                        probsn = sUnigrams$probs  
                        
                        
                }
                
                prediction = list(words = wordsn, probs = probsn)
                return(prediction)  
                
        }  
        
        prediction3gram = function(xy=numeric(0), prevWords=numeric(0), k=3){
                
                x0 = xy[1]
                y0 = xy[2]
                
                wordsn = numeric(0)
                probsn=numeric(0)
                
                (trigramsn = any(!is.na(trigrams[.(x0, y0)][!(z %in% prevWords)]$pXYZ)) )
                
                if(trigramsn){
                        
                        selected = trigrams[.(x0, y0)][!(z %in% prevWords)][order(pXYZ, decreasing = T)]
                        k2 = min(k, nrow(selected))
                        wordsn = selected$z[1:k2]
                        probsn = selected$pXYZ[1:k2]
                        
                        
                        
                        alpha = selected$alpha[1]
                        
                        sBigrams = prediction2gram(x0 = y0, c(prevWords, wordsn), k)
                        wordsn = c(wordsn, sBigrams$words)
                        probsn = c(probsn, alpha * sBigrams$probs)    
                        
                } else {
                        
                        
                        
                        sBigrams = prediction2gram(x0 = y0, prevWords, k)
                        wordsn = sBigrams$words
                        probsn = sBigrams$probs  
                        
                }
                
                prediction = list(words = wordsn, probs = probsn)
                return(prediction)  
                
        }  
        
        
        prediction4gram = function(xyz=numeric(0), prevWords=numeric(0), k=3){
                
                x0 = xyz[1]
                y0 = xyz[2]
                z0 = xyz[3]
                
                wordsn = numeric(0)
                probsn=numeric(0)
                
                (fourgramsn = any(!is.na(fourgrams[.(x0, y0, z0)][!(t %in% prevWords)]$pXYZT)) )
                
                if(fourgramsn){
                        
                        selected = fourgrams[.(x0, y0, z0)][!(t %in% prevWords)][order(pXYZT, decreasing = T)]
                        k2 = min(k, nrow(selected))
                        wordsn = selected$t[1:k2]
                        probsn = selected$pXYZT[1:k2]
                        
                    
                        
                        alpha = selected$alpha[1]
                        
                        sTrigrams = prediction3gram(xy= c(y0,z0), c(prevWords, wordsn), k)
                        wordsn = c(wordsn, sTrigrams$words)
                        probsn = c(probsn, alpha * sTrigrams$probs)  
                        
                } else {
                        
                       
                        
                        sTrigrams = prediction3gram(xy= c(y0,z0), prevWords, k)
                        wordsn = sTrigrams$words
                        probsn = sTrigrams$probs  
                        
                }
                
                prediction = list(words = wordsn, probs = probsn)
                return(prediction)  
                
        }   
        
        
     
       
        
        predictedWord = function(inputSentence=character(0), k=10){
                
                # Preprocess the words
                
                inputWords = initialstage(inputSentence)
                
                # Convert them to numeric codes
                
                inputWords = word_code(inputWords)
                
                
                # First check the number of input words
                n  = length(inputWords)
                
                prevWords = numeric(0)
                prevProbs = numeric(0)
                prediction = list()
                
                #------------Number of words defines the RDS file that will be used to predict----
                
             
                if(n >= 3){
                        
                        xyz = inputWords[(n-2):n]
                        prediction = prediction4gram(xyz, prevWords, k)
                        
                } else if(n == 2){
                        
                        xy = inputWords
                        xy = inputWords[(n-1):n]
                        prediction = prediction3gram(xy, prevWords, k)
                        
                }  else if(n == 1){
                        
                        x = inputWords
                        prediction = prediction2gram(x0=x, prevWords, k)
                        
                }  else if(n == 0){
                        
                        prediction = prediction1gram(prevWords, k)
                        
                } 
                
                ### Now we have a list of predicted word codes and probabilities
                
                
              
                
                predictedWords = list(words = code_word(prediction$words), 
                                      probs = prediction$probs)
                
                
                
                return(predictedWords)
                
        }   
        
        #-----------------------Final Output--------------------------------
        
        
        output$predictedWordMain <- renderText({
                
                predictedWord(input$userSentence, k = input$numPredicted)$words[1]
                
        })
        
        prediction = reactive({
                k = input$numPredicted
                predList = predictedWord(input$userSentence, k)
                aa<- data.table(words = predList$words[1:k], probs = predList$probs[1:k])
                
        })
        
       
        
        output$predictionTable <- renderPlot({
                pred <- prediction()
                barchart <- ggplot(pred, aes(x = reorder(words, probs), y=probs)) + geom_bar(fill="blue",stat="identity") +
                        theme_bw() + coord_flip() +
                        labs(title="Frequent words ordered by probability", x="Words", y="Probs")
                print(barchart)
                return(barchart)
                
        })
        

        
  
        
})