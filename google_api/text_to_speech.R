library(googleLanguageR)
library(tidyverse)

gl_auth("/Users/dzickuhr/R/Project_Test/googleapi/credential.json")

text <- "Hey dude, what the hell are you doing? It seems you're lost there. Do you need help?"
## translate British into Danish
gl_translate(text,source = "en",target = "pt-br")$translatedText


gl_talk(text, 
        gender = "FEMALE",
        
        output = "output.wav")
