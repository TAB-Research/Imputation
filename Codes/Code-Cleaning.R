cleaning=function(string){
  # Change all the alphabet to lower case using 'tolower' function, if necessary
  # string=tolower(string)
  
  # If there is a specific punctuation or symbol that you would like to keep, through in between the brackets
  # For example, remove all punctuation other than $ ' . / -
  # I noticed, '.' was often used as decimal points in the context, so I decided to save it for now
  # I left the 'appostrophe' symbol, since it may be used to as a unit indicator('ft' or 'in') as well as a tool for abbreviation.
  
  #string=gsub("([$/])|[[:punct:]]", "\\1", string)
  
  string=gsub("[[:punct:]]", "\\1", string)
  string=gsub("[[:digit:]]", "\\1", string)
  string=gsub(" {2,}", " ", string)

  # Now, Lets get rid of the 'periods', i.e., '.' only when it is used after alphabet
  # Remove period after alphabets
  #string=gsub("([[:alpha:]])(\\.)", "\\1", string)
  # If there was a 'period' used before the number, then you may need to erase it was well
  #string=gsub("([[:digit:]])(\\.)(\\s)", "\\1 \\3", string)

  # Remove space between digits and alphabets
  #string=gsub("([[:digit:]])( {2,})([[:alpha:]])", "\\1\\3", string)
  #string=gsub("([[:digit:]])( {1,})([[:alpha:]])", "\\1\\3", string)
  # Remove all extra space
  string=gsub(" {2,}", " ", string)
  return(string)
}
