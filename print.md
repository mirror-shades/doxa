method print(to_print :: string) returns nothing | PrintError {
// find each unescaped bracket
// evaluate the expression
// convert expression to string
// concat with the string together
}

@print("Hello World") // takes a string, prints it
@print("The player has {hp} health left") // automatically formats values
@print("The length of {word} is {@length(word) - offset}") // allows for complex expressions
@print("Print can use \{ \} symbols to format values") // allows for escapes
