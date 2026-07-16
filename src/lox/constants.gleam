import gleam/dict
import lox/token

pub fn get_keyword_map() {
  dict.from_list([
    #("and", token.And),
    #("class", token.Class),
    #("else", token.Else),
    #("false", token.False),
    #("fun", token.Fun),
    #("for", token.For),
    #("if", token.If),
    #("or", token.Or),
    #("print", token.Print),
    #("return", token.Return),
    #("super", token.Super),
    #("this", token.This),
    #("true", token.True),
    #("nil", token.Nil),
    #("var", token.Var),
    #("while", token.While),
  ])
}
