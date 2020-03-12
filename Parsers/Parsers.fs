namespace Parsers

open FParsec
open System

module SimpleParsers =

  let parseFloat = pfloat

  let parseFloatBetweenBrackets : Parser<float, unit> = 
    pchar '[' >>. pfloat .>> pchar ']'
  
  let parseFooOrBar : Parser<string, unit> =
    pstring "foo" <|> pstring "bar"

  let parseDigit = digit

module PhoneNumberParsers =

  type PhoneNumber = {
    CountryCode: int option
    AreaCode: int
    Prefix: int
    LineNumber: int
  }

  let nDigits n = 
    anyOf "1234567890" |> parray n |>> String.Concat

  let parseDigitsAsInt n = nDigits n |>> int

  let parseCountryCode numCountryCodeDigits : Parser<int option, string> =
    ((optional <| pchar '+') >>. parseDigitsAsInt numCountryCodeDigits) |> opt
 
  let parseAreaCode: Parser<int, string> =      
    pchar '('
      >>. parseDigitsAsInt 3
      .>> pchar ')'

  let parsePrefix = parseDigitsAsInt 3
  let parseLineNumber = parseDigitsAsInt 4

  let dashOrWhiteSpace =
    optional (skipChar '-' <|> spaces1)

  
  let parsePhoneNumber numCountryCodeDigits = parse {
    let! perhapsCountryCode = parseCountryCode numCountryCodeDigits 
    let! _ = dashOrWhiteSpace
    let! areaCode = parseAreaCode
    let! _ = dashOrWhiteSpace
    let! prefix = parsePrefix
    let! _ = dashOrWhiteSpace
    let! lineNumber = parseLineNumber
    
    return {
        CountryCode=perhapsCountryCode
        AreaCode=areaCode
        Prefix=prefix
        LineNumber=lineNumber
      }
  }
