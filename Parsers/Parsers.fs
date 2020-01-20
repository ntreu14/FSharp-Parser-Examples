namespace Parsers

open FParsec
open System

module SimpleParsers =

  let parseFloat = pfloat

  let parseFloatBetweenBrackets : Parser<float, unit>= 
    pchar '[' >>. pfloat .>> pchar ']'
  
  let parseFooOrBar : Parser<string, unit> =
    pstring "foo" <|> pstring "bar"

module PhoneNumberParsers =

  type PhoneNumber = 
    {
      CountryCode: int option
      AreaCode: int
      Prefix: int
      LineNumber: int
    }

  let private nDigits n = 
    parray n digit |>> (fun c -> String.Join("", c))

  let countryCodePattern : Parser<unit, unit> =
    optional (pchar '+' >>. puint16)
 
  let parensPattern : Parser<string, unit> =
    pchar '(' >>. nDigits 3 .>> pchar ')'
    
  let parsePhoneNumber (phoneNumber: string) = ()
    