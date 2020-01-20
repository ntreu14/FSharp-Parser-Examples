open Expecto
open FParsec
open Parsers.SimpleParsers
open Parsers.PhoneNumberParsers

let isSuccessful = function
  | Success _ -> true
  | Failure _ -> false

let isError parserResult =
  not <| isSuccessful parserResult

let equalsExpectedResult expected = function
  | Success (result, _, _) -> expected = result
  | _ -> false

let always a _ = a

let testCases message cases test =
  cases |> List.map (testCase message << (always test))

let tests =
  testList "FParsec specs" [
    testList "simple parsers" [

      testCase "parsing a string that is a valid float" <| fun _ ->
        Expect.isTrue 
          (isSuccessful <| run parseFloat "1.55") 
          "yields a Success"

      testCase "parsing a string that isn't a valid float" <| fun _ -> 
        Expect.isTrue
          (isError <| run parseFloat "foobar")
          "yields an Error"

      testCase "parsing a valid float between brackets" <| fun _ ->
        Expect.isTrue
          (isSuccessful <| run parseFloatBetweenBrackets "[42.42]")
          "yields a Success"

      testCase "parsing a string that isn't a float between brackets" <| fun _ ->
        Expect.isTrue
          (isError <| run parseFloatBetweenBrackets "[foobar]")
          "yields an Error"

      testCase "parsing strings with missing brackets" <| fun _ ->
        let shouldBeAllErrors = 
          [ "[123.43"; "54.5]"; "42.24"] |> List.forall (isError << run parseFloatBetweenBrackets)
        
        Expect.isTrue
          shouldBeAllErrors
          "returns Errors"

      testCase "parsing strings with brackets, but not at the start or end should error" <| fun _ ->
        let shouldBeAllErrors =
          [ "34[].234"; "[43p]d"; "3[34234]"] |> List.forall (isError << run parseFloatBetweenBrackets)

        Expect.isTrue
          shouldBeAllErrors
          "returns Errors"

      testList "foo bar parser" [

        testCase "parsing a string that is 'foo' or 'bar'" <| fun _ ->
          Expect.isTrue
            (["foo"; "bar"] |> List.forall (isSuccessful << run parseFooOrBar))
            "returns Successes"
      
        testCase "parsing any other string string that is not 'foo' or 'bar'" <| fun _ ->
          Expect.isTrue
            (["abc"; "supercalifragilisticexpialidocious"; "hello world"; "$&*@#_)*(%"] |> List.forall (isError << run parseFooOrBar))
            "returns Errors"
      ]
    ]   
    testList "phone number parsers" [
      testList "parsing the area code with parentheses" [
        
        testCase "should parse successfully" <| fun _ ->
          let sampleAreaCode = "(412)"

          Expect.isTrue
            (isSuccessful <| run parensPattern sampleAreaCode)
            "when given a 3 numbers in between parentheses"

        testCase "should fail" <| fun _ ->
          let badAreaCodes = [
            "(12)"
            "(124"
            "123)"
            "1(23)"
            ""
          ]
          
          Expect.isTrue
            (badAreaCodes |> List.forall (isError << run parensPattern))
            "when given a poorly formatted area code"
      ]

      testList "parsing the country code" [
        testCase "should prase successfully" <| fun _ ->
          let validCountryCodes = ["+1"; "+54"; "+313"]
          
          Expect.isTrue
            (validCountryCodes |> List.forall (isSuccessful << run countryCodePattern))
            "when given codes with a plus an an integer after it"
      ]
    ]
  ]

[<EntryPoint>]
let main _ =
  tests |> runTests { defaultConfig with ``parallel`` = true; verbosity=Logging.LogLevel.Verbose } 