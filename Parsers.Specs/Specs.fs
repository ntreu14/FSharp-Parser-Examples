open Expecto
open FParsec
open Parsers.SimpleParsers
open Parsers.PhoneNumberParsers

let isSuccessful = function
  | Success _ -> true
  | Failure _ -> false

let isError = not << isSuccessful

let equalsExpectedResult expected = function
  | Success (result, _, _) -> expected = result
  | _                      -> false

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

      testCase "parsing a string that is a single digit" <| fun _ ->
        let digit = "9"

        Expect.isTrue
          (isSuccessful <| run parseDigit digit)
          "should return successfully"

      testCase "parsing a stiring that is a char and is not a digit" <| fun _ ->
        let invalidDigit = "s"
        
        Expect.isTrue
         (isError <| run parseDigit invalidDigit)
         "does not parse successfully"

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
            (isSuccessful <| runParserOnString parseAreaCode "" "" sampleAreaCode)
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
            (badAreaCodes |> List.forall (isError << runParserOnString parseAreaCode "" ""))
            "when given a poorly formatted area code"
      ]

      testList "parsing the country code" [
        testCase "should parse successfully" <| fun _ ->
          let parsedCountryCodes = 
            [
              "+1", 1
              "+54", 2
              "+313", 3
              "540", 3
              "5", 1
            ]
            |> List.map (fun (countryCode, digits) -> runParserOnString (parseCountryCode digits) "" "" countryCode)
          
          Expect.isTrue
            (parsedCountryCodes |> List.forall isSuccessful)
            "when given codes with an optional plus and a given specified number of digits after it"

        testList "should not parse successfully" [
          testCase "when given the wrong number of digits" <| fun _ ->
            let badCountryCodes = 
              [
               "+1", 2
               "23", 5
               "+540", 10
              ]
              |> List.map (fun (countryCode, digits) -> runParserOnString (parseCountryCode digits) "" "" countryCode)

            Expect.isTrue
              (badCountryCodes |> List.forall isError)
              "should fail to parse"

          testCase "when given the '+' in the wrong position, but correct number of digits" <| fun _ ->
            let badCountryCodes = 
              [
               "2+3", 2
               "54+0", 3
              ]
              |> List.map (fun (countryCode, digits) -> runParserOnString (parseCountryCode digits) "" "" countryCode)

            Expect.isTrue
              (badCountryCodes |> List.forall isError)
              "should fail to parse"
        ]

        testCase "parsing a full phone number" <| fun _ ->
          let rawPhoneNumber = "+1 (678) 234-5435"
          let result = runParserOnString (parsePhoneNumber 1) "" "phone number parser" rawPhoneNumber

          let expectedPhoneNumber = {
            CountryCode=Some 1
            AreaCode=678
            Prefix=234
            LineNumber=5435
          }

          Expect.isTrue (isSuccessful result) "is successful"
          Expect.isTrue (equalsExpectedResult expectedPhoneNumber result) "parses to the expected phone number type"
      ]
    ]
  ]

[<EntryPoint>]
let main _ =
  tests |> runTests { defaultConfig with ``parallel``=true; verbosity=Logging.LogLevel.Verbose } 