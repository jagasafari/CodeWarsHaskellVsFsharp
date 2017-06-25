import System.Process

data CurlArgs = 
    | UrlOnly String 
    | WithArgs [String] String 

runCurlCmd a = createProcess . shell . unwords $ "curl":a

curl (UrlOnly url) = runCurlCmd [url]
curl (WithArgs args url) = runCurlCmd $ args ++ [url]

getCommand = curl $ UrlOnly "http://localhost:3000"
postNameParam = curl $ WithArgs ["--data 'name=mika'"] "http://localhost:3000/simulateFailure"
postJsonData = 
    curl $ WithArgs 
            ["--header 'Content-Type: application/json'"
            , "--data '{name=mika}'"] 
            "http://localhost:3000/deserializingJsonData "
