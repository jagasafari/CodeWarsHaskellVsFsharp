import System.Process

data CurlArgs = 
    UrlOnly String 
    | WithHeader String String 
    | WithData String String 
    | WithDataHeader String String

runCurlCmd a = createProcess . shell . unwords $ "curl":a

curl (UrlOnly args) = runCurlCmd [args]

getCommand = curl $ UrlOnly "http://localhost:3000"
postNameParam = curl $ UrlOnly "--data 'name=mika' http://localhost:3000/simulateFailure"
postJsonData = curl $ UrlOnly "--header 'Content-Type: application/json' --data '{name=mika}' http://localhost:3000/deserializingJsonData "
