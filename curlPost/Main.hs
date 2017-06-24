import System.Process

curl = createProcess . shell
getCommand = curl "curl http://localhost:3000"
postNameParam = curl "curl --data 'name=mika' http://localhost:3000/simulateFailure"
postJsonData = curl "curl --header 'Content-Type: application/json' --data '{name=mika}' http://localhost:3000/deserializingJsonData "
