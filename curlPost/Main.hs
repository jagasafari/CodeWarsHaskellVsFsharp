import System.Process

getCommand = createProcess $ shell "curl http://localhost:3000"
