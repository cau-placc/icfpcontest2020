module AlienApi (apiKey, server, galaxyFile, statefulFile, statelessFile) where

apiKey,server :: String
apiKey = "e5a6755f75374b6fbb621ae3d46e6f36"
server = "https://icfpc2020-api.testkontur.ru"


galaxyFile, statelessFile, statefulFile :: FilePath
galaxyFile = "galaxy.txt"
statelessFile = "stateless.txt"
statefulFile = "stateful.txt"
