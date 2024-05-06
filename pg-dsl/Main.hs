{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import           AST                       (AST, FParserError (..), Model (..),
                                            PG (..), ParserError, ProgramGraph,
                                            emptyEnv)
import           Control.Monad             (when)
import qualified Data.ByteString.Lazy      as LBS
import           Data.Char                 (toLower)
import           Data.List                 (isSuffixOf)
import           Distribution.Simple.Utils (getDirectoryContentsRecursive)
import           Distribution.Utils.Json   (renderJson)
import           GHC.Base                  (returnIO)
import           Jsonable                  (Jsonable (toJson))
import           Parser                    (parse, parseMain)
import           RangeCheck                (checkRanges)
import           System.Directory          (createDirectoryIfMissing, doesFileExist)
import           System.Environment        (getArgs, withArgs)
import           System.Exit               (ExitCode (ExitFailure), exitSuccess,
                                            exitWith)
import           System.FilePath           (takeDirectory, (</>))
import           System.Process            (createProcess, proc, waitForProcess)
import           Tokenizer                 (tokenize)
import           TypeCheck                 (checkTypes)
import           System.Console.CmdArgs
 
data OutputType = PNG | PUML | JSON | YAML | NuSMV deriving (Show, Data)

data PgDSL = Test  {pgDirectory :: FilePath } |
              Simulate {pgDirectory :: FilePath, steps :: Int, withFaults :: Bool} |
              DCCA {pgDirectory :: FilePath } |
              Output {pgDirectory :: FilePath, outputType :: OutputType}
              deriving (Show, Data, Typeable)

-- weird bug requires different typ for each instance of pgDirectory.. 
-- https://github.com/ndmitchell/cmdargs/issues/67
test = Test {
  pgDirectory = def &= typ "Path" &= opt "." &= args
}

dcca = DCCA {
  pgDirectory = def &= typ "Path " &= opt "." &= args
}

simulate = Simulate {
  steps = def &= typ "(Int > 0)" &= opt "10" &= help "the number of steps to simulate",
  withFaults = def &= help "Whether to include faults in the simulation",
  pgDirectory = def &= typ " Path" &= opt "." &= args
}

output = Output {
  outputType = PNG &= help "Output format type" &= typ "[png,puml,json,yaml,nusmv]",
  pgDirectory = def &= typ " Path " &= opt "." &= args
}

hasMain :: FilePath -> IO (Bool)
hasMain basepath = do
  doesFileExist $ basepath </> "main.pg"

cmdHandler :: PgDSL -> String -> IO ()
cmdHandler (Test pgDir) jsonFile = do
  putStrLn $ "Testing program graphs from folder " ++ pgDir
  let cmd = ["test"]
  let jsonArgs = ["--json-file", jsonFile]
  (_, _, _, processHandle) <- createProcess (proc "pg-verify" $ cmd ++ jsonArgs)
  _ <- waitForProcess processHandle
  putStrLn ""
cmdHandler (Simulate pgDir steps withFaults) jsonFile = do
  let s = if (steps > 0) then steps else 1
  putStrLn $ "Simulating program graphs from folder " ++ pgDir ++ " for " ++ (show steps) ++ " steps."
  putStrLn $ "Faults are " ++ (if withFaults then "included." else "excluded.")
  let cmd = ["simulate"]
  let jsonArgs = ["--json-file", jsonFile]
  let stepsArg = ["--steps", show s]
  let faultsArg = if withFaults then ["--with-faults"] else []
  (_, _, _, processHandle) <- createProcess (proc "pg-verify" $ cmd ++ jsonArgs ++ stepsArg ++ faultsArg)
  _ <- waitForProcess processHandle
  putStrLn ""
cmdHandler (DCCA pgDir) jsonFile = do
  putStrLn $ "Performing DCCA for program graphs from folder " ++ pgDir
  let cmd = ["dcca"]
  let jsonArgs = ["--json-file", jsonFile]
  (_, _, _, processHandle) <- createProcess (proc "pg-verify" $ cmd ++ jsonArgs)
  _ <- waitForProcess processHandle
  putStrLn ""
cmdHandler (Output pgDir typ) jsonFile = do
  putStrLn $ "Showing program graphs from folder " ++ pgDir ++ " as " ++ (show typ)
  let cmd = ["show", map toLower (show typ)]
  let jsonArgs = ["--json-file", jsonFile]
  (_, _, _, processHandle) <- createProcess (proc "pg-verify" $ cmd ++ jsonArgs)
  _ <- waitForProcess processHandle
  putStrLn ""

-- returns the path to the created json file
pgModelToJson :: String -> IO (String)
pgModelToJson basepath = do
  allFiles <- (getDirectoryContentsRecursive basepath)
  let pgFiles = filter isGraphFile allFiles
  contents <- mapM (readFile . (basepath </>)) pgFiles
  let inspected = inspect $ zip pgFiles $ map (parse . tokenize) contents
  let mainFile = basepath </> "main.pg"
  mainContent <- readFile mainFile
  let modelRaw = inspectMain mainFile $ (parseMain . tokenize) mainContent
  let model = mergeGraphs modelRaw inspected
  let checkedModel =
        case model of
          Right m ->
            case checkTypes m of
              Left s -> Left s
              Right env ->
                case checkRanges env m of
                  Nothing  -> Right m {environ = env}
                  Just err -> Left err
          err -> err 
  when (isError checkedModel) $ putStrLn ("Error in model: " ++ (getErrorMsg checkedModel)) >> exitWith (ExitFailure 1)
  let jsonOut = "./out/pg-dsl/" ++ map toLower (getModelName model) ++ ".json"
  createDirectoryIfMissing True $ takeDirectory jsonOut
  LBS.writeFile jsonOut $ renderJson $ toJson checkedModel
  return jsonOut

main :: IO ()
main = do
  raw_args <- getArgs
  carg <- (if null raw_args then withArgs ["--help"] else id) $ cmdArgs (modes [test, output, dcca, simulate])
  print carg
  let basepath = pgDirectory carg
  main_exists <- hasMain basepath
  when (not main_exists) $ putStrLn "Error: Given directory does not contain a main.pg file" >> exitWith (ExitFailure 1)
  jsonFile <- pgModelToJson basepath
  cmdHandler carg jsonFile
  exitSuccess

parseArgs :: [String] -> (FilePath, [String])
parseArgs [] = (".", [])
parseArgs [x]
  | x `elem` ["-t", "--test"] = (".", ["test"])
  | x `elem` ["-s", "--simulate"] = (".", ["simulate"])
  | x `elem` ["-d", "--dcca"] = (".", ["dcca"])
  | notDash x = (x, [])
parseArgs [x, y]
  | x `elem` ["-t", "--test"] && notDash y = (y, ["test"])
  | x `elem` ["-s", "--simulate"] && notDash y = (y, ["simulate"])
  | x `elem` ["-d", "--dcca"] && notDash y = (y, ["dcca"])
  | x `elem` ["-o", "--show"] && y `elem` ["puml", "json", "yaml"] =
    (".", ["show", y])
  | x `elem` ["-o", "--show"] && y == "png" =
    (".", ["show", y, "--hide-precons"])
parseArgs [x, y, z]
  | x `elem` ["-o", "--show"] && y `elem` ["puml", "json", "yaml"] && notDash z =
    (z, ["show", y])
  | x `elem` ["-o", "--show"] && y == "png" = (z, ["show", y, "--hide-precons"])
parseArgs x = ("", ["err"])

notDash :: String -> Bool
notDash [] = False
notDash s  = head s /= '-'

isGraphFile :: String -> Bool
isGraphFile s = ".pg" `isSuffixOf` s && not ("main.pg" `isSuffixOf` s)

readGraph :: FilePath -> IO (String, String)
readGraph fp = do
  content <- readFile fp
  returnIO (content, fp)

inspect :: [(FilePath, ProgramGraph)] -> Either FParserError [PG]
inspect = mg []
  where
    mg acc []                = Right acc
    mg acc ((fp, Left e):t)  = Left $ FPError fp e
    mg acc ((_, Right pg):t) = mg (pg : acc) t

inspectMain :: FilePath -> Either ParserError Model -> Either FParserError Model
inspectMain fp (Left err) = Left $ FPError fp err
inspectMain _ (Right m)   = Right m

mergeGraphs :: Either FParserError Model -> Either FParserError [PG] -> AST
mergeGraphs (Left e) _ = Left $ show e
mergeGraphs _ (Left e) = Left $ show e
mergeGraphs (Right m) (Right g) =
  Right $
  Model
    { modelName = modelName m
    , graphs = graphs m ++ g
    , hazards = hazards m
    , specs = specs m
    , environ = emptyEnv
    }

getModelName :: AST -> String
getModelName (Right m) = modelName m

isError :: AST -> Bool
isError (Left _) = True
isError _        = False

getErrorMsg :: AST -> String
getErrorMsg (Left s) = s
