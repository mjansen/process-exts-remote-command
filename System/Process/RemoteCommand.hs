module System.Process.RemoteCommand
    where

import Control.Applicative

import System.Environment    
import System.FilePath
import System.Directory    

import System.Process hiding (runCommand)
import System.Process.Exts (runCommandCleanly, runCommand)

import qualified Data.ByteString.Char8 as BC

type Host         = String
type User         = String
type IdentityFile = String
type Command      = String

data Node = Node Host (Maybe User) (Maybe IdentityFile)
          deriving (Eq, Ord, Show)

data Path = Path [Node]
          deriving (Eq, Ord, Show)

rrc :: Host -> User -> Command -> IO (Maybe BC.ByteString)
rrc host user cmd = runCommandCleanly "ssh" [ "-axTl", user, host ] (BC.pack cmd)

rrc' :: Host -> User -> Command -> IO (Int, BC.ByteString, BC.ByteString)
rrc' host user cmd = runCommand "ssh" [ "-axTl", user, host ] (BC.pack cmd)

rrcp :: Path -> Command -> IO (Int, BC.ByteString, BC.ByteString)
rrcp (Path []) = runCommand "bash" []                     . BC.pack
rrcp (Path p)  = runCommand "ssh" (tail . followPath $ p) . BC.pack

followPath :: [Node] -> [String]
followPath [] = []
followPath (Node host user idf:rest) = concat [ [ "ssh", "-xT" ]
                                              , (maybe [] (("-l":) . return) user)
                                              , (maybe [] (("-i":) . return) idf)
                                              , [ host ]
                                              , followPath rest
                                              ]

followPath' (Path p) = followPath p

-- start again:

data SSHIdentityHash = SIH
    { sih_type    :: String
    , sih_hash    :: String
    , sih_comment :: String
    } deriving (Eq, Ord, Show, Read)

data SSHIdentity = SI
    { si_hash :: SSHIdentityHash
    , si_path :: FilePath
    } deriving (Eq, Ord, Show, Read)

-- a node in the graph of all accounts in a distributed system of
-- machines consists of a hostname, user (login) name, a list of
-- authorized keys for reaching the node, and a set if ssh keys for
-- transferring to neighbouring nodes.

data MachineLogin = ML
    { ml_host       :: Host
    , ml_user       :: User
    , ml_identities :: [SSHIdentity]
    , ml_authorized :: [SSHIdentityHash]
    } deriving (Eq, Ord, Show, Read)

-- data SSHEdge = SE MachineLogin MachineLogin
--              deriving (Eq, Ord, Show, Read)

-- data SSHPath = SP [SSHEdge]
--              deriving (Eq, Ord, Show, Read)

readPublicFile :: FilePath -> IO [SSHIdentityHash]
readPublicFile fName = map hashParser . lines <$> readFile fName

-- keys accepted for passwordless login (default file location):

readAuthorizedKeyFile :: IO [SSHIdentityHash]
readAuthorizedKeyFile = do
  homeDir <- getEnv "HOME"
  let fName = homeDir </> ".ssh" </> "authorized_keys"
  ok <- doesFileExist fName
  if ok
  then readPublicFile fName
  else return []

-- keys available in the file system, only in the .ssh directory:

readIdentityFiles :: IO [SSHIdentity]
readIdentityFiles = do
  homeDir <- getEnv "HOME"
  fs <- filterCandidates <$> getDirectoryContents (homeDir </> ".ssh")
  let helper name = let dir = homeDir </> ".ssh"
                    in map (\ r -> SI r (dir </> name)) <$> (readPublicFile (dir </> name))
  xs <- concat <$> mapM helper fs
  return xs

filterCandidates :: [FilePath] -> [FilePath]
filterCandidates rs =
    let rs'  = filter ((== ".pub") . takeExtension) rs
        rs'' = filter ((`elem` rs) . dropExtension) rs'
    in rs''

-- keys available through ssh-agent:

readAgentKeys :: IO [SSHIdentityHash]
readAgentKeys = map hashParser . lines <$> readProcess "ssh-add" [ "-L" ] ""

-- oversimplified parser for and SSH Identity Hash, found in public
-- parts of the key file and in authorized_keys:

hashParser :: String -> SSHIdentityHash
hashParser = (\(a:b:rest) -> SIH a b (unwords rest)) . words

------------------------------------------------------------------------

-- we still need to treat connectivity separately:

writeConfiguration :: IO ()
writeConfiguration = do
  hostName   <- readProcess "hostname" [ "-s" ] ""
  user       <- getEnv "USER"
  identities <- readIdentityFiles
  authorized <- readAuthorizedKeyFile
  let n = ML hostName user identities authorized
  print ( user ++ "@" ++ hostName, n)
  
