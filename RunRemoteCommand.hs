module RunRemoteCommand where

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
