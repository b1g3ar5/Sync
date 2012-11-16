
module Sync
    (
        saferGetModificationTime
        , file2fileStats
        , dontCopyFile
        , toDelete
        , toCopy
        , copy
        , rm
        , getDir
        , saferFileSize
        , doNothing  
        , FileStats(..)
        , Dir(..)
        , mdtm
        , Path(..)
        , FtpPath(..)
        , bookclub
        , open_bookclub
        , brayfordlets
        , open_brayfordlets
        , sync
        , seqList
    ) where

import System.IO
import Control.Monad (liftM)
import Control.Exception (handle)
import Data.List (deleteFirstsBy)
import System.Directory
--import System.FilePath.Posix (replaceDirectory)
import Data.Time.Clock (UTCTime)
import Network.FTP.Client
import Network.FTP.Client.Parser
import Data.List.Utils (seqList)
-- My date conversion routines
import Date

data Path = Ftp FtpPath | Local FilePath

instance Show Path where
    show p = case p of
                Ftp fp -> address fp
                Local fp -> fp
                
instance Show FTPConnection where
    show = const "This is an FTP connection"

data FtpPath = FtpPath {  address::String
                        , usr::String
                        , pwd::String
                        , conn::FTPConnection
                        , res::FTPResult
                     } deriving (Show)
                     
data FileStats = FileStats {fname::FilePath, fsize::(Maybe Integer), fdate::Maybe UTCTime} deriving (Show)
data Dir = Dir {path::Path, fs::[FileStats]} deriving (Show)

open_bookclub::FilePath->IO (Dir, Dir)
open_bookclub pp = do
        let brox_address = "ftp1.namesco.net"
        let brox_usr = "broxholme.com"
        let brox_pwd = "55s1nc3r1ty"
        brox_conn <- easyConnectFTP brox_address
        _ <- login brox_conn brox_usr (Just brox_pwd) Nothing
        brox_res2 <- cwd brox_conn $ "./web/" ++ pp ++ "/"        
        let brox = FtpPath brox_address brox_usr brox_pwd brox_conn brox_res2
        rdir<-getDir $ Ftp brox
        let loc = Local $ "C:\\Documents and Settings\\Robin Seeley\\workspace\\Bookclub\\_site\\" ++ pp ++ "\\"
        ldir<-getDir loc        
        return (rdir, ldir)

open_brayfordlets::FilePath->IO (Dir, Dir)
open_brayfordlets pp = do
        let address = "ftp1.namesco.net"
        let usr = "brayfordlets.co.uk"
        let pwd = "5kipper55"
        conn <- easyConnectFTP address
        _ <- login conn usr (Just pwd) Nothing
        res2 <- cwd conn $ "./web/" ++ pp ++ "/"        
        let p = FtpPath address usr pwd conn res2
        rdir<-getDir $ Ftp p
        let loc = Local $ "C:\\Documents and Settings\\Robin Seeley\\workspace\\Brayfordlets\\_site\\" ++ pp ++ "\\"
        ldir<-getDir loc        
        return (rdir, ldir)

bookclub::IO [[String]]
bookclub = do
        let ds = ["pages", "css", "xml"]
        mapM (\d-> do
                    (rdir, ldir) <- open_bookclub d
                    sync ldir rdir
            ) ds

brayfordlets::IO [[String]]
brayfordlets = do
        let ds = ["pages", "css", "images", "photos"]
        mapM (\d-> do
                    (rdir, ldir) <- open_brayfordlets d
                    sync ldir rdir
            ) ds

-- This is how to use these functions
sync::Dir->Dir->IO [String]
sync new old = do
    lcstr <- copy (toCopy new old) old
    lrstr <- rm (toDelete old new)
    return $ lcstr ++ lrstr
    
-- currently old must be local!!!
copy::Dir->Dir->IO [String]
copy new old = do
    setCurrentDirectory $ show $ path new
    case path old of
        Ftp opath -> sequence $ map (\fs'-> liftM show $ uploadbinary (conn opath) (fname fs')
                                    ) (seqList $ fs new)
        Local _ -> sequence $ map (\fs'-> liftM show $ copyFile ((show $ path new) ++ fname fs') ((show $ path old) ++ fname fs')
                                  ) (fs new)
    
rm::Dir->IO [String]
rm old = do
    case path old of
        Local opath -> sequence $ map (\f-> liftM show $ removeFile $ opath ++ fname f) (fs old)
        Ftp opath -> sequence $ map (\f-> liftM show $ delete (conn opath) (fname f)
                                    ) (seqList $ fs old)
    

-- | Gets the modification time of a file over an FTP connection
-- | Wraps the time in a @Maybe@ so that non existant files are caught
-- Has to be in IO because of getNodificationTime
mdtm :: (Num a, Read a) => FTPConnection -> String -> IO a
mdtm h fn = do
            r <- sendcmd h ("MDTM " ++ fn)
            forceioresp 200 r
            return (read . head . snd $ r)


           
-- | Wraps the time in a @Maybe@ so that non existant files are caught
-- Has to be in IO because of getNodificationTime
saferGetModificationTime::FilePath->IO (Maybe UTCTime)
saferGetModificationTime f = do
    ok <- doesFileExist f
    case ok of
        True -> do
                    ct <- getModificationTime f 
                    return $ Just $ clock2UTC ct
        False -> return Nothing
      
-- | Conversion of filepath to FileStats      
-- Has to be in IO because of getNodificationTime and saferFileSize
file2fileStats::Path->FilePath->IO FileStats
file2fileStats p f =
    case p of
        Local pname ->  do 
                            sz <- saferFileSize $ pname ++ f
                            tm <- saferGetModificationTime $ pname ++ f 
                            return (FileStats f sz tm)
        Ftp pname   -> do
                            sz <- size (conn pname) f
                            tm <- mdtm (conn pname) f
                            return (FileStats f (Just sz) (Just $ ftpInt2UTC tm))
    
-- | Don't copy returns false when the new file is older than the old one
dontCopyFile::FileStats->FileStats->Bool
dontCopyFile new old = (fname new == fname old) && (fdate new <= fdate old)
    
-- | Creats a list of all the files in old that not in the new list
-- These are the files that eneed to be deleted
toDelete::Dir->Dir->Dir
toDelete (Dir n []) _ = Dir n[]
toDelete old (Dir _ []) = old
toDelete old new = Dir (path old) $ deleteFirstsBy (\o n -> fname n == fname o) (fs old) (fs new)

-- | Creates a list of all the files that need to be copied
-- This will be all the ones that don't satisfy dontCopyFile
toCopy::Dir->Dir->Dir
toCopy (Dir n []) _ = Dir n []
toCopy new (Dir _ []) = new
toCopy new old = Dir (path new) $ deleteFirstsBy (\o n -> (fname n == fname o) && (fdate n <= fdate o)) (fs new) (fs old)

-- | Has to be in IO because of getDirectoryContents
-- handles potential errors in getDirectoryContents
-- FilePath should include the trailing '/'
getDir::Path->IO Dir
getDir p = handle doNothing2 $ do 
    case p of
        Ftp fp -> do 
                    files <- nlst (conn fp) Nothing
                    fs' <- sequence $ map (file2fileStats p) $ seqList files 
                    return $ Dir p fs'
        Local lpath -> do 
                        files <- liftM (filter (\x-> (x/="." && x/=".."))) $ getDirectoryContents lpath
                        fs'<- sequence $ map (file2fileStats p) files 
                        return $ Dir p fs'
  
-- | Has to be in IO because we have to open the file
-- handles potential errors in openFile
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path' = handle doNothing $ do
  h <- openFile path' ReadMode
  size' <- hFileSize h
  hClose h
  return (Just size') 
   
-- I think handle catches erorrs - so this just returns Nothing when it does
doNothing :: IOError -> IO (Maybe a)
doNothing _ = return Nothing    

-- I think handle catches errors - so this just returns Nothing when it does
doNothing2 :: IOError -> IO Dir
doNothing2 _ = return (Dir (Local "") [])