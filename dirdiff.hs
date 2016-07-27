module Main where
 
import System.IO
import System.Directory
import System.FilePath
import Control.Monad
 
fsize :: FilePath -> IO Integer
fsize path = withFile path ReadMode hFileSize
 
strFile :: (FilePath, IO Integer) -> IO [FilePath]
strFile file = do
    let file_path = fst file
    file_size <- snd file
    return [file_path ++ " " ++ show file_size]

listFiles :: FilePath -> IO [FilePath]
listFiles path = do
    t <- getDirectoryContents path
    let dir = filter (`notElem` [".", ".."]) t
    files <- mapM (\f -> process path f) dir
    return $ concat files

process :: FilePath -> FilePath -> IO [FilePath]
process path f = do
    let full_path = path </> f
    is_dir <- doesDirectoryExist full_path
    if is_dir
        then listFiles full_path
        else strFile (full_path, fsize full_path)

trimRoot :: FilePath -> [FilePath] -> IO [FilePath]
trimRoot root files = return [drop (length root + 1) file | file <- files]

main = do
    first <- getLine
    second <- getLine
 
    f_files <- listFiles first >>= trimRoot first
    s_files <- listFiles second >>= trimRoot second

    putStrLn ""
    putStrLn $ "Unique files from " ++ first
    mapM_ putStrLn [f1 | f1 <- f_files, f1 `notElem` s_files]
    
    putStrLn ""
    putStrLn $ "Unique files from " ++ second
    mapM_ putStrLn [f1 | f1 <- s_files, f1 `notElem` f_files]