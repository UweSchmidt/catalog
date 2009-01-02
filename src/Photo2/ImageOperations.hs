module Photo2.ImageOperations
where

import qualified Control.Exception as CE

import           Control.Monad.Error hiding ( liftIO )
import qualified Control.Monad.Error as ME

import           Control.Parallel.Strategies( rnf )


import           Data.Atom
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C
import           Data.Char
import           Data.List
import qualified Data.Map as M
import           Data.Maybe

import           Photo2.ArchiveTypes
import           Photo2.ExifData
import           Photo2.FilePath

import           System	( system )
import           System.Directory
import           System.Exit
import           System.IO
import           System.Posix	( getProcessID )
import           System.Time	( ClockTime
				, toCalendarTime
				, formatCalendarTime
				, getClockTime
				)
import           System.Locale	( defaultTimeLocale )

import           Text.Regex

import           Text.XML.HXT.RelaxNG.XmlSchema.RegexMatch

-- ------------------------------------------------------------

type IOE a	= ErrorT String IO a

-- ------------------------------------------------------------

-- lift IO commands to IOE commands and map IO errors to ErrorT errors

liftIO	:: IO a -> IOE a
liftIO a
    = do
      r <- ME.liftIO $
           catch' ( do
		    r1 <- a
		    return (Right r1)
		  )
                  (\ err -> return (Left $ show err))
      evalRc r
    where
    catch' :: IO a -> (CE.SomeException -> IO a) -> IO a
    catch' = CE.catch
    evalRc (Left msg)	= throwError msg
    evalRc (Right res)	= return res

mapError	:: IOE a -> (String -> String) -> IOE a
mapError a f
    = a `catchError` (throwError . f)

dryCmd	:: Bool -> String -> IOE () -> IOE ()
dryCmd True   msg  _cmd	= liftIO $ hPutStrLn stderr ("dry run: " ++ msg)
dryCmd False _msg   cmd	= cmd

-- ------------------------------------------------------------

mergeAttrs	:: Attrs -> Attrs -> Attrs
mergeAttrs n o	= M.foldWithKey mergeAttr o n

mergeAttr	:: Atom -> Value -> Attrs -> Attrs
mergeAttr k v
    | null v	= M.delete k
    | otherwise	= M.insert k v

remAttrs	:: String -> Attrs -> Attrs
remAttrs kp	= M.foldWithKey remK M.empty
		  where
		  remK k a m
		      | match kp (show k)	= m
		      | otherwise		= M.insert k a m

-- ------------------------------------------------------------

mvPic	:: Name -> Config -> Path -> Pic -> IOE Pic
mvPic newName c p pic
    = do
      mapM renameCopy $ (map show . M.keys . load theCopies $ pic)
      return $ store theId newName pic
    where
    imgtype	= getDefOpt "jpg"           "imgtype" c

    renameCopy	:: Name -> IOE ()
    renameCopy	dir
	= do
	  when (isAl pic)		-- rename album dir
	       (mvFile src dir)
	  mvFile (src `addExtension` imgtype) (dst `addExtension` imgtype)
	where
	src = dir </> joinPath p
	dst = dir </> joinPath (init p) </> newName

-- ------------------------------------------------------------

importExifAttrs	:: Config -> Path -> Pic -> IOE Pic
importExifAttrs c _p pic
    = do
      ex <- liftIO $ doesFileExist orig
      when (not ex)
           ( throwError $ "importExifAttrs: original image " ++ show orig ++ " not found" )
      up <- upToDate
      if up
	 then return pic
	 else do
	      newData <- allImgAttrs [] c orig raw xmp
	      let pic' = change theAttrs (mergeAttrs newData) pic
	      return (rnf pic' `seq` pic')
    where
    orig	= base </-> picOrig pic
    raw		= base </-> picRaw  pic
    xmp		= base </-> picXmp  pic
    modified	= fromMaybe "" . M.lookup fileModificationKey . picAttrs $ pic

    base	= getImportBase c

    force	= optON  optForceExif c
    dry		= optOFF optForceExif c

    upToDate
	| dry		= return True
	| force		= return False
	| otherwise	= liftIO $ fileNewerThanDate modified orig

-- ------------------------------------------------------------

allImgAttrs	:: [String] -> Config -> String -> String -> String -> IOE Attrs
allImgAttrs opts c orig raw xmp
    = do
      exifDataOrig <- imgAttrs orig
      exifDataRaw  <- imgAttrs raw
      xmpData      <- imgAttrsXmp xmp
      return ( ( ( exifDataOrig
		   `M.union` exifDataRaw
		 )
		 `M.union` xmpData
	       )
	       `M.union` fileData
	     )
    where
    debug	= optON  optDebug     c
    exifAttrs t	= parseExif (confPicAttrs c) t

    imgAttrs ""
	= return $ emptyAttrs
    imgAttrs f
	= do
	  t <- execFct debug (["exiftool"] ++ opts ++ [f]) -- don't use "-s" option
	  let res = exifAttrs t
	  return $ res

    imgAttrsXmp ""
	= return $ emptyAttrs
    imgAttrsXmp _f
	= do
	  return $ emptyAttrs	-- parseXmp t

    fileData
	= exifAttrs . unlines $
	  [ "RefOrig : " ++ orig
	  , "RefRaw : "  ++ raw
	  , "RefXmp : "  ++ xmp
	  ]


-- ------------------------------------------------------------

importOrig	:: Config -> Path -> Pic -> IOE Pic
importOrig c p pic
    = do
      ex <- existsSrc
      when (not ex)
           ( throwError $ "importOrig: original file " ++ show src ++ " does not exist" )
      up <- upToDate
      if not up
	 then do
	      mkDirectoryPath dst
	      copy
	      geo <- getImageSize dst
	      let pic' = change theCopies (M.insert (newAtom dir) (Copy geo)) pic
	      return (rnf pic' `seq` pic')
	 else return pic
    where
    existsSrc	= liftIO $ doesFileExist src

    copy
	| extension src == extension dst	-- simple copy
	    = liftIO $ copyFile src dst
	| otherwise				-- conversion with convert command
	    = do
	      execFct debug shellcmd
	      return ()

    upToDate
	| dry		= return True
	| force		= return False
	| otherwise	= liftIO $ fileNewerThanFile src dst

    src		= base </-> picOrig pic
    dst		= dir  </> joinPath p `addExtension` imgtype

    base	= getImportBase                       c
    dir         = getDefOpt "org"           "dir"     c
    imgtype	= getDefOpt "jpg"           "imgtype" c

    debug	= optON  optDebug     c
    force	= optON  optForceOrig c
    dry		= optOFF optForceOrig c

    shellcmd
	-- | extension src == extension dst
	--    = [ "cp", src, dst, "&&", "chmod", "644", dst ]
	| otherwise
	    = [ "convert", "-quality", "90", src, dst ]

-- ------------------------------------------------------------

createCopy	:: Config -> Path -> Size -> Pic -> IOE Pic
createCopy c p s pic
    = do
      ex <- existsSrc
      when (not ex && not dry)
           ( throwError $ "createCopy: original file " ++ show src ++ " does not exist" )
      mkDirectoryPath dst
      up <- upToDate
      if not up
	 then do
	      resize
	      geo <- getImageSize dst
	      let pic' = change theCopies (M.insert (newAtom . sizeDir $ s) (Copy geo)) pic
	      return (rnf pic' `seq` pic')
	 else return pic
    where
    existsSrc	= liftIO $ doesFileExist src

    resize
	= resizeImage debug src dst (crGeo aspect) (rGeo aspect)
	  where
	  rGeo Fix = cGeo
	  rGeo _   = resizeGeo sGeo cGeo

	  crGeo Fix	= cropGeo sGeo cGeo
	  crGeo Pad	= (sGeo, Geo (-1) (-1))
	  crGeo Crop	= (sGeo, emptyGeo)

	  sGeo   = maybe emptyGeo copyGeo . M.lookup (newAtom dir) . picCopies $ pic
	  cGeo   = sizeGeo    s
	  aspect = sizeAspect s

    upToDate
	| dry		= return True
	| force		= return False
	| otherwise	= liftIO $ fileNewerThanFile src dst

    dir         = getDefOpt "org"           "dir"     c
    imgtype	= getDefOpt "jpg"           "imgtype" c

    debug	= optON  optDebug     c
    force	= optON  optForceCopy c
    dry		= optOFF optForceCopy c

    img         = joinPath p `addExtension` imgtype
    src		= dir       </> img
    dst		= sizeDir s </> img

-- ------------------------------------------------------------
--
-- | image resize
--
-- croping may be given by @(cropWidth, cropHeight, xOffset, yOffset)@ for
-- generating an image with a specific aspect ration

resizeImage	:: Bool -> String -> String -> (Geo, Geo) -> Geo -> IOE ()
resizeImage debug src dst (Geo cw ch, Geo xoff yoff) (Geo w h)
    = do
      execFct debug shellCmd
      return ()
    where
    unsharp	= [] -- ["-unsharp", "0.7x0.7+1.0+0.05"] -- sharpen option removed
    resize	= ["-thumbnail", show w ++ "x" ++ show h ++ "!"]
    resize1	= ["-geometry", show w ++ "x" ++ show h, "-thumbnail", show w ++ "x" ++ show h]
    quality	= ["-quality", "85"]
    interlace	= [ "-interlace", "Plane" ]
    isPad	= (xoff == (-1) && yoff == (-1))
    isCrop	= (xoff > 0     || yoff > 0)
    cmdName
	| isPad		= [ "montage" ]
	| otherwise	= [ "convert" ]
    cmdArgs
	| isPad		= resize1
			  ++ [ "-background", "#333333" ]
			  -- ++ [ "-size", show (2*w) ++ "x" ++ show (2*h) ] -- this gives too low quality
			  ++ [ src, dst ]
	| isCrop	= [ "-crop", show cw ++ "x" ++ show ch ++ "+" ++ show xoff ++ "+" ++ show yoff
			  , src, "miff:-"
			  , "|"
			  , "convert"
			  ]
			  ++ resize ++ unsharp ++ quality
			  ++ ["miff:-", dst ]
	| otherwise	= resize ++ unsharp
			  ++ [ src, dst ]
    shellCmd	= cmdName
		  ++ interlace
		  ++ quality
		  ++ cmdArgs

resizeGeo	:: Geo -> Geo -> Geo
resizeGeo sGeo@(Geo sw sh) (Geo dw dh)
    | sw <= dw && sh <= dh		-- source fits into display
	= sGeo				-- no downsizing, no magnification

    | sw * dh >= dw * sh		-- source wider than display
	= Geo dw (dw * sh `div` sw)	-- maximum width, height scaled down

    | otherwise				-- source higher than display
	= Geo (dh * sw `div` sh) dh	-- maximum height, width scaled down


cropGeo		:: Geo -> Geo -> (Geo, Geo)
cropGeo (Geo sw sh) (Geo dw dh)
    | sw *dh >= dw * sh			-- source wider than reqired
	= (Geo sw' sh, Geo xoff 0)
    | otherwise				-- sorce highter than required
	= (Geo sw sh', Geo 0 yoff)
    where
    sw'  = dw * sh `div` dh
    xoff = (sw - sw') `div` 2		-- cut off left and right parts
    sh'  = dh * sw `div` dw
    yoff = (sh - sh') `div` 3		-- cut off 1/3 from top and 2/3 from bottom
					-- else important parts like heads are cut off (Ouch!!)

-- ------------------------------------------------------------

getImageSize	:: String -> IOE Geo
getImageSize f
    = do
      res <- execFct False ["identify", "-ping", f]
	     `catchError`
	     const (return "")
      return ( maybe (Geo 0 0) (readGeo . head) (matchRegex geometryRE res) )
    where
    geometryRE	:: Regex
    geometryRE
	= mkRegex ( "(" ++ digit1 ++ digit0 ++ "*x" ++ digit1 ++ digit0 ++ "*)" )
	  where
	  digit0 = "[0-9]"
	  digit1 = "[1-9]"

-- ------------------------------------------------------------
--
-- call of external programs
-- and file handling

-- | quote a string to be used as command line argument for a system call

addArg		:: String -> String
addArg ";"	= " ;"
addArg "|"	= " |"
addArg "||"	= " ||"
addArg "&&"	= " &&"
addArg ">"	= " >"
addArg "2>"	= " 2>"
addArg t	= " '" ++ concatMap (\ c -> if c == '\'' then "\\'" else [c]) t ++ "\'"

-- | execute a shell command and return exit code

exec		:: String -> IOE ()
exec cmd
    = do
      rc <- liftIO $ system cmd
      exit rc
    where
    exit ExitSuccess	   = return ()
    exit (ExitFailure rc ) = throwError ("command " ++ show cmd ++ " exited with rc=" ++ show rc)

-- ------------------------------------------------------------

execFct		:: Bool -> [String] -> IOE String

execFct _ []
    = return ""

execFct debug (cmd : args)
    = do
      pid <- liftIO $ getProcessID
      let tmpName = "/tmp/album-" ++ show pid
      let errDev  = "/dev/null"
      let ioReDir = [">", tmpName, "2>", errDev]
      let command = cmd ++ concatMap addArg args
      when debug
           ( liftIO $ hPutStrLn stderr ("executed: " ++ command) )
      let command' = command ++ concatMap addArg ioReDir
      exec command'
      catchError ( do
		   res <- liftIO $ B.readFile tmpName
		   rmFile tmpName
		   return (C.unpack res)
		 ) ( \ _ -> return "" )

-- ------------------------------------------------------------

mkDirectoryPath		:: String -> IOE ()
mkDirectoryPath f
    = do
      ex <- liftIO $ doesDirectoryExist dir
      when (not ex)
           ( ( liftIO $ createDirectoryIfMissing True dir )
	     `mapError`
	     (("createDirectory " ++ dir ++ " failed: ") ++)
	   )
    where
    dir = dirPath f

-- ------------------------------------------------------------

mvFile		:: String -> String -> IOE ()
mvFile src dst
    = ( do
	ex <- liftIO $ doesFileExist src
	when ex (liftIO $ renameFile src dst)
      )
      `mapError` (("rename " ++ show src ++ " " ++ show dst ++ " failed: ") ++)

-- ------------------------------------------------------------

rmFile		:: String -> IOE ()
rmFile f
    = ( do
	ex <- liftIO $ doesFileExist f
	when ex (liftIO $ removeFile f)
      )
      `mapError` (("remove " ++ show f ++ " failed: ") ++)

-- ------------------------------------------------------------

mkBackupFile	:: String -> String -> IOE ()
mkBackupFile bak f
    = do
      ex <- liftIO $ doesFileExist f
      when ex
	   ( liftIO $ copyFile f (f ++ bak) )

-- ------------------------------------------------------------
--
-- simple IO actions for time stamps and date and time functions

formatDateTime	:: IO ClockTime -> IO String
formatDateTime timeStamp
    = catch
      ( do
	ctime <- timeStamp
	time  <- toCalendarTime ctime
	return $ formatCalendarTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
      )
      ( \_ -> return "" )

getTimeStamp	:: IO String
getTimeStamp	= formatDateTime getClockTime

-- | read the last modified time stamp of a file

fileLastModified	:: String -> IO String
fileLastModified f	= formatDateTime (getModificationTime f)

-- | compare 2 file stamps
--
-- @fileNewerThanFile reference file@
-- if reference does not exist, return False,
-- else if file does not exist, return False
-- else compare time stamps

fileNewerThanFile	:: String -> String -> IO Bool
fileNewerThanFile ref f
    = do
      mf   <- fileLastModified f
      mref <- fileLastModified ref
      let n = ( not (null mf)
	       &&
	       not (null mref)
	       &&
	       mref <= mf
	     )
      -- putStrLn ("file newer file=" ++ show f ++ "," ++ show mf ++ " ref=" ++ show ref ++ "," ++ show mref ++ " status=" ++ show n)
      return n

fileNewerThanFiles	:: [String] -> String -> IO Bool
fileNewerThanFiles [] _f
    = return True
fileNewerThanFiles (r:refs) f
    = do
      newer <- fileNewerThanFile r f
      if newer
	 then fileNewerThanFiles refs f
	 else return False

-- | compare a file stamp with a time stamp
--
-- @fileNewerThanDate dateRef file@
-- if dateRef is empty, return False,
-- else if file does not exist, return True
-- else compare time stamps

fileNewerThanDate	:: String -> String -> IO Bool
fileNewerThanDate dref f
    = if null dref
      then return False
      else do
	   mf   <- fileLastModified f
	   return ( not (null dref)
		    &&
		    dref <= mf
		  )

fileNewerThanDates	:: [String] -> String -> IO Bool
fileNewerThanDates [] _f
    = return True
fileNewerThanDates (r:refs) f
    = do
      newer <- fileNewerThanDate r f
      if newer
	 then fileNewerThanDates refs f
	 else return False

-- ------------------------------------------------------------
{-
cleanupDir		:: FilePath -> [String] -> IOE ()
cleanupDir dir fl	= liftIO $ cleanupDir' dir fl
-}
cleanupDir		:: Bool -> FilePath -> [String] -> IOE ()
cleanupDir execute dir fl
    = do
      ex <- liftIO $ doesDirectoryExist dir
      when ex $
	   do
	   fl' <- liftIO $ getDirectoryContents dir
	   mapM_ cleanEntry fl'
	   return ()
    where
    cleanEntry e
	| e `elem` fl
	    = return ()
	| otherwise
	    = do
	      isd <- liftIO $ doesDirectoryExist e'	-- don't touch dirs
	      when (not isd) $
		   do
		   when execute (rmFile e')
		   liftIO $ hPutStrLn stderr msg
	where
	e' = dir </> e
	msg	| execute	= "unused file removed " ++ show e'
		| otherwise	= "unused file found "   ++ show e'

-- ------------------------------------------------------------
