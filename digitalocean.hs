------------------------------------------------------------------------------
--  DigitalOcean Image/Droplet management (API V2)
------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveGeneric #-}

import Data.Aeson
import Data.List
import Data.Aeson.Types hiding (Options, defaultOptions)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad
import Text.Printf

import Network.HTTP.Conduit

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method

import GHC.Generics
import System.Console.GetOpt
import System.Environment (getArgs, lookupEnv)
import Control.Applicative
import Control.Monad.Trans.Reader

digitalocean :: String
digitalocean = "https://api.digitalocean.com/v2/"

data Options = Options {
    nameArg :: Maybe T.Text,
    regionArg :: T.Text,
    sizeArg  :: T.Text,
    imageArg :: T.Text,
    imageidArg :: Integer,
    helpArg  :: Bool,
    tokenArg :: B.ByteString
} deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options {
    nameArg = Nothing,
    regionArg = "nyc2",
    sizeArg = "512mb",
    imageArg = "ubuntu-14-04-x64",
    imageidArg = 0,
    helpArg = False,
    tokenArg = B.empty
}

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"]
        (NoArg (\o->o{helpArg=True}))
        "help message",
    Option ['n'] ["name"]
        (ReqArg (\n o->o{nameArg = Just $ T.pack n}) "Name")
        "droplet name",
    Option ['r'] ["region"]
        (ReqArg (\n o->o{regionArg = T.pack n}) "Region")
        "region",
    Option ['s'] ["size"]
        (ReqArg (\n o->o{sizeArg = T.pack n}) "Size")
        "size",
    Option ['d'] ["imageid"]
        (ReqArg (\n o->o{imageidArg = read n}) "Imageid")
        "imageid",
    Option ['i'] ["image"]
        (ReqArg (\n o->o{imageArg = T.pack n}) "Image")
        "image" ]

actionTable :: [(String, ReaderT Options IO ())]
actionTable = [ ("listimages",    listimages),
                ("listregions",   listregions),
                ("listsizes",     listsizes),
                ("listdroplets",  listdroplets),
                ("createdroplet", createdroplet),
                ("deletedroplet", deletedroplet)]

data CreateDropletReq = CreateDropletReq {
    cname  :: !T.Text,
    cregion:: !T.Text,
    csize  :: !T.Text,
    cimage :: Integer,
    cssh_keys :: ![Integer],
    cbackups:: Bool,
    cipv6  :: Bool,
    cprivate_networking :: Bool
} deriving (Show, Generic)

instance FromJSON CreateDropletReq where
    parseJSON (Object v) =
        CreateDropletReq    <$> v .: "name"
                            <*> v .: "region"
                            <*> v .: "size"
                            <*> v .: "image"
                            <*> v .: "ssh_keys"
                            <*> v .: "backups"
                            <*> v .: "ipv6"
                            <*> v .: "private_networking"
        
instance ToJSON CreateDropletReq where
    toJSON r = object [
        "name" .= cname r,
        "region" .= cregion r,
        "size" .= csize r,
        "image" .= cimage r,
        "ssh_keys" .= cssh_keys r,
        "backups" .= cbackups r,
        "ipv6" .= cipv6 r,
        "private_networking" .= cprivate_networking r ]

makebody :: Options -> [Integer] -> L.ByteString
makebody option keys =
    encode $ CreateDropletReq dropletname region size image keys
        False True True
    where
        dropletname = fromJust $ nameArg option
        region = regionArg option
        size = sizeArg option
        image = imageidArg option

createdroplet :: ReaderT Options IO ()
createdroplet = do
    option <- ask
    token <- return $ tokenArg option
    maybename <- return $ nameArg option
    when (not $ isJust maybename) $
        liftIO $ ioError (userError ("please provide image name"))
    keys <- liftIO $ listkeys token
    imageid <- if imageidArg option == 0
        then liftIO $ getimageid token (imageArg option)
        else return $ imageidArg option
    let newoption = option {imageidArg = imageid}
    bodytext <- return $ makebody newoption keys

    liftIO $ (sendReq "droplets" methodPost token bodytext :: IO Value)
    return ()

{- Retrieve Droplets -}
data Network = V4 {
    ip_address :: !T.Text,
    netmask :: !T.Text,
    gateway :: !T.Text,
    ntype :: Bool
} deriving (Show, Generic)

network_type :: [(T.Text, Bool)]
network_type = [ ("public", True), ("private", False) ]

instance FromJSON Network where
    parseJSON (Object v) =
        V4      <$> v .: "ip_address"
                <*> v .: "netmask"
                <*> v .: "gateway"
                <*> ((fromJust . (flip lookup) network_type) <$> v .: "type")
instance ToJSON Network

data Networks = Networks {
    v4 :: [Network]
} deriving (Show, Generic)

instance FromJSON Networks
instance ToJSON Networks

data Droplet = Droplet {
    did :: Integer,
    dname :: !T.Text,
    status :: !T.Text,
    networks :: Networks,
    dregion :: Region,
    dimage :: Image,
    dsize  :: Size,
    locked :: Bool,
    dcreated_at :: !T.Text
} deriving (Show, Generic)

instance FromJSON Droplet where
    parseJSON (Object v) =
        Droplet <$> v .: "id"
                <*> v .: "name"
                <*> v .: "status"
                <*> v .: "networks"
                <*> v .: "region"
                <*> v .: "image"
                <*> v .: "size"
                <*> v .: "locked"
                <*> v .: "created_at"

instance ToJSON Droplet

listdroplets' :: ReaderT Options IO [Droplet]
listdroplets' = do
    token <- liftM tokenArg ask
    liftIO $ sendReq "droplets" methodGet token L.empty >>=
        decomposetoplevel "droplets"

listdroplets :: ReaderT Options IO ()
listdroplets = do
    v <- listdroplets'
    liftIO $ do
        forM_ v $ \m ->
            let nws = v4 $ networks m
                public'  = map ip_address $ filter ntype nws
                private' = map ip_address $ filter (not . ntype) nws
                public  | null public' = ""
                        | otherwise = head public'
                private | null private' = ""
                        | otherwise = head private'
                imageid  = T.pack $ show $ iid $ dimage m
            in  TIO.putStrLn $ dname m `T.append` " " `T.append` public
                `T.append` " " `T.append` private `T.append` " " `T.append` imageid

finddroplet :: ReaderT Options IO Integer
finddroplet = do
    name <- liftM (fromJust . nameArg) ask
    v <- listdroplets'
    liftIO $ do
        let drop = filter (\d -> dname d == name) v
        when (null drop) $
            ioError (userError $ "droplet " ++ T.unpack name ++ " not found")
        return $ did $ head drop

sendReq' :: String -> Method -> B.ByteString -> L.ByteString ->
            IO (Response L.ByteString)
sendReq' urlpartial method token body = do
    req <- parseUrl $ digitalocean ++ urlpartial
    let headers = [(hAuthorization, "Bearer " `B.append` token)] ++
            if not $ L.null body
                then [(hContentType, "application/json")]
                else []
    let req' = req {
        method = method,
        requestHeaders = headers,
        requestBody = RequestBodyLBS body }
    withManager $ \man -> httpLbs req' man

sendReq :: FromJSON a => String -> Method -> B.ByteString -> L.ByteString ->
           IO a
sendReq urlpartial method token body = do
    resp <- sendReq' urlpartial method token body
    either (ioError . userError) return $ eitherDecode $ responseBody resp

sendReq_ :: String -> Method -> B.ByteString -> L.ByteString -> IO ()
sendReq_ urlpartial method token body =
    sendReq' urlpartial method token body >> return ()

deletedroplet :: ReaderT Options IO ()
deletedroplet = do
    token <- liftM tokenArg ask
    maybename <- liftM nameArg ask
    when (not $ isJust $ maybename) $
        liftIO $ ioError (userError ("please provide image name"))
    name <- return $ fromJust maybename
    i <- finddroplet
    liftIO $ (sendReq_ ("droplets/" ++ show i)
        methodDelete token L.empty)

{- Retrieve Images -}
data Image = Image {
    iid :: Integer,
    name :: !T.Text,
    distribution :: !T.Text,
    slug :: Maybe T.Text,
    public :: Bool,
    iregions :: ![T.Text],
    created_at :: !T.Text
} deriving (Show, Generic)

instance FromJSON Image where
    parseJSON (Object v) =
        Image   <$> v .: "id"
                <*> v .: "name"
                <*> v .: "distribution"
                <*> v .:? "slug"
                <*> v .: "public"
                <*> v .: "regions"
                <*> v .: "created_at"

instance ToJSON Image

decomposetoplevel :: FromJSON a => T.Text -> Object -> IO a
decomposetoplevel key value = either (ioError . userError) return $
                                parseEither (.: key) value

getimageid :: B.ByteString -> T.Text -> IO Integer
getimageid token image = do
    v <- sendReq ("images/" ++ T.unpack image) methodGet token L.empty >>=
            decomposetoplevel "image"
    return $ iid v

listimages :: ReaderT Options IO ()
listimages = do
    token <- liftM tokenArg ask
    liftIO $ do
        v <- sendReq "images" methodGet token L.empty >>=
                decomposetoplevel "images"
        forM_ (v) $ \m ->
            putStrLn $ printf "%d %s %s" 
                (iid m) (T.unpack $ name m) (T.unpack (maybe "Null" id (slug m)))

{- Retrieve a list of keys -}
data Key = Key {
    kid :: Integer,
    kname :: !T.Text,
    fingerprint :: !T.Text,
    public_key :: !T.Text
} deriving (Show, Generic)

instance FromJSON Key where
    parseJSON (Object v) =
        Key     <$> v .: "id"
                <*> v .: "name"
                <*> v .: "fingerprint"
                <*> v .: "public_key"

instance ToJSON Key

listkeys :: B.ByteString -> IO [Integer]
listkeys token = do
    v <- liftIO $ sendReq "account/keys" methodGet token L.empty >>=
            decomposetoplevel "ssh_keys"
    return $ map kid v

{- Retrieve Regions -}

data Region = Region {
    rname :: !T.Text,
    rslug :: Maybe T.Text,
    rsizes :: ![T.Text],
    available :: Bool,
    features :: ![T.Text]
} deriving (Show, Generic)

instance FromJSON Region where
    parseJSON (Object v) =
        Region  <$> v .: "name"
                <*> v .: "slug"
                <*> v .: "sizes"
                <*> v .: "available"
                <*> v .: "features"

instance ToJSON Region

listregions :: ReaderT Options IO ()
listregions = do
    token <- liftM tokenArg ask
    liftIO $ do
        v <- liftIO $ sendReq "regions" methodGet token L.empty >>=
                decomposetoplevel "regions"
        forM_ (filter available v) $ \m ->
            let s = rslug m in
            when (isJust s) $ TIO.putStrLn $ fromJust $ s

{- Retrieve Sizes -}

data Size = Size {
    sslug :: !T.Text,
    memory:: Integer,
    vcpus :: Integer,
    disk  :: Integer,
    transfer :: Integer,
    price_monthly :: !T.Text,
    price_hourly :: !T.Text,
    sregions ::  ![T.Text]
} deriving (Show, Generic)

instance FromJSON Size where
    parseJSON (Object v) =
        Size    <$> v .: "slug"
                <*> v .: "memory"
                <*> v .: "vcpus"
                <*> v .: "disk"
                <*> v .: "transfer"
                <*> v .: "price_monthly"
                <*> v .: "price_hourly"
                <*> v .: "regions"

instance ToJSON Size

listsizes :: ReaderT Options IO ()
listsizes = do
    token <- liftM tokenArg ask
    liftIO $ do
        v <- liftIO $ sendReq "sizes" methodGet token L.empty >>=
                decomposetoplevel "sizes"
        forM_ v $ TIO.putStrLn . sslug

header = "Usage: digitalocean [OPTION...] COMMAND\n    COMMAND:\n" ++
         (intercalate "\n" $ map fst actionTable) ++ "\n\n"

main :: IO ()
main = do
    args <- getArgs
    let (opt, nonOpts, msgs) = getOpt Permute options args
    when (not $ null msgs) $ ioError (userError
        (concat msgs ++ usageInfo header options))
    when (null nonOpts) $ ioError (userError (usageInfo header options))
    flags <- return $ foldl (flip id) defaultOptions opt
    maybetoken <- lookupEnv "TOKEN"
    when (not $ isJust maybetoken) $
        ioError (userError ("please set environment variable TOKEN"))
    op <- return $ flags {tokenArg = B.pack  $ fromJust maybetoken}
    when (helpArg op) $ ioError (userError (usageInfo header options))
    case lookup (head nonOpts) actionTable of
        Just cmd -> runReaderT cmd op
        _ -> ioError (userError (usageInfo header options))
    return ()
