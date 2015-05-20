{-----------------------------------------------------------------------------------------
Module name: BufferedSocket
Made by:     Tomas Möre 2015
 

Usage:
    This module is mean to be imported qualified 

    ex: import Headers qualified BS 

Notes:    
    Buffered sockets are a data type that is a kind of overlay on normal sockets. 

    
    All the exported read / write operations are build such that they ALLWAYS read / write the ammount of bytes requested 
    
    
    This package allows some cases of lazy IO. Some people see Lazy io as the devil incarné. However it is excpected that anyone using this module 
    is cabale of understanding any possible side effects.
    

WARNINGS:
    This module uses a ton of IO and non functional ways of solving problems. 
    This is because we want to be as spacetime efficient as possible. 
    
    This module does NOT contain "beatiful" haskell code
------------------------------------------------------------------------------------------}


{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Builder   as BB 
import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 


import qualified Core as BS
import qualified Reader as BS
import qualified Writer as BS

import Data.Word
import Data.Int 
import Control.Monad

import qualified Network.Socket as NS 

import Data.IORef 
import System.Timeout
import Data.Maybe
import Control.Concurrent
import System.Exit

type ErrorMessage = String 
type DataType = String 
testServerPort = 1337
testHost = "localhost"
testServerMaxConnections = 1


bufferSize = 1024 * 10

makeTestTcpServer:: (BS.BufferedSocket -> IO ())  -> IO ()
makeTestTcpServer thunk  = NS.withSocketsDo $ do 
    -- create socket
    serverSock <- NS.socket NS.AF_INET NS.Stream 0
    -- make socket immediately reusable - eases debugging.
    NS.setSocketOption serverSock NS.ReuseAddr 1
    NS.bindSocket serverSock (NS.SockAddrInet testServerPort NS.iNADDR_ANY)
    NS.listen serverSock testServerMaxConnections
    socketData@(subSock,subSockaddr) <- NS.accept serverSock

    serverBSock <- BS.makeBufferedSocket socketData bufferSize bufferSize

    thunk serverBSock

    NS.sClose serverSock
    

makeTestTcpClient :: IO BS.BufferedSocket
makeTestTcpClient = do
    sock <- NS.socket NS.AF_INET NS.Stream 0
    let myHints = NS.defaultHints { NS.addrFlags = [],  NS.addrFamily = NS.AF_INET, NS.addrSocketType =  NS.Stream}

    (adressInfo:_) <- NS.getAddrInfo (Just myHints) (Just testHost) (Just $ show testServerPort)
    let sockAddr = NS.addrAddress adressInfo
    putStrLn $ show adressInfo
    putStrLn $ show sockAddr
    NS.connect sock sockAddr
    clientSockAddr <-  NS.getSocketName sock
    BS.makeBufferedSocket (sock, clientSockAddr) bufferSize bufferSize




main = do 
        quitMVar <- newEmptyMVar :: IO (MVar Bool)

        clientMvar <- newEmptyMVar

        let thunk = testingServerThunk quitMVar clientMvar
        forkIO $ makeTestTcpServer thunk 

        clientBSock <- makeTestTcpClient 
        putMVar clientMvar clientBSock
        readMVar quitMVar 

testByteString = "Hello world!!!"
testLazyByteString = "Hello world!!!"


testingServerThunk :: MVar Bool  -> MVar BS.BufferedSocket  -> BS.BufferedSocket -> IO ()
testingServerThunk  quitMVar clientMvar serverBSock  = do 
    clientBSock <- readMVar clientMvar

    putStrLn "Starting tests: "
    
    testNumbersFunction serverBSock clientBSock "Word8" (255 :: Word8)
    testNumbersFunction serverBSock clientBSock "Word16" (456 :: Word16)
    testNumbersFunction serverBSock clientBSock "Word32" (16 :: Word32)
    testNumbersFunction serverBSock clientBSock "Word64" (32 :: Word64)

    testNumbersFunction serverBSock clientBSock "Int8" (123 :: Int8)
    testNumbersFunction serverBSock clientBSock "Int16" (456 :: Int16)
    testNumbersFunction serverBSock clientBSock "Int32" (789 :: Int32)
    testNumbersFunction serverBSock clientBSock "Int64" (9000 :: Int64)

    testNumbersFunction serverBSock clientBSock "Int8"  (-123 :: Int8)
    testNumbersFunction serverBSock clientBSock "Int16" (-456 :: Int16)
    testNumbersFunction serverBSock clientBSock "Int32" (-789 :: Int32)
    testNumbersFunction serverBSock clientBSock "Int64" (-9000 :: Int64)

    testStringsFunction serverBSock clientBSock "ByteString"        (B.length testByteString)  (testByteString:: B.ByteString)
    testStringsFunction serverBSock clientBSock "Lazy ByteString"   (fromIntegral $ BL.length testLazyByteString) (testLazyByteString :: BL.ByteString)
    {--     

    testStringsFunction serverBSock clientBSock "String"            (length testNativeString)    (testNativeString:: String)
    testStringsFunction serverBSock clientBSock "Text"              (T.length testText)  (testText :: T.Text)
    testStringsFunction serverBSock clientBSock "Lazy Text"         (fromIntegral $ TL.length testLazyText) (testLazyText :: TL.Text)
    --}

    NS.sClose $ BS.nativeSocket serverBSock 
    NS.sClose $ BS.nativeSocket clientBSock

    putMVar quitMVar True 
    exitSuccess


testNumbersFunction:: (BS.Sendable a, Show a, BS.Readable a, Eq a) => BS.BufferedSocket -> BS.BufferedSocket -> DataType -> a -> IO ()
testNumbersFunction  serverBSock clientBSock dataType value = do 
    putStrLn $ "--------- Testing data of type: " ++ dataType ++ " ---------"
    putStrLn $ "Value: " ++ (show value)
    BS.send clientBSock value
    BS.flush clientBSock
    putStrLn $ "Data sent!"
    inData <- BS.read serverBSock 
    putStrLn "Data recieved!" 
    putStrLn $ "Recieved value: "  ++ (show inData)
    if inData == value
        then putStrLn $ "--------- " ++ dataType ++ " tests success ---------"
        else putStrLn $ "!!! ERROR: Recieved data not same as sent data" 

testStringsFunction:: (BS.Sendable a, Show a, BS.ReadableString a, Eq a) => BS.BufferedSocket -> BS.BufferedSocket -> DataType -> Int -> a ->  IO ()
testStringsFunction  serverBSock clientBSock dataType dataLength value = do 
    putStrLn $ "--------- Testing data of type: " ++ dataType ++ " ---------"
    putStrLn $ "Value: " ++ (show value)
    BS.send clientBSock value
    BS.flush clientBSock
    putStrLn "Data sent!" 
    inData <- BS.readString serverBSock dataLength 
    putStrLn "Data recieved!"
    putStrLn $ "Recieved value: " ++ (show inData)
    if inData == value
        then putStrLn $ "--------- " ++ dataType ++ " tests success ---------"
        else putStrLn $ "!!! ERROR: Recieved data not same as sent data"     


