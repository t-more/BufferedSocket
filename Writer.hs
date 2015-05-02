{-----------------------------------------------------------------------------------------
Module name: BufferedSocketReader
Made by:     Tomas Möre 2015
 

Usage:
    This module is mean to be imported qualified 
    This is an addition to BufferedSocket
    ex: import Headers qualified BS

Notes:    
    This reader modules purpose is to make it easto to read diffrent kinds of data types. This is of course only if the socket isn't encrypted.

WARNINGS:
------------------------------------------------------------------------------------------}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Writer
( sendText
, sendTextLazy
, sendString
, sendWord8
, sendWord16
, sendWord32
, sendWord64
, sendInt8
, sendInt16
, sendInt32
, sendInt64
, Sendable
, send 
  )
where 

import Prelude hiding (getLine, read)



import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Builder   as BB 
import qualified Data.ByteString.Char8     as BC8 
import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 

import qualified Data.Text.Encoding as ENC 
import qualified Data.Text.Lazy.Encoding as ENCL


import Control.Monad
import Data.Functor
import Data.Monoid
import Data.Word 
import Data.String
import Data.Int 
import Data.Bits

import Foreign.Storable

import qualified Core as BS 

bitsToByteStringReal:: (Bits a, Integral a) => a -> Int -> [Word8]
bitsToByteStringReal a 0 = [fromIntegral a]
bitsToByteStringReal inData size = 
    let thisByte = shift inData (- (size * 8 ))
    in (fromIntegral thisByte : bitsToByteStringReal inData (size - 1))

bitsToByteString:: (Bits a, Integral a, Storable a) => a -> B.ByteString
bitsToByteString inData = B.pack $ bitsToByteStringReal inData $ sizeOf inData - 1


sendWord8:: BS.BufferedSocket -> Word8 -> IO ()
sendWord8 bSocket byte = BS.sendByteString bSocket $ B.pack [byte] 

sendByte :: BS.BufferedSocket -> Word8 -> IO ()
sendByte = sendWord8

sendWord16:: BS.BufferedSocket -> Word16 -> IO ()
sendWord16 bSocket word16 = let sendData = bitsToByteString word16
                            in do 
                                putStrLn $ "sent bytes: " ++ (show $ B.unpack sendData)
                                BS.sendByteString bSocket $ sendData


sendWord32:: BS.BufferedSocket -> Word32 -> IO ()
sendWord32 bSocket word32 =  BS.sendByteString bSocket $ bitsToByteString word32

sendWord64:: BS.BufferedSocket -> Word64 -> IO ()
sendWord64 bSocket word64 =  BS.sendByteString bSocket $ bitsToByteString word64
sendInt8:: BS.BufferedSocket -> Int8 -> IO ()
sendInt8 bSocket int8 =  sendWord8 bSocket $ fromIntegral int8
sendInt16:: BS.BufferedSocket -> Int16 -> IO ()
sendInt16 bSocket int16 = sendWord16 bSocket $ (fromIntegral int16 :: Word16)
sendInt32:: BS.BufferedSocket -> Int32 -> IO ()
sendInt32 bSocket int32 = sendWord32 bSocket $ fromIntegral int32
sendInt64:: BS.BufferedSocket -> Int64 -> IO ()
sendInt64 bSocket int64 = sendWord64 bSocket $ fromIntegral int64

sendLazyReal :: BS.BufferedSocket -> [B.ByteString] -> IO () 
sendLazyReal _ [] = return ()
sendLazyReal bSocket (x:xs) = BS.sendByteString bSocket x >> sendLazyReal bSocket xs

sendLazy :: BS.BufferedSocket -> BL.ByteString -> IO ()
sendLazy bSocket lazyBytestring = sendLazyReal bSocket $ BL.toChunks lazyBytestring


sendText:: BS.BufferedSocket -> T.Text -> IO ()
sendText bSocket textData = BS.sendByteString bSocket $ ENC.encodeUtf8 textData

sendTextLazy:: BS.BufferedSocket -> TL.Text -> IO ()
sendTextLazy bSocket lazyText = sendLazy bSocket $ ENCL.encodeUtf8 lazyText

sendString:: BS.BufferedSocket -> String -> IO ()
sendString bSocket string = sendLazy bSocket $ fromString string



class Sendable s where
    send :: (BS.BufferedSocket -> s -> IO ())        

instance Sendable Word8  where 
    send = sendWord8
instance Sendable Word16  where 
    send = sendWord16
instance Sendable Word32  where 
    send = sendWord32
instance Sendable Word64  where 
    send = sendWord64

instance Sendable Int8  where 
    send = sendInt8
instance Sendable Int16  where 
    send = sendInt16
instance Sendable Int32  where 
    send = sendInt32
instance Sendable Int64  where 
    send = sendInt64 

instance Sendable B.ByteString  where 
    send = BS.sendByteString
instance Sendable BL.ByteString  where 
    send = sendLazy
{-- 
instance Sendable T.Text  where 
    send = sendText
instance Sendable TL.Text  where 
    send = sendTextLazy
instance Sendable String  where 
    send = sendString
--}
