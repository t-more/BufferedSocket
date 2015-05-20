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
    Nah Nothing 
------------------------------------------------------------------------------------------}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.BufferedSocket.Reader
( readText
, readTextLazy
, readNativeString
, readWord8
, readWord16
, readWord32
, readWord64
, readInt8
, readInt16
, readInt32
, readInt64
, read
, Readable
, readString
, ReadableString
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

import qualified Data.Text.Encoding.Error as ENCERROR
import Data.Maybe (isJust)

import Control.Monad

import Data.Functor 

import Data.Monoid
import Data.Word 
import Data.String
import Data.Int 

import qualified Network.BufferedSocket.Core as BS 

import Data.Bits 

listToValue :: (Bits a, Num a) => [a] -> a 
listToValue = foldl xor zeroBits


convertToShiftedList:: (Bits b, Num b) => [Word8] -> Int -> [b]
convertToShiftedList [] _ = []
convertToShiftedList (x:xs) byteIndex = 
    let n = byteIndex - 1 -- Since we want the size of the result data the actual index -||- is (-1)
    in (shift (fromIntegral x) (8 * n): convertToShiftedList xs n) 


readText:: BS.BufferedSocket -> BS.ReadSize -> IO T.Text
readText bSocket readSize = do
    byteData <- BS.readRaw bSocket readSize
    return $ ENC.decodeUtf8With (ENCERROR.replace '\xfffd') byteData

readTextLazy:: BS.BufferedSocket -> BS.ReadSize -> IO TL.Text
readTextLazy bSocket readSize = do
    byteData <- BS.readLazy bSocket readSize
    return $ ENCL.decodeUtf8With (ENCERROR.replace '\xfffd') byteData

readNativeString:: BS.BufferedSocket -> BS.ReadSize -> IO String 
readNativeString bSock readSize = do 
    byteString <-  readText bSock readSize
    return $ T.unpack byteString 

readWord8:: BS.BufferedSocket -> IO Word8
readWord8 = BS.readByte

readWord16:: BS.BufferedSocket -> IO Word16
readWord16 bSocket = do 
    bytes <- B.unpack <$> BS.readRaw bSocket byteSize
    let result =  convertToShiftedList bytes byteSize:: [Word16]
    return $ listToValue result

    where 
        byteSize = 2

readWord32:: BS.BufferedSocket -> IO Word32
readWord32 bSocket = do 
    bytes <- B.unpack <$> BS.readRaw bSocket byteSize
    let result =  convertToShiftedList bytes byteSize:: [Word32]
    return $ listToValue result

    where 
        byteSize = 4

readWord64:: BS.BufferedSocket -> IO Word64
readWord64 bSocket = do 
    bytes <- B.unpack <$> BS.readRaw bSocket byteSize
    let result =  convertToShiftedList bytes byteSize:: [Word64]
    return $ listToValue result

    where 
        byteSize = 8
readInt8:: BS.BufferedSocket -> IO Int8
readInt8 =  (fromIntegral <$>) . readWord8

readInt16:: BS.BufferedSocket -> IO Int16
readInt16 =  (fromIntegral <$>) . readWord16

readInt32:: BS.BufferedSocket -> IO Int32
readInt32 =  (fromIntegral <$>) . readWord32

readInt64:: BS.BufferedSocket -> IO Int64
readInt64 =  (fromIntegral <$>) . readWord64


class Readable r where 
    read :: (BS.BufferedSocket -> IO r)

instance Readable Word8  where 
    read = readWord8
instance Readable Word16  where 
    read = readWord16
instance Readable Word32  where 
    read = readWord32
instance Readable Word64  where 
    read = readWord64

instance Readable Int8  where 
    read = readInt8
instance Readable Int16  where 
    read = readInt16
instance Readable Int32  where 
    read = readInt32
instance Readable Int64  where 
    read = readInt64

class ReadableString s where 
    readString :: (BS.BufferedSocket -> Int -> IO s)

instance ReadableString B.ByteString  where 
    readString = BS.readRaw
instance ReadableString BL.ByteString  where 
    readString = BS.readLazy
{-- 
instance ReadableString T.Text  where 
    readString = readText
instance ReadableString TL.Text  where 
    readString = readTextLazy
instance ReadableString String where 
    readString = readNativeString
--}
