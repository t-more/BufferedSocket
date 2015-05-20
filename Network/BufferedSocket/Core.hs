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

module Network.BufferedSocket.Core
(
  readRaw
, readLazy
, readByte
, readToByte
, readToByteMax
, sendByteString
, readToByteStringMax
, flush
, BufferedSocket 
, makeBufferedSocket
, MaxLength
, ReadSize

, inBuffer
, waitForRead
, isReadable
, isWriteable
, closeRead
, closeWrite
, nativeSocket
  )
where 

import Prelude hiding (getLine, read)



import Control.Monad
import Control.Applicative
import qualified Data.ByteString           as B
import qualified Data.ByteString.Internal  as BI
import qualified Data.ByteString.Lazy      as BL
import qualified Data.ByteString.Builder   as BB 
import qualified Data.ByteString.Char8     as BC8 
import qualified Data.Text as T 
import qualified Data.Text.Lazy as TL 

import Data.List
import Data.IORef
import Data.Functor
import Data.Maybe (isJust)

import qualified Network.Socket as NS -- ns for native socket  
import Foreign.Storable
import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe -- I'm so sorry for this
import System.Posix.Types
import System.Timeout

import Foreign.ForeignPtr
import Data.Monoid
import Data.Word 
import Data.String
import qualified Data.Text.Encoding as ENCSTRICT 
import qualified Data.Text.Lazy.Encoding as ENCLAZY

import GHC.Conc.IO

type MaxLength        = Int

type BufferSize       = Int

type InputBufferSize  = BufferSize
type OutputBufferSize = BufferSize

type BytesInBuffer   = IORef Int 
type ByteOffset      = IORef Int 

type RemainingBytes  = Int   

type ReadSize         = Int 

type Read = Int  

type Timeout = Int 

type SocketData = (NS.Socket,NS.SockAddr,Fd)

type ByteString = B.ByteString
-- Since newtype is disregarded at compile time we make these newtypes to make sure no errors occur 

-- The output buffer is simply a buffer that attemps to store a certain ammount of data before we send the package.
-- This is to make minimalize the TCP overhead. Some operating systems might attemp to do the same. 
type OutputBuffer = (ForeignPtr Word8, BytesInBuffer,BufferSize)
type InputBuffer  = (ForeignPtr Word8, ByteOffset,BytesInBuffer,BufferSize)

newtype BufferedSocket = BufferedSocket (SocketData, InputBuffer, OutputBuffer)

{-
UTIL 
-}


nativeSocket:: BufferedSocket -> NS.Socket
nativeSocket (BufferedSocket ((socket,_,_),_,_))  = socket 

isReadable:: BufferedSocket -> IO Bool 
isReadable (BufferedSocket ((socket,_,_),_,_)) = NS.sIsReadable socket

isWriteable:: BufferedSocket -> IO Bool 
isWriteable (BufferedSocket ((socket,_,_),_,_)) = NS.isWritable socket

closeRead :: BufferedSocket -> IO ()
closeRead (BufferedSocket ((socket,_,_),_,_)) = NS.shutdown socket NS.ShutdownReceive

closeWrite :: BufferedSocket -> IO ()
closeWrite (BufferedSocket ((socket,_,_),_,_)) = NS.shutdown socket NS.ShutdownSend
-- INPUT BUFFER 
inBuffer   :: BufferedSocket -> InputBuffer
inBuffer (BufferedSocket (_,inBuf,_)) = inBuf 

inBufferClear :: InputBuffer -> IO () 
inBufferClear (_, offsetRef, bytesRef, _) = writeIORef offsetRef 0 >> writeIORef bytesRef 0

inBufferReadAll :: InputBuffer -> IO ByteString
inBufferReadAll inBuf@(buf, offsetRef, bytesRef, _) =
    do
        offset <- readIORef offsetRef 
        bytesN <- readIORef bytesRef
        let unreadBytes = bytesN - offset
        if bytesN == 0
            then return "" 
            else (inBufferClear inBuf) >> (return $ BI.fromForeignPtr buf offset unreadBytes)


-- moves the bytes in the buffer to the start 
inBufferRealign :: InputBuffer -> IO ()
inBufferRealign (buf, offsetRef, bytesRef, _) = 
    withForeignPtr buf  $ \ptr -> 
        do  putStrLn "realigned"
            offset <- readIORef offsetRef
            when (offset > 0) $
               do   bytesN <- readIORef bytesRef
                    let unreadBytes = bytesN - offset
                        offsetPtr   = plusPtr  ptr offset
                    BI.memcpy   ptr offsetPtr unreadBytes
                    writeIORef offsetRef 0 

inBufferFindByteReal :: Ptr Word8 -> Int -> Word8 -> IO (Maybe Int)
inBufferFindByteReal _ 0 _ = return Nothing 
inBufferFindByteReal ptr bytesLeft matchByte = 
    do  currentChar <- peek ptr 
        if isMatch currentChar  
            then return $ Just bytesLeft
            else inBufferFindByteReal (plusPtr ptr 1) (bytesLeft - 1) matchByte
    where 
        isMatch = (==matchByte)


inBufferFindByte :: InputBuffer -> Word8 -> IO (Maybe Int)
inBufferFindByte (buf, offsetRef, bytesRef, _) byte =
    do  offset <- readIORef offsetRef 
        bytesN <- readIORef bytesRef 
        let unreadBytes = bytesN - offset

        withForeignPtr buf $ 
            \ ptr ->    do  let startPtr = plusPtr ptr offset
                            maybeBytesLeft <- inBufferFindByteReal startPtr unreadBytes byte
                            case maybeBytesLeft of 
                                Just n  -> return $ Just  (unreadBytes - n) 
                                Nothing -> return Nothing  

        
-- OUTPUT BUFFER 
outBuffer  :: BufferedSocket -> OutputBuffer  
outBuffer (BufferedSocket (_,_,outBuf)) = outBuf


-- SOCKET DATA 
socketData :: BufferedSocket -> SocketData
socketData (BufferedSocket (sockData,_,_)) = sockData


-- Reads up to max the available bytes of data
bSocketRecv :: BufferedSocket -> IO Int 
bSocketRecv (BufferedSocket ((sock,_,_),(buf,offsetRef,bytesNRef,bufSize),_)) = 
    withForeignPtr buf $ \ptr -> 
        do  bytesN <- readIORef bytesNRef 
            let offsetBuf  = plusPtr ptr bytesN
                maxRead    = bufSize - bytesN
            if maxRead > 0 
                then 
                    do 
                        (recievedBytes, _) <- NS.recvBufFrom sock offsetBuf maxRead
                        writeIORef bytesNRef (bytesN + recievedBytes)
                        return recievedBytes
                else 
                    return 0 

-- Unsafe. Do not use if you don't know what you're doing 
bSocketRecvMin :: BufferedSocket -> Int -> IO ()
bSocketRecvMin bSocket n = 
    do  bytesRead <- bSocketRecv bSocket 
        when (bytesRead < n) $ bSocketRecvMin bSocket (n - bytesRead)

waitForRead :: BufferedSocket -> Timeout -> IO Bool 
waitForRead (BufferedSocket ((sock,_,fideDesc),_,_)) timeoutDuration =
    do maybeSucess <- timeout timeoutDuration (threadWaitRead  fideDesc)
       if isJust maybeSucess 
        then return True 
        else return False

{-
CREATING A SOCKET

This puts together a new "BufferedSocket". or commonly refered as "bSock"
A buffered socket is a socket which is acommodated by a Ptr. Because of the need to split headers etc up at certain points but still
safet he rest of the data to be whatever the server requires it to be.

A buffered socket should be called with freeBSocket before disregarded. 
The server thunk function does so automaticly.

-}
--BufferedSocket ((sock, sockAddr, socketFileDesc), inputBuffer, outPutBuffer)
makeBufferedSocket :: (NS.Socket, NS.SockAddr) -> InputBufferSize -> OutputBufferSize -> IO BufferedSocket
makeBufferedSocket (sock, sockAddr)  inBufferSize outBufferSize = 
    do
      inputBuffer      <- makeInputBuffer inBufferSize
      outPutBuffer     <- makeOutputBuffer outBufferSize
      let socketFileDesc = NS.fdSocket sock
      return $ BufferedSocket ((sock, sockAddr, Fd socketFileDesc), inputBuffer, outPutBuffer)

makeInputBuffer :: InputBufferSize -> IO InputBuffer
makeInputBuffer bufferSize =
    do  offset      <- newIORef 0 
        bytesCount  <- newIORef 0
        buffer      <- BI.mallocByteString bufferSize
        return (buffer, offset, bytesCount, bufferSize)

makeOutputBuffer :: OutputBufferSize -> IO OutputBuffer
makeOutputBuffer bufferSize = 
    do  bytesCount  <- newIORef 0
        buffer      <- BI.mallocByteString bufferSize
        return (buffer, bytesCount, bufferSize)

{-
USING THE BUFFERED SOCKETS:
-}

{- Strict reading 
If the buffer contains the required ammount of bytes it will simply read it.
If not it will read all the bytes. Clear the buffer and attempt to read more 

-} 
readRaw :: BufferedSocket -> Int -> IO ByteString
readRaw _ 0 = return ""
readRaw bSock@(BufferedSocket ((sock,_,_),inBuf@(buf,offsetRef,bytesNRef,bufSize),_)) readSize = 
    do  bytesN <- readIORef bytesNRef
        offset <- readIORef offsetRef

        let unreadBytes               = bytesN - offset
            availableBytesAfterBytesN = bufSize - bytesN
            availableBytesTotal       = availableBytesAfterBytesN + offset

            missingBytes              = readSize - unreadBytes

        if missingBytes <= 0 
        then writeIORef offsetRef (offset +  readSize) >> (return $ BI.fromForeignPtr buf offset readSize)
        else if missingBytes <= availableBytesAfterBytesN 
        then bSocketRecvMin bSock missingBytes >> 
                loop
        else if missingBytes <=  availableBytesTotal
        then inBufferRealign inBuf >> 
                fillBuffer availableBytesTotal >>
                    loop
        else do  
            let strFragment =  if unreadBytes > 0 
                                then BI.fromForeignPtr buf offset unreadBytes
                                else ""
            inBufferClear inBuf 
            (strFragment<>) <$> readRaw  bSock (readSize - B.length strFragment)
    where 
        fillBuffer  = bSocketRecvMin bSock
        loop        = readRaw bSock readSize

-- 
lazyReader :: BufferedSocket -> [Int] -> IO [ByteString]
lazyReader _ [] = return []
lazyReader bSock ~(chunkSize:rest) = 
    do  chunk <- unsafeInterleaveIO $ readRaw bSock chunkSize
        next  <- unsafeInterleaveIO $ lazyReader bSock rest 
        return (chunk:next)

-- readLazy will read will read in chunks of the same size of the buffered socket 
readLazy :: BufferedSocket -> Int -> IO BL.ByteString
readLazy _ 0 = return ""
readLazy bSock@(BufferedSocket ((sock,_,_),inBuf@(buf,offsetRef,bytesNRef,bufSize),_)) readSize = 
    let chunkSizes = unfoldr (\b -> if b == 0 
                                    then Nothing 
                                    else if b > bufSize 
                                        then Just (bufSize, b - bufSize) 
                                        else Just (b,0)) readSize
    in  BL.fromChunks <$> lazyReader bSock chunkSizes


readByte :: BufferedSocket -> IO Word8
readByte bSock@(BufferedSocket ((sock,_,_),inBuf@(buf,offsetRef,bytesNRef,bufSize),_)) =
    do  bytesN <- readIORef bytesNRef 
        offset <- readIORef offsetRef
        let unreadBytes = bytesN - offset

        if unreadBytes > 0 
            then do 
                writeIORef offsetRef (offset + 1)
                withForeignPtr buf (`peekByteOff` offset)
            else do 
                inBufferClear inBuf 
                bSocketRecvMin bSock 1 
                readByte bSock

readToByte :: BufferedSocket -> Word8 -> IO ByteString
readToByte bSock@(BufferedSocket ((sock,_,_),inBuf@(buf,offsetRef,bytesNRef,bufSize),_)) byte = 
    do  maybeByteIndex <- inBufferFindByte inBuf byte
        case maybeByteIndex of 
            Nothing -> (<>) <$> inBufferReadAll inBuf <*> readToByte bSock byte --  >>= (<>) fmap (<>(readToByte bSock byte)) $ inBufferReadAll inBuf
            Just n  -> do 
                        offset <- readIORef offsetRef 
                        writeIORef offsetRef (offset + n + 1)
                        return $ BI.fromForeignPtr buf offset n

readToByteMax :: BufferedSocket -> Word8 -> MaxLength -> IO (Maybe ByteString)
readToByteMax bSock@(BufferedSocket ((sock,_,_),inBuf@(buf,offsetRef,bytesNRef,bufSize),_)) byte maxLen 
    | hasNoMaxLen = return Nothing 
    | otherwise = 
        do  maybeByteIndex <- inBufferFindByte inBuf byte
            bytesN         <- readIORef bytesNRef
            offset         <- readIORef offsetRef 

            let unreadBytes = bytesN - offset

            case maybeByteIndex of
                Nothing ->  if unreadBytes >= maxLen
                            then return Nothing  
                            else do let maxBytesLeft = maxLen - unreadBytes
                                    thisBufData <- inBufferReadAll inBuf -- empties the current buffer data
                                    bSocketRecv bSock                    -- attempts to rebuffer the socket 
                                    maybeData   <- readToByteMax bSock byte maxBytesLeft -- repeat 
                                    case maybeData of 
                                        Nothing -> return Nothing 
                                        Just a  -> return $ Just (thisBufData <> a)
                Just n -> do writeIORef offsetRef (offset + n + 1)
                             return $ Just $ BI.fromForeignPtr buf offset n 
    where 
        hasNoMaxLen = maxLen <= 0

-- Dangerous will read forever if nothing is found 
-- You will most probably want to use the readToByteStringMax
readToByteString :: BufferedSocket -> ByteString -> IO ByteString
readToByteString _ "" = error "readToByteString can not take an empty bytestring" 
readToByteString bSock searchString =
    do  dataString <- readToByte bSock firstByte
        trail      <- readRaw bSock $ B.length restSearchString
        if trail == restSearchString
        then return dataString
        else ((dataString<>trail)<>) <$> readToByteString bSock searchString
    where 
        firstByte        = B.head searchString 
        restSearchString = B.tail searchString

-- Same as above but Will read to a max length. If this is reached It returns "Nothing"
readToByteStringMax :: BufferedSocket -> ByteString -> MaxLength -> IO (Maybe ByteString)
readToByteStringMax _ _ 0  = return Nothing
readToByteStringMax _ "" _ = error "readToByteString max got an empty string"
readToByteStringMax bSock searchString maxLength = 
    do  maybeDataString <- readToByteMax bSock firstByte maxLength
        case maybeDataString of 
            Nothing         -> return Nothing 
            Just dataString -> do   trail <- readRaw bSock $ B.length restSearchString
                                    if trail == restSearchString
                                    then return $ Just dataString
                                    else do maybeMoreString <- readToByteStringMax bSock searchString (maxLength - B.length dataString - B.length trail)
                                            case maybeMoreString of 
                                                Nothing -> return Nothing 
                                                Just a  -> return $ Just $ dataString <> trail <> a
    where 
        firstByte        = B.head searchString 
        restSearchString = B.tail searchString


-- Simple and clean. force pushed the data to the actual socket. Afther this 
flush :: BufferedSocket -> IO()
flush (BufferedSocket ((socket,sockAddr,_),_,(outBuf, bytesNRef, bufferSize))) =
    do  bytesInBuffer <- readIORef bytesNRef
        when (bytesInBuffer > 0) $
            do  let sender ptr bytes = 
                        do  sent <- NS.sendBufTo socket ptr bytes sockAddr
                            when (sent < bytes) $ sender (plusPtr ptr sent) (bytes - sent)

                    str = BI.fromForeignPtr outBuf 0 bytesInBuffer
                withForeignPtr outBuf (`sender` bytesInBuffer)
                writeIORef bytesNRef 0
    

-- if the string sent to this function is less or equal to the ammount of bytes left in the buffer 
-- we will simply fill the buffer 
-- (if equal) we will also flush this buffer.
-- If the string is greater then the ammount of bytes left 
-- we will attempt to fill the last bytes in the buffer then 
sendByteString:: BufferedSocket -> ByteString -> IO ()
sendByteString _ "" = return ()
sendByteString bSock@(BufferedSocket ((sock,sockAddr,_),_,(outBuf, bytesNRef, bufferSize))) outputStr = 
    do  bytesInBuffer <- readIORef bytesNRef
        let bytesLeftInBuffer = bufferSize - bytesInBuffer

        if srcLength <= bytesLeftInBuffer
            then do
                withForeignPtr outBuf $ \bufPtr -> 
                    withForeignPtr sourceFrgnPtr $ \srcPtr -> 
                        BI.memcpy (plusPtr bufPtr bytesInBuffer) (plusPtr srcPtr srcOffet) srcLength
                          
                writeIORef bytesNRef (bytesInBuffer + srcLength)
                when (srcLength + bytesInBuffer == bufferSize) $ flush bSock
            else do 

                let (current,rest)  = B.splitAt bytesLeftInBuffer outputStr
                    overflowRest    = div (B.length rest) bufferSize
                    directSendChunk = B.take overflowRest rest 
                    directSendLen   = B.length directSendChunk
                    (directSend,_,_)= BI.toForeignPtr directSendChunk

                    toBufferPart    = B.drop overflowRest rest

                sendByteString bSock current 
                when (directSendChunk /= "") $ withForeignPtr sourceFrgnPtr $ \ptr -> void $ NS.sendBufTo sock (plusPtr ptr srcOffet) directSendLen sockAddr
                sendByteString bSock toBufferPart

    where 
        (sourceFrgnPtr, srcOffet, srcLength) = BI.toForeignPtr outputStr

               


