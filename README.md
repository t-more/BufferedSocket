### BufferedSocket
##### A wrapper for network sockets

This package is made to make reading and writing from network sockets easy in haskell.

To make a socket you need three things:

1. A socket made from the Network package (BufferedSockets does not meddle with your configuration so you have to do all this yourself)
2. Decide output buffer size 

3. Decide input buffer size

4. (party like it's 1969) 

This is done with the following function:
```haskell
makeBufferedSocket :: (Socket, SockAddr) -> InputBufferSize -> OutputBufferSize -> IO BufferedSocket
```


###   The main functions for read and write:

#####  Reading:
```haskell
read :: BS.BufferedSocket -> IO r
```
The standard read is made to ready any basic data values.
This is amongst Word8 to Word64 and Int8 to Int64

```haskell
readString :: BS.BufferedSocket -> Int -> IO s
```
The readString function can read both lazy and strict bytestrings. 
But you need to provide the number of bytes to read.

To read any other kind of data is is reccomended to still use these functions. And then use decoding methods to get the data format you want. 


```haskell
readToByte :: BufferedSocket -> Word8 -> IO ByteString
readToByteMax :: BufferedSocket -> Word8 -> MaxLength -> IO (Maybe ByteString)

readToByteString :: BufferedSocket -> ByteString -> IO ByteString
readToByteStringMax :: BufferedSocket -> ByteString -> MaxLength -> IO (Maybe ByteString)
```
The above functions are made to scan the input buffer for specific bytes. All of them of course reads more data from the network if necesarry 
However it is recommended to use the "Max" verions of the functions as they put a limit to how much data that may be read.


#####  Writing:
```haskell
send :: BS.BufferedSocket -> s -> IO ()
```
Sends any basic data type and both lazy and strict ByteStrings
If you wish to send any other kind of data use encoding methods for the data type.

```haskell
flush :: BufferedSocket -> IO ()
```
Data is not sent to the network unless:
1. The writing buffer is full
2. Flush is called


###  Buffer functionalities 
##### Input
The buffers are made to work is a fairly standard way. 
Every time we read from the socket the socket first checks the buffer if data is available. If it is it will just aquire the data from the buffer. 
If not then it will access the network and attempt to read for the ammount of bytes that are available in the buffer.
If not enthough space is available the first step is to see if clearing the offset will grant enoguh space. If not it will allow the  data to be saved elsewhere.


##### Output
Is much simpler then Input. Simply puts the bytes together before sending. The actaul sending of data will only happend once the buffer is full OR the flush function is called



###  Example code:
here under is some example code. Warning this is pretty pointless code!!! 

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Network.Socket as NS -- NS for "native socket"
import qualified BufferedSocket as BS 
import qualified Data.ByteString as B

testServerPort = 1337
testHost = "localhost"
testServerMaxConnections = 1


bufferSize = 1024 * 10

-- This is an example how to make a TCP server in haskell 
-- Keep in mind that this is a pretty "minimalistic" server 

makeTestTcpServer:: (BS.BufferedSocket -> IO ())  -> IO ()
makeTestTcpServer thunk  = NS.withSocketsDo $ do 
    -- creates a tcp socket
    serverSock <- NS.socket NS.AF_INET NS.Stream 0
    -- Binds the socket to serve at address 
    NS.bindSocket serverSock (NS.SockAddrInet testServerPort NS.iNADDR_ANY)

    NS.listen serverSock testServerMaxConnections
    socketData@(subSock,subSockaddr) <- NS.accept serverSock

    serverBSock <- BS.makeBufferedSocket socketData bufferSize bufferSize

    thunk serverBSock

    NS.sClose serverSock

main = makeTestTcpServer $ \bSock -> 
            BS.send bSocket ("Hello world!" :: B.ByteString) 


```
