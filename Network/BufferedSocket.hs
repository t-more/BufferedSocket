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

module Network.BufferedSocket (module Network.BufferedSocket.Core , module Network.BufferedSocket.Reader, module Network.BufferedSocket.Writer) where

import Network.BufferedSocket.Core 
import Network.BufferedSocket.Reader
import Network.BufferedSocket.Writer