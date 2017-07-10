{-# LANGUAGE CPP #-}

module Main where

#ifdef SONG
song = 10
#else
song = 20
#endif

main = print song
