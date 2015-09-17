{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.FileIO where

import Course.Core
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.List

{-

Useful Functions --

  getArgs :: IO (List Chars)
  putStrLn :: Chars -> IO ()
  readFile :: Chars -> IO Chars
  lines :: Chars -> List Chars
  void :: IO a -> IO ()

Abstractions --
  Applicative, Monad:

    <$>, <*>, >>=, =<<, pure

Problem --
  Given a single argument of a file name, read that file,
  each line of that file contains the name of another file,
  read the referenced file and print out its name and contents.

Example --
Given file files.txt, containing:
  a.txt
  b.txt
  c.txt

And a.txt, containing:
  the contents of a

And b.txt, containing:
  the contents of b

And c.txt, containing:
  the contents of c

$ runhaskell FileIO.hs "files.txt"
============ a.txt
the contents of a

============ b.txt
the contents of b

============ c.txt
the contents of c

-}

-- /Tip:/ use @getArgs@ and @run@
main ::
  IO ()
main =
--  error "todo: Course.FileIO#main"
  getArgs >>= \a -> case a of
--                      Nil -> error "todo: no CLAs"
--                      h :. _ -> error "todo: stuff"
--                      Nil -> putStrLn "forgot command line arguments"
                      h :. _ -> run h
                      _ -> putStrLn "forgot command line arguments"

--getArgs >>= \args ->
--    case args of
--      filename :. Nil -> run filename
--      _ -> putStrLn "usage: runhaskell io.hs filename"

type FilePath =
  Chars

-- /Tip:/ Use @getFiles@ and @printFiles@.
run ::
  Chars
  -> IO ()
--run =
--  error "todo: Course.FileIO#run"
--run filename =
run a = --putStrLn a
  --{-
  do
--    content <- readFile filename
    c <- readFile a
--    results <- getFiles (lines content)
    fs <- getFiles (lines c)
--    printFiles results
    printFiles fs
-- -}



getFiles ::
  List FilePath
  -> IO (List (FilePath, Chars))
--getFiles =
--  error "todo: Course.FileIO#getFiles"
--getFiles =
getFiles fs = 
  sequence (getFile <$> fs)
--  sequence . (<$>) getFile

getFile ::
  FilePath
  -> IO (FilePath, Chars)
--getFile =
  --error "todo: Course.FileIO#getFile"
--  lift2 (<$>) (,) readFile
getFile f =
  (\c -> (f, c)) <$> readFile f

printFiles ::
  List (FilePath, Chars)
  -> IO ()
printFiles =
--  error "todo: Course.FileIO#printFiles"
  void . sequence . (<$>) (uncurry printFile)

printFile ::
  FilePath
  -> Chars
  -> IO ()
--printFile =
--  error "todo: Course.FileIO#printFile"
--printFile name content =
--  putStrLn ("============ " ++ name) >>
--  putStrLn content
printFile p c =
  putStrLn ("The name of the file: " ++ p ++ "\n") *>
  putStrLn ("the contents: " ++ c)


