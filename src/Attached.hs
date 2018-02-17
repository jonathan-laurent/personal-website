--------------------------------------------------------------------------------
-- Attached.hs

-- Defines a Monad Transformer `AttachedT` that can be used to extend
-- Hakyll's `Compiler` monad so that it handles attached files.
--------------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Attached
  ( AttachedT
  , Attached (..)
  , runAttachedT

  , attachCmd
  , attachFile

  , withoutAttachments
  , withAttachedFiles
  , dropAttachedFiles

  , load'
  , loadAll'
  , loadAllSnapshots'
  , saveSnapshot'
  ) where


import Control.Monad.Trans.Class
import Control.Monad (forM_, unless)
import Control.Monad.State
import Control.Monad.Writer

import System.Process (readCreateProcess, shell)
import System.FilePath
import System.Directory

import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Typeable

import Hakyll

import qualified Config

--------------------------------------------------------------------------------

data ActionType =
    WriteFile String
    -- `Writefile content`
  | RunCmd String String deriving (Generic, Typeable)
    -- `RunCmd cmd stdin`
    -- `cmd` is the shell command to be run, with '%' in place of the
    --       name of the output

data Action = Action String ActionType deriving (Generic, Typeable)
-- `Action file action`

type ConcreteAction = String -> IO ()
-- Takes the name of the directory in which attached files should be put

--------------------------------------------------------------------------------

compileAction :: Action -> ConcreteAction
compileAction (Action file action) dir =
  case action of
    WriteFile toWrite -> write dest (Item (fromFilePath dest) toWrite)
    RunCmd cmd stdin ->
      void $ readCreateProcess (shell (replaceBy '%' dest cmd)) stdin
  where
    dest = dir </> file
    replaceBy srcChar destStr s =
      let aux c = if c == srcChar then destStr else [c]
      in concatMap aux s

newtype AttachedT m a = AttachedT (StateT (Set String) (WriterT [Action] m) a)
-- State:  set of generated files
-- Writer: actions to perform in order to generate these files

--------------------------------------------------------------------------------

deriving instance (Monad m) => Functor (AttachedT m)
deriving instance (Monad m) => Applicative (AttachedT m)
deriving instance (Monad m) => Monad (AttachedT m)
deriving instance (Monad m) => MonadState (Set String) (AttachedT m)
deriving instance (Monad m) => MonadWriter [Action] (AttachedT m)

instance Binary ActionType
instance Binary Action

instance MonadTrans AttachedT where
  lift = AttachedT . lift . lift


data Attached a = Attached [Action] a deriving (Generic)

deriving instance (Typeable a) => Typeable (Attached a)

instance (Binary a) => Binary (Attached a)

--------------------------------------------------------------------------------

dataDirPath :: FilePath -> String
dataDirPath path = dropExtension path ++ Config.attachmentDirSuffix

relPath :: FilePath -> FilePath
relPath p = "." </> takeFileName p

runAttachedT :: Monad m => AttachedT m a -> m (Attached a)
runAttachedT (AttachedT swx) = fromWriterOutput <$>
  runWriterT (fst <$> runStateT swx Set.empty)
  where fromWriterOutput (x, actions) = Attached actions x


instance (Writable a) => Writable (Attached a) where

  write file itemA = do

    let (Attached actions body) = itemBody itemA
    write file (itemSetBody body itemA)

    unless (null actions) $ do
      let dir = dataDirPath file
      exists <- doesDirectoryExist dir
      unless exists $ createDirectory dir
      forM_ (compileAction <$> actions) ($dir)


makeFresh :: String -> Set String -> String
makeFresh name used =
  if not (name `Set.member` used) then name
  else
    let base = dropExtension name
        ext  = takeExtension name in
    makeFresh (base ++ "-bis" <.> ext) used


-- Argument of the second argument: file that should be written
attach :: String -> ActionType -> AttachedT Compiler String
attach fileName act = do
  used <- get
  let fresh = makeFresh fileName used
  put (Set.insert fresh used)
  tell [Action fresh act]
  ((</> fresh) . relPath . dataDirPath) <$> lift getResourceFilePath


attachFile :: String -> String -> AttachedT Compiler String
attachFile file x = attach file (WriteFile x)

attachCmd :: String -> String -> String -> AttachedT Compiler String
attachCmd file cmd stdin = attach file (RunCmd cmd stdin)

--------------------------------------------------------------------------------
-- Boilerplate to integrate AttachedT with the rest of the Hakyll pipeline

permuteAttachedItem :: Attached (Item a) -> Item (Attached a)
permuteAttachedItem (Attached act it) =
  itemSetBody (Attached act (itemBody it)) it

_permuteItemAttached :: Item (Attached a) -> Attached (Item a)
_permuteItemAttached it =
  let Attached actions x = itemBody it in
  Attached actions (itemSetBody x it)

withAttachedFiles :: AttachedT Compiler (Item a) -> Compiler (Item (Attached a))
withAttachedFiles = fmap permuteAttachedItem . runAttachedT

dropAttachedFiles :: AttachedT Compiler (Item a) -> Compiler (Item a)
dropAttachedFiles = withoutAttachment . withAttachedFiles

-- (Binary a, Typeable a) => Snapshot -> Item a -> Compiler (Item a)
saveSnapshot' :: (Binary a, Typeable a, MonadTrans t) =>
                  Snapshot -> Item a -> t Compiler (Item (Attached a))
saveSnapshot' f it = lift (saveSnapshot f (Attached [] <$> it))

dumpAttached :: Attached a -> a
dumpAttached (Attached _ x) = x

withoutAttachment :: Compiler (Item (Attached a)) -> Compiler (Item a)
withoutAttachment = fmap (fmap dumpAttached)

withoutAttachments :: Compiler [Item (Attached a)] -> Compiler [Item a]
withoutAttachments = fmap (fmap (fmap dumpAttached))

-- These functions have the same signature than their unprimed versions.
-- They can be used to load some resources without their attachments.

load' :: (Binary a, Typeable a) => Identifier -> Compiler (Item a)
load' f = withoutAttachment (load f)

loadAll' :: (Binary a, Typeable a) => Pattern -> Compiler [Item a]
loadAll' f = withoutAttachments (loadAll f)

loadAllSnapshots' ::
  (Binary a, Typeable a) => Pattern -> Snapshot -> Compiler [Item a]
loadAllSnapshots' f s = withoutAttachments (loadAllSnapshots f s)

--------------------------------------------------------------------------------