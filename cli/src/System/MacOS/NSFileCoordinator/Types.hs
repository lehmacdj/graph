{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module System.MacOS.NSFileCoordinator.Types
  ( foundationCtx,

    -- * Helpers
    Objc (castToId, castFromId),

    -- * Opaque types for use with pointers
    NSError,
    NSURL,
    NSFileCoordinator,
    NSArray,
    NSString,
    NSErrorDomain,
    NSInteger,
    NSUInteger,

    -- * Reading/writing options
    NSFileCoordinatorReadingOptions,
    k_NSFileCoordinatorReadingWithoutChanges,
    k_NSFileCoordinatorReadingResolvesSymbolicLink,
    k_NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly,
    k_NSFileCoordinatorReadingForUploading,
    NSFileCoordinatorWritingOptions,
    k_NSFileCoordinatorWritingForDeleting,
    k_NSFileCoordinatorWritingForMoving,
    k_NSFileCoordinatorWritingForMerging,
    k_NSFileCoordinatorWritingForReplacing,
    k_NSFileCoordinatorWritingContentIndependentMetadataOnly,
  )
where

import Foreign
import Language.C.Inline
import Language.C.Inline.Context
import qualified Language.C.Inline.ObjC as C
import qualified Language.C.Types as CT
import qualified Language.Haskell.TH.Syntax as TH
import MyPrelude

-- | Marker typeclass for Objective-C types
class Objc a where
  castToId :: Ptr a -> C.Id
  castToId = C.Id . castPtr

  castFromId :: C.Id -> Ptr a
  castFromId (C.Id ptr) = castPtr ptr

data NSError deriving (Objc)

data NSURL deriving (Objc)

data NSFileCoordinator deriving (Objc)

data NSArray deriving (Objc)

data NSString deriving (Objc)

type NSErrorDomain = NSString

type NSInteger = CLong

type NSUInteger = CULong

type NSFileCoordinatorReadingOptions = NSUInteger

k_NSFileCoordinatorReadingWithoutChanges, k_NSFileCoordinatorReadingResolvesSymbolicLink, k_NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly, k_NSFileCoordinatorReadingForUploading :: NSFileCoordinatorReadingOptions
k_NSFileCoordinatorReadingWithoutChanges = shift 1 0
k_NSFileCoordinatorReadingResolvesSymbolicLink = shift 1 1
k_NSFileCoordinatorReadingImmediatelyAvailableMetadataOnly = shift 1 2
k_NSFileCoordinatorReadingForUploading = shift 1 3

type NSFileCoordinatorWritingOptions = NSUInteger

k_NSFileCoordinatorWritingForDeleting, k_NSFileCoordinatorWritingForMoving, k_NSFileCoordinatorWritingForMerging, k_NSFileCoordinatorWritingForReplacing, k_NSFileCoordinatorWritingContentIndependentMetadataOnly :: NSFileCoordinatorWritingOptions
k_NSFileCoordinatorWritingForDeleting = shift 1 0
k_NSFileCoordinatorWritingForMoving = shift 1 1
k_NSFileCoordinatorWritingForMerging = shift 1 2
k_NSFileCoordinatorWritingForReplacing = shift 1 3
k_NSFileCoordinatorWritingContentIndependentMetadataOnly = shift 1 4

-- | Language.Inline.C.Context that allows using these types in inline-c
foundationCtx :: Context
foundationCtx =
  mempty
    { ctxForeignSrcLang = Just TH.LangObjc,
      ctxTypesTable =
        mapFromList
          [ (CT.TypeName "NSURL", [t|NSURL|]),
            (CT.TypeName "NSFileCoordinator", [t|NSFileCoordinator|]),
            (CT.TypeName "NSError", [t|NSError|]),
            (CT.TypeName "NSArray", [t|NSArray|]),
            (CT.TypeName "NSString", [t|NSString|]),
            (CT.TypeName "NSErrorDomain", [t|NSErrorDomain|]),
            (CT.TypeName "NSInteger", [t|NSInteger|]),
            (CT.TypeName "NSUInteger", [t|NSUInteger|]),
            (CT.TypeName "NSFileCoordinatorReadingOptions", [t|NSFileCoordinatorReadingOptions|]),
            (CT.TypeName "NSFileCoordinatorWritingOptions", [t|NSFileCoordinatorWritingOptions|])
          ]
    }
