{-# LANGUAGE CPP #-}

#ifdef darwin_HOST_OS
{-# LANGUAGE TemplateHaskell #-}
#endif

module System.MacOS.NSFileCoordinator.RawBindings
  ( module X,
    module System.MacOS.NSFileCoordinator,
  )
where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Language.C.Inline.ObjC as C
import MyPrelude
import System.MacOS.NSFileCoordinator.Types
import System.MacOS.NSFileCoordinator.Types as X hiding (foundationCtx)

#ifdef darwin_HOST_OS

C.context (C.objcCtx <> C.funCtx <> foundationCtx)
C.include "<Foundation/Foundation.h>"

type SingleURLAccessor = Ptr NSURL -> IO ()
type DualURLAccessor = Ptr NSURL -> Ptr NSURL -> IO ()
type CleaningUpAccessor = FunPtr (() -> IO ()) -> IO ()

nsURL_initFileURL :: CString -> CBool -> IO (Ptr NSURL)
nsURL_initFileURL cannonicalPath isDirectory = [C.exp| NSURL *{
    [[NSURL alloc]
      initFileURLWithFileSystemRepresentation: $(char *cannonicalPath)
      isDirectory: $(bool isDirectory)
      relativeToURL: nil
    ]
  }
  |]

-- | - (instancetype)initWithFilePresenter:(nullable id<NSFilePresenter>)filePresenterOrNil NS_DESIGNATED_INITIALIZER;
nsFileCoordinator_init :: IO (Ptr NSFileCoordinator)
nsFileCoordinator_init = [C.exp| NSFileCoordinator *{ [[NSFileCoordinator alloc] initWithFilePresenter:nil] } |]

-- | - (void)coordinateReadingItemAtURL:(NSURL *)url options:(NSFileCoordinatorReadingOptions)options error:(NSError **)outError byAccessor:(void (NS_NOESCAPE ^)(NSURL *newURL))reader;
m_NSFileCoordinator_coordinateReadingItem ::
  Ptr NSFileCoordinator ->
  Ptr NSURL -> NSFileCoordinatorReadingOptions -> Ptr (Ptr NSError) -> SingleURLAccessor -> IO ()
m_NSFileCoordinator_coordinateReadingItem fileCoordinator url options errorPtr accessor = [C.exp| void {
    [$(NSFileCoordinator *fileCoordinator)
      coordinateReadingItemAtURL: $(NSURL *url)
      options: $(NSFileCoordinatorReadingOptions options)
      error: $(NSError **errorPtr)
      byAccessor: ^(NSURL *url) { $fun:(void (*accessor)(NSURL *))(url); }
    ]
  }
  |]

-- | - (void)coordinateWritingItemAtURL:(NSURL *)url options:(NSFileCoordinatorWritingOptions)options error:(NSError **)outError byAccessor:(void (NS_NOESCAPE ^)(NSURL *newURL))writer;

-- | - (void)coordinateReadingItemAtURL:(NSURL *)readingURL options:(NSFileCoordinatorReadingOptions)readingOptions writingItemAtURL:(NSURL *)writingURL options:(NSFileCoordinatorWritingOptions)writingOptions error:(NSError **)outError byAccessor:(void (NS_NOESCAPE ^)(NSURL *newReadingURL, NSURL *newWritingURL))readerWriter;

-- | - (void)coordinateWritingItemAtURL:(NSURL *)url1 options:(NSFileCoordinatorWritingOptions)options1 writingItemAtURL:(NSURL *)url2 options:(NSFileCoordinatorWritingOptions)options2 error:(NSError **)outError byAccessor:(void (NS_NOESCAPE ^)(NSURL *newURL1, NSURL *newURL2))writer;

#endif
