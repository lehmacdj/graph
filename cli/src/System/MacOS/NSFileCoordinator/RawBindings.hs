{-# LANGUAGE CPP #-}

#ifdef darwin_HOST_OS
{-# LANGUAGE TemplateHaskell #-}
#endif

module System.MacOS.NSFileCoordinator.RawBindings
  ( module X,
    module System.MacOS.NSFileCoordinator.RawBindings,
  )
where

import qualified Data.ByteString.Unsafe as ByteString
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.ObjC as C
import MyPrelude
import System.MacOS.NSFileCoordinator.Types
import System.MacOS.NSFileCoordinator.Types as X hiding (foundationCtx)

#ifdef darwin_HOST_OS

C.context (C.objcCtx <> C.funCtx <> foundationCtx)
C.include "<Foundation/Foundation.h>"

m_release :: Objc a => Ptr a -> IO ()
m_release (castToId -> obj) = [C.exp| void { [$(id obj) release] } |]

type SingleURLAccessor = Ptr NSURL -> IO ()
type DualURLAccessor = Ptr NSURL -> Ptr NSURL -> IO ()

nsURL_initFileURL :: CString -> CBool -> IO (Ptr NSURL)
nsURL_initFileURL cannonicalPath isDirectory = [C.exp| NSURL *{
    [[NSURL alloc]
      initFileURLWithFileSystemRepresentation: $(char *cannonicalPath)
      isDirectory: $(bool isDirectory)
      relativeToURL: nil
    ]
  }
  |]

p_NSURL_path :: Ptr NSURL -> IO (Ptr NSString)
p_NSURL_path nsURL = [C.exp| NSString *{ [$(NSURL *nsURL) path] } |]

p_NSError_domain :: Ptr NSError -> IO (Ptr NSErrorDomain)
p_NSError_domain nsError = [C.exp| NSString *{ $(NSError *nsError).domain } |]

p_NSError_code :: Ptr NSError -> IO NSInteger
p_NSError_code nsError = [C.exp| NSInteger { $(NSError *nsError).code } |]

packNSString :: Ptr NSString -> IO Text
packNSString nsString = do
  cString <- [C.exp| const char *{ [$(NSString *nsString) UTF8String] } |]
  decodeUtf8 <$> ByteString.unsafePackCString cString

nsArray_initWithObjects :: Ptr C.Id -> CUInt -> IO (Ptr NSArray)
nsArray_initWithObjects objects count = [C.exp| NSArray *{
    [NSArray arrayWithObjects: $(id *objects) count: $(unsigned int count)]
  }
  |]

-- | - (instancetype)initWithFilePresenter:(nullable id<NSFilePresenter>)filePresenterOrNil NS_DESIGNATED_INITIALIZER;
nsFileCoordinator_init :: IO (Ptr NSFileCoordinator)
nsFileCoordinator_init = [C.exp| NSFileCoordinator *{ [[NSFileCoordinator alloc] initWithFilePresenter:nil] } |]

-- | - (void)coordinateReadingItemAtURL:(NSURL *)url
--        options:(NSFileCoordinatorReadingOptions)options
--        error:(NSError **)outError
--        byAccessor:(void (NS_NOESCAPE ^)(NSURL *newURL))reader;
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

-- | - (void)coordinateWritingItemAtURL:(NSURL *)url
--        options:(NSFileCoordinatorWritingOptions)options
--        error:(NSError **)outError
--        byAccessor:(void (NS_NOESCAPE ^)(NSURL *newURL))writer;
m_NSFileCoordinator_coordinateWritingItem ::
  Ptr NSFileCoordinator ->
  Ptr NSURL -> NSFileCoordinatorWritingOptions -> Ptr (Ptr NSError) -> SingleURLAccessor -> IO ()
m_NSFileCoordinator_coordinateWritingItem fileCoordinator url options errorPtr accessor = [C.exp| void {
    [$(NSFileCoordinator *fileCoordinator)
      coordinateWritingItemAtURL: $(NSURL *url)
      options: $(NSFileCoordinatorWritingOptions options)
      error: $(NSError **errorPtr)
      byAccessor: ^(NSURL *url) { $fun:(void (*accessor)(NSURL *))(url); }
    ]
  }
  |]

-- | - (void)coordinateReadingItemAtURL:(NSURL *)readingURL
--        options:(NSFileCoordinatorReadingOptions)readingOptions
--        writingItemAtURL:(NSURL *)writingURL
--        options:(NSFileCoordinatorWritingOptions)writingOptions
--        error:(NSError **)outError
--        byAccessor:(void (NS_NOESCAPE ^)(NSURL *newReadingURL, NSURL *newWritingURL))readerWriter;
m_NSFileCoordinator_coordinateReadingAndWritingItem ::
  Ptr NSFileCoordinator ->
  Ptr NSURL -> NSFileCoordinatorReadingOptions ->
  Ptr NSURL -> NSFileCoordinatorWritingOptions ->
  Ptr (Ptr NSError) -> DualURLAccessor -> IO ()
m_NSFileCoordinator_coordinateReadingAndWritingItem fileCoordinator readingURL readingOptions writingURL writingOptions errorPtr accessor = [C.exp| void {
    [$(NSFileCoordinator *fileCoordinator)
      coordinateReadingItemAtURL: $(NSURL *readingURL)
      options: $(NSFileCoordinatorReadingOptions readingOptions)
      writingItemAtURL: $(NSURL *writingURL)
      options: $(NSFileCoordinatorWritingOptions writingOptions)
      error: $(NSError **errorPtr)
      byAccessor: ^(NSURL *newReadingURL, NSURL *newWritingURL) {
        $fun:(void (*accessor)(NSURL *, NSURL *))(newReadingURL, newWritingURL);
      }
    ]
  }
  |]

-- | - (void)coordinateWritingItemAtURL:(NSURL *)url1
--        options:(NSFileCoordinatorWritingOptions)options1
--        writingItemAtURL:(NSURL *)url2
--        options:(NSFileCoordinatorWritingOptions)options2
--        error:(NSError **)outError
--        byAccessor:(void (NS_NOESCAPE ^)(NSURL *newURL1, NSURL *newURL2))writer;
m_NSFileCoordinator_coordinateWritingItems ::
  Ptr NSFileCoordinator ->
  Ptr NSURL -> NSFileCoordinatorWritingOptions ->
  Ptr NSURL -> NSFileCoordinatorWritingOptions ->
  Ptr (Ptr NSError) -> DualURLAccessor -> IO ()
m_NSFileCoordinator_coordinateWritingItems fileCoordinator url1 options1 url2 options2 errorPtr accessor = [C.exp| void {
    [$(NSFileCoordinator *fileCoordinator)
      coordinateWritingItemAtURL: $(NSURL *url1)
      options: $(NSFileCoordinatorWritingOptions options1)
      writingItemAtURL: $(NSURL *url2)
      options: $(NSFileCoordinatorWritingOptions options2)
      error: $(NSError **errorPtr)
      byAccessor: ^(NSURL *newURL1, NSURL *newURL2) {
        $fun:(void (*accessor)(NSURL *, NSURL *))(newURL1, newURL2);
      }
    ]
  }
  |]

-- | - (void)prepareForReadingItemsAtURLs:(NSArray<NSURL *> *)readingURLs
--        options:(NSFileCoordinatorReadingOptions)readingOptions
--        writingItemsAtURLs:(NSArray<NSURL *> *)writingURLs
--        options:(NSFileCoordinatorWritingOptions)writingOptions
--        error:(NSError **)outError
--        byAccessor:(void (^)(void (^completionHandler)(void)))batchAccessor;
m_NSFileCoordinator_prepareForReadingAndWritingItems ::
  Ptr NSFileCoordinator ->
  Ptr NSArray ->  -- readingURLs
  NSFileCoordinatorReadingOptions ->
  Ptr NSArray ->  -- writingURLs
  NSFileCoordinatorWritingOptions ->
  Ptr (Ptr NSError) ->
  IO () ->
  IO ()
m_NSFileCoordinator_prepareForReadingAndWritingItems fileCoordinator readingURLs readingOptions writingURLs writingOptions errorPtr accessor =
  [C.block| void {
    [$(NSFileCoordinator *fileCoordinator)
      prepareForReadingItemsAtURLs: $(NSArray *readingURLs)
      options: $(NSFileCoordinatorReadingOptions readingOptions)
      writingItemsAtURLs: $(NSArray *writingURLs)
      options: $(NSFileCoordinatorWritingOptions writingOptions)
      error: $(NSError **errorPtr)
      byAccessor: ^(void (^completionHandler)(void)) {
        $fun:(void (*accessor)())();
        completionHandler();
      }
    ];
  }
  |]

#endif
