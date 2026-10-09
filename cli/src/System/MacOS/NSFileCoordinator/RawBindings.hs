{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module System.MacOS.NSFileCoordinator.RawBindings
  ( module X,
    module System.MacOS.NSFileCoordinator.RawBindings,
  )
where

import Data.ByteString qualified as ByteString
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Language.C.Inline qualified as C
import Language.C.Inline.ObjC qualified as C
import MyPrelude
import System.MacOS.NSFileCoordinator.Types
import System.MacOS.NSFileCoordinator.Types as X hiding (foundationCtx)

C.context (C.objcCtx <> C.funCtx <> foundationCtx)
C.include "<Foundation/Foundation.h>"

m_release :: (Objc a) => Ptr a -> IO ()
m_release (castToId -> obj) = [C.exp| void { [$(id obj) release] } |]

type SingleURLAccessor = Ptr NSURL -> IO ()

type DualURLAccessor = Ptr NSURL -> Ptr NSURL -> IO ()

nsURL_initFileURL :: CString -> CBool -> IO (Ptr NSURL)
nsURL_initFileURL cannonicalPath isDirectory =
  [C.exp| NSURL *{
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

-- | What an error means, in words. For the codes of @NSCocoaErrorDomain@
-- that are about files this follows their documentation in
-- @FoundationErrors.h@, which says more than the error does of itself; the
-- codes are the SDK's constants, so the compiler checks them. Any other
-- error is described as it describes itself.
p_NSError_description :: Ptr NSError -> IO (Ptr NSString)
p_NSError_description nsError =
  [C.block| NSString *{
    NSError *error = $(NSError *nsError);
    if ([error.domain isEqualToString: NSCocoaErrorDomain]) {
      switch (error.code) {
        case NSFileNoSuchFileError:
          return @"a filesystem operation was attempted on a file that does not exist";
        case NSFileLockingError:
          return @"the file could not be locked";
        case NSFileReadUnknownError:
          return @"the file could not be read, for unknown reasons";
        case NSFileReadNoPermissionError:
          return @"the file could not be read because of a permission problem";
        case NSFileReadInvalidFileNameError:
          return @"the file could not be read because its name is invalid";
        case NSFileReadCorruptFileError:
          return @"the file could not be read because it is corrupted, in a bad format, or similar";
        case NSFileReadNoSuchFileError:
          return @"the file could not be read because no such file was found";
        case NSFileReadTooLargeError:
          return @"the file could not be read because it is too large";
        case NSFileWriteUnknownError:
          return @"the file could not be written, for unknown reasons";
        case NSFileWriteNoPermissionError:
          return @"the file could not be written because of a permission problem";
        case NSFileWriteInvalidFileNameError:
          return @"the file could not be written because its name is invalid";
        case NSFileWriteFileExistsError:
          return @"the operation could not be performed because the destination file already exists";
        case NSFileWriteOutOfSpaceError:
          return @"the file could not be written because of a lack of disk space";
        case NSFileWriteVolumeReadOnlyError:
          return @"the file could not be written because the volume is read-only";
        case NSUserCancelledError:
          return @"the operation was cancelled";
        case NSFeatureUnsupportedError:
          return @"the feature is not supported, because the file system lacks it, required libraries are missing, or similar";
      }
    }
    return error.localizedDescription;
  } |]

-- | An error with this domain and code, for the tests
nsError_init :: CString -> NSInteger -> IO (Ptr NSError)
nsError_init domain code =
  [C.exp| NSError *{
    [[NSError alloc]
      initWithDomain: [NSString stringWithUTF8String: $(char *domain)]
      code: $(NSInteger code)
      userInfo: nil
    ]
  }
  |]

-- | Runs the action in an autorelease pool of its own, for the tests: what
-- is autoreleased while it runs is freed when it returns. (The action is
-- called back in the operating system thread that the pool belongs to.)
inAutoreleasePool :: IO () -> IO ()
inAutoreleasePool action =
  [C.block| void { @autoreleasepool { $fun:(void (*action)())(); } } |]

-- | The text of a string, copied out of it: it is good for as long as it is
-- kept, whatever becomes of the string.
--
-- @UTF8String@ gives a buffer that is freed with the string, or with the
-- autorelease pool that it was made in, or when the thread of that pool
-- ends. So its bytes are copied and decoded before this returns: a text
-- that is decoded only when it is first looked at (a path that the body of
-- a coordinated access keeps, the description of an error that is shown
-- later) reads freed memory then.
packNSString :: Ptr NSString -> IO Text
packNSString nsString = do
  cString <- [C.exp| const char *{ [$(NSString *nsString) UTF8String] } |]
  evaluate . decodeUtf8 =<< ByteString.packCString cString

-- | Returns an owned (+1) array; the caller must release it.
nsArray_initWithObjects :: Ptr C.Id -> CUInt -> IO (Ptr NSArray)
nsArray_initWithObjects objects count =
  [C.exp| NSArray *{
    [[NSArray alloc]
      initWithObjects: $(id *objects) count: $(unsigned int count)]
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
  Ptr NSURL ->
  NSFileCoordinatorReadingOptions ->
  Ptr (Ptr NSError) ->
  SingleURLAccessor ->
  IO ()
m_NSFileCoordinator_coordinateReadingItem fileCoordinator url options errorPtr accessor =
  [C.exp| void {
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
  Ptr NSURL ->
  NSFileCoordinatorWritingOptions ->
  Ptr (Ptr NSError) ->
  SingleURLAccessor ->
  IO ()
m_NSFileCoordinator_coordinateWritingItem fileCoordinator url options errorPtr accessor =
  [C.exp| void {
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
  Ptr NSURL ->
  NSFileCoordinatorReadingOptions ->
  Ptr NSURL ->
  NSFileCoordinatorWritingOptions ->
  Ptr (Ptr NSError) ->
  DualURLAccessor ->
  IO ()
m_NSFileCoordinator_coordinateReadingAndWritingItem fileCoordinator readingURL readingOptions writingURL writingOptions errorPtr accessor =
  [C.exp| void {
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
  Ptr NSURL ->
  NSFileCoordinatorWritingOptions ->
  Ptr NSURL ->
  NSFileCoordinatorWritingOptions ->
  Ptr (Ptr NSError) ->
  DualURLAccessor ->
  IO ()
m_NSFileCoordinator_coordinateWritingItems fileCoordinator url1 options1 url2 options2 errorPtr accessor =
  [C.exp| void {
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
  Ptr NSArray -> -- readingURLs
  NSFileCoordinatorReadingOptions ->
  Ptr NSArray -> -- writingURLs
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
