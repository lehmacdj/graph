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
import Foreign.Marshal.Utils (toBool)
import Foreign.Ptr
import Language.C.Inline qualified as C
import Language.C.Inline.ObjC qualified as C
import MyPrelude
import System.MacOS.NSFileCoordinator.Types
import System.MacOS.NSFileCoordinator.Types as X hiding (foundationCtx)

C.context (C.objcCtx <> C.funCtx <> foundationCtx)
C.include "<Foundation/Foundation.h>"

-- the weak references of the Objective-C runtime, which need no ARC
C.verbatim "extern id objc_initWeak(id *location, id value);"
C.verbatim "extern id objc_loadWeakRetained(id *location);"
C.verbatim "extern void objc_destroyWeak(id *location);"

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

-- | Runs the action in an autorelease pool of its own: what is autoreleased
-- while it runs is freed when it returns. (The action is called back in the
-- operating system thread that the pool belongs to, and must not throw.)
inAutoreleasePool :: IO () -> IO ()
inAutoreleasePool action =
  [C.block| void { @autoreleasepool { $fun:(void (*action)())(); } } |]

-- | A weak reference to an object, for the tests: 'wasFreed' says whether
-- the object is gone.
type Marker = Ptr C.Id

-- | For the tests: autoreleases a new object in the calling thread, and
-- returns a weak reference to it, to be given to 'wasFreed'.
autoreleaseMarker :: IO Marker
autoreleaseMarker =
  [C.block| id *{
    id *marker = calloc(1, sizeof(id));
    objc_initWeak(marker, [[[NSObject alloc] init] autorelease]);
    return marker;
  } |]

-- | For the tests: whether the object of a weak reference has been freed.
-- Gives up the reference: it is asked once.
wasFreed :: Marker -> IO Bool
wasFreed marker =
  toBool
    <$> [C.block| bool {
      id *marker = $(id *marker);
      id object = objc_loadWeakRetained(marker);
      [object release];
      objc_destroyWeak(marker);
      free(marker);
      return object == nil;
    } |]

-- | For the tests: a weak reference to an object.
weakTo :: (Objc a) => Ptr a -> IO Marker
weakTo (castToId -> object) =
  [C.block| id *{
    id *marker = calloc(1, sizeof(id));
    objc_initWeak(marker, $(id object));
    return marker;
  } |]

-- | For the tests: an error (owned, +1) with a description of its own: a
-- string that only the error holds.
nsError_initDescribed :: CString -> NSInteger -> CString -> IO (Ptr NSError)
nsError_initDescribed domain code description =
  [C.block| NSError *{
    @autoreleasepool {
      NSString *described =
        [[NSString alloc] initWithUTF8String: $(char *description)];
      NSDictionary *userInfo =
        [[NSDictionary alloc]
          initWithObjectsAndKeys: described, NSLocalizedDescriptionKey, nil];
      NSError *error =
        [[NSError alloc]
          initWithDomain: [NSString stringWithUTF8String: $(char *domain)]
          code: $(NSInteger code)
          userInfo: userInfo];
      [userInfo release];
      [described release];
      return error;
    }
  } |]

-- | For the tests: a weak reference to the description that an error of
-- 'nsError_initDescribed' was given.
weakToDescription :: Ptr NSError -> IO Marker
weakToDescription nsError =
  [C.block| id *{
    id *marker = calloc(1, sizeof(id));
    @autoreleasepool {
      objc_initWeak(
        marker,
        [[$(NSError *nsError) userInfo] objectForKey: NSLocalizedDescriptionKey]);
    }
    return marker;
  } |]

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

-- | - (void)cancel;
--
-- For the tests: every coordinated call of the coordinator after this one
-- fails with @NSUserCancelledError@, without calling its accessor.
m_NSFileCoordinator_cancel :: Ptr NSFileCoordinator -> IO ()
m_NSFileCoordinator_cancel fileCoordinator =
  [C.exp| void { [$(NSFileCoordinator *fileCoordinator) cancel] } |]

-- * Coordinated calls

-- Each of these runs in an autorelease pool of its own, and returns the
-- error of the call, if there is one, as an owned (+1) object that the
-- caller must release (null if there is none).
--
-- The pool: file coordination autoreleases objects on every call (two
-- buffers of 1280 bytes for the path of the file among them; about 2.5 KB
-- for each file read and 3.3 KB for each one written). They are freed when the
-- innermost pool of their thread is left, and a thread that has none keeps
-- them until it ends. The threads of a Haskell program have none, and the
-- one that runs the REPL never ends: without a pool here the memory of a
-- session grows with every file that it reads. A pool can't be kept open
-- from Haskell, as a pool must be left by the operating system thread that
-- entered it, and a Haskell thread is not tied to one from one foreign call
-- to the next. One foreign call is, from its start to its end, so the pool
-- is entered and left inside the call: the accessor runs inside it, in the
-- same operating system thread, as does a coordinated call that the accessor
-- makes itself (in a pool of its own, inside this one).
--
-- What is made inside the pool is gone when the call returns, so nothing
-- of it is handed out: the accessor reads the path out of its URL before it
-- returns ('packNSString' copies it), and the error, which is autoreleased
-- as well, is retained inside the pool and belongs to the caller. It is a
-- variable of the call and not a place that the caller provides: several
-- calls can run at once (the readers and writers of a batch do), and each
-- has its own error.

-- | - (void)coordinateReadingItemAtURL:(NSURL *)url
--        options:(NSFileCoordinatorReadingOptions)options
--        error:(NSError **)outError
--        byAccessor:(void (NS_NOESCAPE ^)(NSURL *newURL))reader;
m_NSFileCoordinator_coordinateReadingItem ::
  Ptr NSFileCoordinator ->
  Ptr NSURL ->
  NSFileCoordinatorReadingOptions ->
  SingleURLAccessor ->
  IO (Ptr NSError)
m_NSFileCoordinator_coordinateReadingItem fileCoordinator url options accessor =
  [C.block| NSError *{
    NSError *error = nil;
    @autoreleasepool {
      [$(NSFileCoordinator *fileCoordinator)
        coordinateReadingItemAtURL: $(NSURL *url)
        options: $(NSFileCoordinatorReadingOptions options)
        error: &error
        byAccessor: ^(NSURL *url) { $fun:(void (*accessor)(NSURL *))(url); }
      ];
      [error retain];
    }
    return error;
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
  SingleURLAccessor ->
  IO (Ptr NSError)
m_NSFileCoordinator_coordinateWritingItem fileCoordinator url options accessor =
  [C.block| NSError *{
    NSError *error = nil;
    @autoreleasepool {
      [$(NSFileCoordinator *fileCoordinator)
        coordinateWritingItemAtURL: $(NSURL *url)
        options: $(NSFileCoordinatorWritingOptions options)
        error: &error
        byAccessor: ^(NSURL *url) { $fun:(void (*accessor)(NSURL *))(url); }
      ];
      [error retain];
    }
    return error;
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
  DualURLAccessor ->
  IO (Ptr NSError)
m_NSFileCoordinator_coordinateReadingAndWritingItem fileCoordinator readingURL readingOptions writingURL writingOptions accessor =
  [C.block| NSError *{
    NSError *error = nil;
    @autoreleasepool {
      [$(NSFileCoordinator *fileCoordinator)
        coordinateReadingItemAtURL: $(NSURL *readingURL)
        options: $(NSFileCoordinatorReadingOptions readingOptions)
        writingItemAtURL: $(NSURL *writingURL)
        options: $(NSFileCoordinatorWritingOptions writingOptions)
        error: &error
        byAccessor: ^(NSURL *newReadingURL, NSURL *newWritingURL) {
          $fun:(void (*accessor)(NSURL *, NSURL *))(newReadingURL, newWritingURL);
        }
      ];
      [error retain];
    }
    return error;
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
  IO () ->
  IO (Ptr NSError)
m_NSFileCoordinator_prepareForReadingAndWritingItems fileCoordinator readingURLs readingOptions writingURLs writingOptions accessor =
  [C.block| NSError *{
    NSError *error = nil;
    @autoreleasepool {
      [$(NSFileCoordinator *fileCoordinator)
        prepareForReadingItemsAtURLs: $(NSArray *readingURLs)
        options: $(NSFileCoordinatorReadingOptions readingOptions)
        writingItemsAtURLs: $(NSArray *writingURLs)
        options: $(NSFileCoordinatorWritingOptions writingOptions)
        error: &error
        byAccessor: ^(void (^completionHandler)(void)) {
          $fun:(void (*accessor)())();
          completionHandler();
        }
      ];
      [error retain];
    }
    return error;
  }
  |]
