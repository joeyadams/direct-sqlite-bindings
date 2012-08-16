{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
module Database.SQLite3.Bindings (
    -- * Types
    Database(..),
    CDatabase,
    Statement(..),
    CStatement,

    -- ** Enumerations

    -- *** Error
    Error(..),
    CError(..),
    decodeError,

    -- *** ColumnType
    ColumnType(..),
    CColumnType(..),
    decodeColumnType,

    -- ** Miscellaneous
    CDestructor,
    c_SQLITE_TRANSIENT,

    -- * Foreign functions

) where

#include "sqlite3.h"

import Foreign
import Foreign.C

newtype Database  = Database  (Ptr CDatabase)
newtype Statement = Statement (Ptr CStatement)

-- Result code documentation copied from <http://www.sqlite.org/c3ref/c_abort.html>

data Error = ErrorOK                     -- ^ Successful result
           | ErrorError                  -- ^ SQL error or missing database
           | ErrorInternal               -- ^ Internal logic error in SQLite
           | ErrorPermission             -- ^ Access permission denied
           | ErrorAbort                  -- ^ Callback routine requested an abort
           | ErrorBusy                   -- ^ The database file is locked
           | ErrorLocked                 -- ^ A table in the database is locked
           | ErrorNoMemory               -- ^ A @malloc()@ failed
           | ErrorReadOnly               -- ^ Attempt to write a readonly database
           | ErrorInterrupt              -- ^ Operation terminated by @sqlite3_interrupt()@
           | ErrorIO                     -- ^ Some kind of disk I/O error occurred
           | ErrorCorrupt                -- ^ The database disk image is malformed
           | ErrorNotFound               -- ^ Unknown opcode in @sqlite3_file_control()@
           | ErrorFull                   -- ^ Insertion failed because database is full
           | ErrorCan'tOpen              -- ^ Unable to open the database file
           | ErrorProtocol               -- ^ Database lock protocol error
           | ErrorEmpty                  -- ^ Database is empty
           | ErrorSchema                 -- ^ The database schema changed
           | ErrorTooBig                 -- ^ String or BLOB exceeds size limit
           | ErrorConstraint             -- ^ Abort due to constraint violation
           | ErrorMismatch               -- ^ Data type mismatch
           | ErrorMisuse                 -- ^ Library used incorrectly
           | ErrorNoLargeFileSupport     -- ^ Uses OS features not supported on host
           | ErrorAuthorization          -- ^ Authorization denied
           | ErrorFormat                 -- ^ Auxiliary database format error
           | ErrorRange                  -- ^ 2nd parameter to sqlite3_bind out of range
           | ErrorNotADatabase           -- ^ File opened that is not a database file
           | ErrorRow                    -- ^ @sqlite3_step()@ has another row ready
           | ErrorDone                   -- ^ @sqlite3_step()@ has finished executing
             deriving (Eq, Show)

data ColumnType = IntegerColumn
                | FloatColumn
                | TextColumn
                | BlobColumn
                | NullColumn
                  deriving (Eq, Show)

-- | @sqlite3@
data CDatabase

-- | @sqlite3_stmt@
data CStatement

-- | @Ptr CDestructor@ = @sqlite3_destructor_type@.
-- See <http://www.sqlite.org/c3ref/c_static.html>
data CDestructor

c_SQLITE_TRANSIENT :: Ptr CDestructor
c_SQLITE_TRANSIENT = intPtrToPtr (-1)


newtype CError = CError CInt

decodeError :: CError -> Error
decodeError (CError n) = case n of
    #{const SQLITE_OK}         -> ErrorOK
    #{const SQLITE_ERROR}      -> ErrorError
    #{const SQLITE_INTERNAL}   -> ErrorInternal
    #{const SQLITE_PERM}       -> ErrorPermission
    #{const SQLITE_ABORT}      -> ErrorAbort
    #{const SQLITE_BUSY}       -> ErrorBusy
    #{const SQLITE_LOCKED}     -> ErrorLocked
    #{const SQLITE_NOMEM}      -> ErrorNoMemory
    #{const SQLITE_READONLY}   -> ErrorReadOnly
    #{const SQLITE_INTERRUPT}  -> ErrorInterrupt
    #{const SQLITE_IOERR}      -> ErrorIO
    #{const SQLITE_CORRUPT}    -> ErrorCorrupt
    #{const SQLITE_NOTFOUND}   -> ErrorNotFound
    #{const SQLITE_FULL}       -> ErrorFull
    #{const SQLITE_CANTOPEN}   -> ErrorCan'tOpen
    #{const SQLITE_PROTOCOL}   -> ErrorProtocol
    #{const SQLITE_EMPTY}      -> ErrorEmpty
    #{const SQLITE_SCHEMA}     -> ErrorSchema
    #{const SQLITE_TOOBIG}     -> ErrorTooBig
    #{const SQLITE_CONSTRAINT} -> ErrorConstraint
    #{const SQLITE_MISMATCH}   -> ErrorMismatch
    #{const SQLITE_MISUSE}     -> ErrorMisuse
    #{const SQLITE_NOLFS}      -> ErrorNoLargeFileSupport
    #{const SQLITE_AUTH}       -> ErrorAuthorization
    #{const SQLITE_FORMAT}     -> ErrorFormat
    #{const SQLITE_RANGE}      -> ErrorRange
    #{const SQLITE_NOTADB}     -> ErrorNotADatabase
    #{const SQLITE_ROW}        -> ErrorRow
    #{const SQLITE_DONE}       -> ErrorDone
    _                          -> Prelude.error $ "decodeError " ++ show n


newtype CColumnType = CColumnType CInt

decodeColumnType :: CColumnType -> ColumnType
decodeColumnType (CColumnType n) = case n of
    #{const SQLITE_INTEGER} -> IntegerColumn
    #{const SQLITE_FLOAT}   -> FloatColumn
    #{const SQLITE_TEXT}    -> TextColumn
    #{const SQLITE_BLOB}    -> BlobColumn
    #{const SQLITE_NULL}    -> NullColumn
    _                       -> Prelude.error $ "decodeColumnType " ++ show n


foreign import ccall "sqlite3_errmsg"
  errmsgC :: Ptr CDatabase -> IO CString

foreign import ccall "sqlite3_open"
  openC :: CString -> Ptr (Ptr CDatabase) -> IO CError

foreign import ccall "sqlite3_close"
  closeC :: Ptr CDatabase -> IO CError

foreign import ccall "sqlite3_prepare_v2"
  prepareC :: Ptr CDatabase         -- ^ Database handle
           -> CString               -- ^ SQL statement, UTF-8 encoded
           -> Int                   -- ^ Maximum length of zSql in bytes.
           -> Ptr (Ptr CStatement)  -- ^ OUT: Statement handle
           -> Ptr CString           -- ^ OUT: Pointer to unused portion of zSql
           -> IO CError

foreign import ccall "sqlite3_step"
  stepC :: Ptr CStatement -> IO CError

foreign import ccall "sqlite3_reset"
  resetC :: Ptr CStatement -> IO CError

foreign import ccall "sqlite3_finalize"
  finalizeC :: Ptr CStatement -> IO CError

foreign import ccall "sqlite3_bind_parameter_count"
  bindParameterCountC :: Ptr CStatement -> IO Int

foreign import ccall "sqlite3_bind_parameter_name"
  bindParameterNameC :: Ptr CStatement -> Int -> IO CString

foreign import ccall "sqlite3_bind_blob"
  bindBlobC :: Ptr CStatement
            -> Int              -- ^ Index of the SQL parameter to be set
            -> Ptr ()           -- ^ Value to bind to the parameter.
                                --   C type: void *ptr
            -> Int              -- ^ Length, in bytes
            -> Ptr CDestructor
            -> IO CError

foreign import ccall "sqlite3_bind_double"
  bindDoubleC :: Ptr CStatement -> Int -> Double -> IO CError

foreign import ccall "sqlite3_bind_int"
  bindIntC :: Ptr CStatement -> Int -> Int -> IO CError

foreign import ccall "sqlite3_bind_int64"
  bindInt64C :: Ptr CStatement -> Int -> Int64 -> IO CError

foreign import ccall "sqlite3_bind_null"
  bindNullC :: Ptr CStatement -> Int -> IO CError

foreign import ccall "sqlite3_bind_text"
  bindTextC :: Ptr CStatement -> Int -> CString -> Int -> Ptr CDestructor -> IO CError

foreign import ccall "sqlite3_column_type"
  columnTypeC :: Ptr CStatement -> Int -> IO CColumnType

foreign import ccall "sqlite3_column_bytes"
  columnBytesC :: Ptr CStatement -> Int -> IO Int

foreign import ccall "sqlite3_column_blob"
  columnBlobC :: Ptr CStatement -> Int -> IO (Ptr ())

foreign import ccall "sqlite3_column_int64"
  columnInt64C :: Ptr CStatement -> Int -> IO Int64

foreign import ccall "sqlite3_column_double"
  columnDoubleC :: Ptr CStatement -> Int -> IO Double

foreign import ccall "sqlite3_column_text"
  columnTextC :: Ptr CStatement -> Int -> IO CString

foreign import ccall "sqlite3_column_count"
  columnCountC :: Ptr CStatement -> IO Int
