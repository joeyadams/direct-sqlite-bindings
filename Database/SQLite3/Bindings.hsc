{-# LANGUAGE ForeignFunctionInterface #-}
module Database.SQLite3.Bindings (
    module Database.SQLite3.Bindings.Types,

    -- * Connection management
    c_sqlite3_open,
    c_sqlite3_close,
    c_sqlite3_errmsg,

    -- * Statement management
    c_sqlite3_prepare_v2,
    c_sqlite3_step,
    c_sqlite3_reset,
    c_sqlite3_finalize,
    c_sqlite3_bind_parameter_count,
    c_sqlite3_bind_parameter_name,
    c_sqlite3_column_count,

    -- * Binding Values To Prepared Statements
    -- | <http://www.sqlite.org/c3ref/bind_blob.html>
    c_sqlite3_bind_blob,
    c_sqlite3_bind_double,
    c_sqlite3_bind_int,
    c_sqlite3_bind_int64,
    c_sqlite3_bind_null,
    c_sqlite3_bind_text,

    -- * Result Values From A Query
    -- | <http://www.sqlite.org/c3ref/column_blob.html>
    c_sqlite3_column_type,
    c_sqlite3_column_bytes,
    c_sqlite3_column_blob,
    c_sqlite3_column_int64,
    c_sqlite3_column_double,
    c_sqlite3_column_text,
) where

import Database.SQLite3.Bindings.Types

import Foreign
import Foreign.C


-- | <http://www.sqlite.org/c3ref/open.html>
foreign import ccall "sqlite3_open"
    c_sqlite3_open :: CString -> Ptr (Ptr CDatabase) -> IO CError

-- | <http://www.sqlite.org/c3ref/close.html>
foreign import ccall "sqlite3_close"
    c_sqlite3_close :: Ptr CDatabase -> IO CError

-- | <http://www.sqlite.org/c3ref/errcode.html>
foreign import ccall "sqlite3_errmsg"
    c_sqlite3_errmsg :: Ptr CDatabase -> IO CString

-- | <http://www.sqlite.org/c3ref/prepare.html>
foreign import ccall "sqlite3_prepare_v2"
    c_sqlite3_prepare_v2
        :: Ptr CDatabase        -- ^ Database handle
        -> CString              -- ^ SQL statement, UTF-8 encoded
        -> Int                  -- ^ Maximum length of the SQL statement, in bytes.
        -> Ptr (Ptr CStatement) -- ^ OUT: Statement handle
        -> Ptr CString          -- ^ OUT: Pointer to unused portion of zSql
        -> IO CError

-- | <http://www.sqlite.org/c3ref/step.html>
foreign import ccall "sqlite3_step"
    c_sqlite3_step :: Ptr CStatement -> IO CError

-- | <http://www.sqlite.org/c3ref/reset.html>
foreign import ccall "sqlite3_reset"
    c_sqlite3_reset :: Ptr CStatement -> IO CError

-- | <http://www.sqlite.org/c3ref/finalize.html>
foreign import ccall "sqlite3_finalize"
    c_sqlite3_finalize :: Ptr CStatement -> IO CError

-- | <http://www.sqlite.org/c3ref/bind_parameter_count.html>
foreign import ccall "sqlite3_bind_parameter_count"
    c_sqlite3_bind_parameter_count :: Ptr CStatement -> IO Int

-- | <http://www.sqlite.org/c3ref/bind_parameter_name.html>
foreign import ccall "sqlite3_bind_parameter_name"
    c_sqlite3_bind_parameter_name :: Ptr CStatement -> Int -> IO CString

foreign import ccall "sqlite3_bind_blob"
    c_sqlite3_bind_blob
        :: Ptr CStatement
        -> Int              -- ^ Index of the SQL parameter to be set
        -> Ptr ()           -- ^ Value to bind to the parameter.
                            --   C type: void *ptr
        -> Int              -- ^ Length, in bytes
        -> Ptr CDestructor
        -> IO CError

foreign import ccall "sqlite3_bind_double"
    c_sqlite3_bind_double :: Ptr CStatement -> Int -> Double -> IO CError

foreign import ccall "sqlite3_bind_int"
    c_sqlite3_bind_int :: Ptr CStatement -> Int -> Int -> IO CError

foreign import ccall "sqlite3_bind_int64"
    c_sqlite3_bind_int64 :: Ptr CStatement -> Int -> Int64 -> IO CError

foreign import ccall "sqlite3_bind_null"
    c_sqlite3_bind_null :: Ptr CStatement -> Int -> IO CError

foreign import ccall "sqlite3_bind_text"
    c_sqlite3_bind_text :: Ptr CStatement -> Int -> CString -> Int -> Ptr CDestructor -> IO CError

foreign import ccall "sqlite3_column_type"
    c_sqlite3_column_type :: Ptr CStatement -> Int -> IO CColumnType

foreign import ccall "sqlite3_column_bytes"
    c_sqlite3_column_bytes :: Ptr CStatement -> Int -> IO Int

foreign import ccall "sqlite3_column_blob"
    c_sqlite3_column_blob :: Ptr CStatement -> Int -> IO (Ptr ())

foreign import ccall "sqlite3_column_int64"
    c_sqlite3_column_int64 :: Ptr CStatement -> Int -> IO Int64

foreign import ccall "sqlite3_column_double"
    c_sqlite3_column_double :: Ptr CStatement -> Int -> IO Double

foreign import ccall "sqlite3_column_text"
    c_sqlite3_column_text :: Ptr CStatement -> Int -> IO CString

-- | <http://www.sqlite.org/c3ref/column_count.html>
foreign import ccall "sqlite3_column_count"
    c_sqlite3_column_count :: Ptr CStatement -> IO Int
