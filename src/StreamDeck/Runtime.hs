-- | Event dispatch independent of the HID transport.
module StreamDeck.Runtime (runEventLoop) where

import Control.Concurrent (newChan, readChan, writeChan)
import Control.Concurrent.Async (link, withAsync)
import Control.Exception (SomeAsyncException, SomeException, catch, fromException, throwIO)
import Control.Monad (forM_, forever)
import StreamDeck.Protocol (Key, KeyEvent (..))
import System.IO (hPutStrLn, stderr)

-- | Continuously poll input while a separate worker executes queued press actions.
-- Actions run in order; releases and unbound keys are ignored. Synchronous action
-- errors are logged without stopping dispatch. Cancellation stops both threads.
-- The poll operation should wait for input or use a timeout rather than busy-spin.
runEventLoop :: IO [KeyEvent] -> [(Key, IO ())] -> IO ()
runEventLoop poll bindings = do
    actions <- newChan
    withAsync (forever (readChan actions >>= runAction)) $ \worker -> do
        link worker
        forever $ do
            events <- poll
            forM_ events $ \event -> case event of
                Released _ -> pure ()
                Pressed key -> forM_ (lookup key bindings) (writeChan actions)

runAction :: IO () -> IO ()
runAction action = action `catch` handleError
  where
    handleError :: SomeException -> IO ()
    handleError err = case fromException err :: Maybe SomeAsyncException of
        Just _ -> throwIO err
        Nothing -> hPutStrLn stderr ("Stream Deck action failed: " ++ show err)
