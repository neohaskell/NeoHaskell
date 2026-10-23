module Service.CommandResultReceipt.Internal (
  ReceiptExit (..),
  reportIfRegistered,
  setRelatedUserSub,
  withFreshReceipt,
) where

import Basics
import AtomicVar (AtomicVar)
import AtomicVar qualified
import ConcurrentMap qualified
import Control.Exception qualified as GhcException
import Maybe (Maybe (..))
import Prelude qualified as GhcPrelude
import Result qualified
import System.IO.Unsafe qualified as GhcUnsafe
import Task (Task)
import Task qualified
import Text (Text)
import Uuid (Uuid)
import Uuid qualified
import Var (Var)
import Var qualified


data ReceiptExit
  = HandlerFailed
  | HandlerInterrupted
  | HandlerReturnedWithoutResult


data ResultReceipt = ResultReceipt
  { wasReported :: AtomicVar Bool,
    relatedUserSub :: Var (Maybe Text)
  }


receipts :: ConcurrentMap.ConcurrentMap Text ResultReceipt
receipts = GhcUnsafe.unsafePerformIO (Task.runOrPanic (ConcurrentMap.new :: Task Text (ConcurrentMap.ConcurrentMap Text ResultReceipt)))
{-# NOINLINE receipts #-}


reportIfRegistered :: Uuid -> Task Text Unit -> Task Text (Maybe Bool)
reportIfRegistered requestId report = Task.fromIO do
  GhcException.mask_ do
    maybeReceipt <- Task.runOrPanic (ConcurrentMap.get (Uuid.toText requestId) receipts :: Task Text (Maybe ResultReceipt))
    case maybeReceipt of
      Maybe.Nothing -> pure Maybe.Nothing
      Maybe.Just receipt -> do
        wasClaimed <-
          AtomicVar.modifyWithResult receipt.wasReported (\claimed -> (True, claimed))
            |> Task.runNoErrors
        if wasClaimed
          then pure (Maybe.Just False)
          else do
            reportResult <- GhcException.try @GhcException.SomeException (Task.runResult report)
            case reportResult of
              GhcPrelude.Left exception ->
                case GhcException.fromException @GhcException.SomeAsyncException exception of
                  Maybe.Just asyncException -> GhcException.throwIO asyncException
                  Maybe.Nothing -> pure ()
              GhcPrelude.Right _ -> pure ()
            pure (Maybe.Just True)


setRelatedUserSub :: Uuid -> Maybe Text -> Task Text Unit
setRelatedUserSub requestId actor = Task.fromIO do
  GhcException.mask_ do
    maybeReceipt <- Task.runOrPanic (ConcurrentMap.get (Uuid.toText requestId) receipts :: Task Text (Maybe ResultReceipt))
    case maybeReceipt of
      Maybe.Nothing -> pure ()
      Maybe.Just receipt -> Var.set actor receipt.relatedUserSub |> Task.runNoErrors


withFreshReceipt :: (Uuid -> Maybe Text -> ReceiptExit -> Task Text Unit) -> (Uuid -> Task Text value) -> Task Text value
withFreshReceipt reportFallback action = Task.fromIOResult do
  GhcException.mask \restore -> do
    uuidResult <- Task.runResult (Uuid.generate :: Task Text Uuid)
    case uuidResult of
      Result.Err error -> pure (Result.Err error)
      Result.Ok requestId -> do
        wasReported <- AtomicVar.containing False |> Task.runNoErrors
        relatedUserSub <- Var.new Maybe.Nothing |> Task.runNoErrors
        let receipt = ResultReceipt {wasReported, relatedUserSub}
        Task.runOrPanic (ConcurrentMap.set (Uuid.toText requestId) receipt receipts :: Task Text Unit)
        let cleanup = Task.runOrPanic (ConcurrentMap.remove (Uuid.toText requestId) receipts :: Task Text Unit)
            finish exit = do
              actor <- Var.get relatedUserSub |> Task.runNoErrors
              _ <- Task.runResult (reportIfRegistered requestId (reportFallback requestId actor exit))
              pure ()
            runHandler = do
              handlerResult <- GhcException.try @GhcException.SomeException (restore (Task.runResult (action requestId)))
              case handlerResult of
                GhcPrelude.Left exception -> do
                  let exit = case GhcException.fromException @GhcException.SomeAsyncException exception of
                        Maybe.Just _ -> HandlerInterrupted
                        Maybe.Nothing -> HandlerFailed
                  finish exit
                  GhcException.throwIO exception
                GhcPrelude.Right (Result.Err error) -> do
                  finish HandlerFailed
                  pure (Result.Err error)
                GhcPrelude.Right (Result.Ok value) -> do
                  finish HandlerReturnedWithoutResult
                  pure (Result.Ok value)
        GhcException.finally runHandler cleanup
