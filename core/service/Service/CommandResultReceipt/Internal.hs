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
import IO qualified
import Maybe (Maybe (..))
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
receipts = IO.dangerouslyRun (Task.runOrPanic (ConcurrentMap.new :: Task Text (ConcurrentMap.ConcurrentMap Text ResultReceipt)))
{-# NOINLINE receipts #-}


reportIfRegistered :: Uuid -> Task Text Unit -> Task Text (Maybe Bool)
reportIfRegistered requestId report = Task.mask \_ -> do
  maybeReceipt <- ConcurrentMap.get (Uuid.toText requestId) receipts
  case maybeReceipt of
    Maybe.Nothing -> Task.yield Maybe.Nothing
    Maybe.Just receipt -> do
      wasClaimed <-
        AtomicVar.modifyWithResult receipt.wasReported (\claimed -> (True, claimed))
      if wasClaimed
        then Task.yield (Maybe.Just False)
        else do
          let runReport = Task.evaluate report |> Task.andThen identity
          _ <- runReport |> Task.asResultSafe
          Task.yield (Maybe.Just True)


setRelatedUserSub :: Uuid -> Maybe Text -> Task Text Unit
setRelatedUserSub requestId actor = Task.mask \_ -> do
  maybeReceipt <- ConcurrentMap.get (Uuid.toText requestId) receipts
  case maybeReceipt of
    Maybe.Nothing -> Task.yield unit
    Maybe.Just receipt -> Var.set actor receipt.relatedUserSub


withFreshReceipt :: (Uuid -> Maybe Text -> ReceiptExit -> Task Text Unit) -> (Uuid -> Task Text value) -> Task Text value
withFreshReceipt reportFallback action = Task.mask \restore -> do
  requestId <- Uuid.generate
  wasReported <- AtomicVar.containing False
  relatedUserSub <- Var.new Maybe.Nothing
  let receipt = ResultReceipt {wasReported, relatedUserSub}
  let cleanup = ConcurrentMap.remove (Uuid.toText requestId) receipts
  let finish exit = do
        actor <- Var.get relatedUserSub
        let report = Task.evaluate (reportFallback requestId actor exit) |> Task.andThen identity
        _ <- reportIfRegistered requestId report
        Task.yield unit
  let reportFailure failure = case failure of
        Task.Interrupted -> finish HandlerInterrupted
        Task.TypedFailure _ -> finish HandlerFailed
        Task.RuntimeFailure -> finish HandlerFailed
  let runHandler = do
        callback <- Task.evaluate (action requestId)
        restore callback
  Task.finally cleanup do
    ConcurrentMap.set (Uuid.toText requestId) receipt receipts
    result <- Task.onFailure reportFailure runHandler
    finish HandlerReturnedWithoutResult
    Task.yield result
