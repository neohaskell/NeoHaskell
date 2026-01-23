module Service.FileUpload.FileStateStore.InMemorySpec (spec) where

import Core
import Service.FileUpload.Web qualified as FileUpload
import Task qualified
import Test
import Test.Service.FileUpload.FileStateStore qualified as FileStateStoreSpec


spec :: Spec Unit
spec = do
  describe "InMemoryFileStateStore" do
    let newStore = do
          stateMap <- FileUpload.newInMemoryFileStateStore
          Task.yield (FileUpload.inMemoryFileStateStore stateMap)
    FileStateStoreSpec.spec newStore
