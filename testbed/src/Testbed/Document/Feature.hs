-- | Declare the document command for the testbed Event Model.
-- Compose 'documentFlow' in the application model.
module Testbed.Document.Feature (documentFlow) where

import Service.EventModel
import Testbed.Document.Commands.CreateDocument (CreateDocument)
import Testbed.Document.Core (DocumentEntity, DocumentEvent)


documentFlow :: FeatureAction
documentFlow = feature "Document flow" do
  chapter "Manage documents" do
    slice "Create a document" do
      entity @DocumentEntity do
        command @CreateDocument do
          produces @DocumentEvent
